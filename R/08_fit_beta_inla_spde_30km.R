# ============================================================
# 07_fit_test_30km_mesh.R
#
# TEST INLA-SPDE MODEL USING 30-KM MESH
#
# Response:
#   mesic_beta
#
# Fixed effects:
#   pdsi
#   pr
#   ro
#   soil
#   vpd
#   elevation
#   slope
#   aspect
#   distanceToRoads
#   fire_occurrence
#   n_fires
#   county_population
#   land_tenure
#
# Random effects:
#   RW1(year)
#   RW1(month)
#   IID(ecoregion)
#   SPDE spatial field
#
# Temperature min/max REMOVED
# ============================================================
library(data.table)
library(INLA)
source("config/config.R")



data_file <- file.path(

  processed_dir,

  "Mesic_dataset_250k.csv"

)



model_output_dir <- file.path(

  model_dir,

  "test_30km_model_250k"

)



dir.create(

  model_output_dir,

  recursive = TRUE,

  showWarnings = FALSE

)

# ============================================================
# 1. PATHS
# ============================================================
# ============================================================
# 2. READ DATA
# ============================================================
cat("\n=========================================\n")
cat("READING DATA\n")
cat("=========================================\n")
dt <- fread(data_file)
setDT(dt)
# Small test dataset
#set.seed(123)
#dt <- dt[sample(   .N,   size = min(250000, .N))]


cat("Rows:", nrow(dt), "\n")
cat("Unique locations:", uniqueN(dt$location_id), "\n")
cat("Years:", uniqueN(dt$year), "\n")
cat("Months:", uniqueN(dt$month), "\n")
# ============================================================
# 3. READ 30-KM MESH
# ============================================================
# ============================================================
# 4. CHECK REQUIRED VARIABLES
# ============================================================
required_variables <- c(
  "mesic_beta",
  "x",
  "y",
  "year",
  "month",
  "SPI6",
  "pdsi",
  "pr",
  "ro",
  "soil",
  "vpd",
  "elevation",
  "slope",
  "aspect",
  "distanceToRoads",
  "distanceToWater",
  "fire_occurrence",
  "n_fires",
  "county_population",
  "land_tenure",
  "ecoImg"
)

missing_variables <- setdiff(
  required_variables,
  names(dt)
)

if (length(missing_variables) > 0) {
  stop(
    paste(
      "Missing variables:",
      paste(
        missing_variables,
        collapse = ", "
      )

    )

  )
}

# ============================================================
# 5. LAND TENURE
# ============================================================
# Blank strings -> NA
dt[
  trimws(land_tenure) == "",
  land_tenure := NA_character_
]

# Factor
dt[
  ,
  land_tenure := factor(land_tenure)
]

# Federal as reference
dt[
  ,
  land_tenure := relevel(
    land_tenure,
    ref = "Federal"
  )
]

cat("\nCounts:\n")
print(
  table(
    dt$land_tenure,
    useNA = "ifany"
  )
)

cat("\nLevels:\n")
print(
  levels(dt$land_tenure)
)

# ============================================================
# 6. ECOREGION
# ============================================================
cat("\n=========================================\n")
cat("ECOREGION\n")
cat("=========================================\n")
dt[,
  eco_factor := factor(ecoImg)
]

dt[,
  eco_id := as.integer(eco_factor)
]
cat(  "Number of ecoregions:",
  nlevels(dt$eco_factor),
  "\n"
)

# ============================================================
# 7. FIRE OCCURRENCE
# ============================================================
dt[
  ,
  fire_occurrence := as.integer(fire_occurrence)
]
# ============================================================
# 8. TEMPORAL INDICES
# ============================================================
cat("\n=========================================\n")
cat("TEMPORAL INDICES\n")
cat("=========================================\n")
year_levels <- c(
  1985,
  1990,
  1995,
  2000,
  2005,
  2010,
  2015,
  2020,
  2025
)
month_levels <- c(
  6L,
  7L,
  8L,
  9L
)

dt[
  ,
  year_rw := match(year, year_levels)
]

dt[
  ,
  month_rw := match(month, month_levels)
]

cat("\nYear RW index:\n")

print(
  unique(
    dt[
      order(year),
      .(
        year,
        year_rw
      )
    ]
  )
)
cat("\nMonth RW index:\n")
print(
  unique(
    dt[
      order(month),
      .(
        month,
        month_rw
      )
    ]
  )
)
# ============================================================
# 9. COMPLETE CASE FILTER
# ============================================================
cat("\n=========================================\n")
cat("COMPLETE CASE FILTER\n")
cat("=========================================\n")
model_variables <- c(
  "mesic_beta",
  "x",
  "y",
  "year_rw",
  "month_rw",
  "SPI6",
  "pdsi",
  "pr",
  "ro",
  "soil",
  "vpd",
  "elevation",
  "slope",
  "aspect",
  "distanceToRoads",
  "distanceToWater",
  "fire_occurrence",
  "n_fires",
  "county_population",
  "land_tenure",
  "eco_id"
)
before <- nrow(dt)
dt <- dt[
  complete.cases(
    dt[, ..model_variables]
  )
]
after <- nrow(dt)
cat("Before:", before, "\n")
cat("After:", after, "\n")
cat("Removed:", before - after, "\n")
# ============================================================
# 10. LAND TENURE DESIGN MATRIX
# ============================================================
cat("\n=========================================\n")
cat("LAND TENURE DESIGN MATRIX\n")
cat("=========================================\n")
land_matrix <- model.matrix(
  ~ land_tenure,
  data = dt
)
land_matrix <- land_matrix[
  ,
  colnames(land_matrix) != "(Intercept)",
  drop = FALSE
]
colnames(
  land_matrix
) <- make.names(
  colnames(land_matrix)
)
land_terms <- colnames(
  land_matrix
)
cat("Reference category:",
  levels(dt$land_tenure)[1],
  "\n"
)
cat("\nEstimated land-tenure coefficients:\n")
print(
  land_terms
)


# ============================================================
# CREATE 30-KM MESH EXPLICITLY
# ============================================================
coords_mesh <- unique(
  dt[, .(x, y)]
)

coords_mesh <- as.matrix(coords_mesh)

cat("\n=========================================\n")
cat("CREATING 30-KM MESH\n")
cat("=========================================\n")

# Create non-convex boundary
boundary <- inla.nonconvex.hull(
  coords_mesh,
  convex = 30000,
  concave = 60000,
  resolution = c(100, 100)
)

# Create mesh
mesh <- inla.mesh.2d(
  loc = coords_mesh,
  boundary = boundary,
  max.edge = c(
    30000,   # ~30 km inside study area
    60000    # coarser outside
  ),
  offset = c(
    30000,
    90000
  ),
  cutoff = 5000
)

cat("Mesh nodes:",  mesh$n,  "\n")
# ============================================================
# 11. CREATE SPDE
# ============================================================
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 2,
  prior.range = c(
    200000,
    0.05
  ),
  prior.sigma = c(
    1,
    0.05
  )
)
cat(  "SPDE nodes:",  spde$n.spde,"\n")
# ============================================================
# 12. PROJECTOR MATRIX
# ============================================================
cat("\n=========================================\n")
cat("BUILDING A MATRIX\n")
cat("=========================================\n")
coords <- as.matrix(
  dt[,    .(x,y)])

A_spatial <- inla.spde.make.A(
  mesh = mesh,
  loc = coords
)
cat(
  "A dimensions:",
  nrow(A_spatial),
  "x",
  ncol(A_spatial),
  "\n"
)
# ============================================================
# 13. SPATIAL INDEX
# ============================================================
spatial_index <- inla.spde.make.index(
  name = "spatial",
  n.spde = spde$n.spde
)
# ============================================================
# 14. FIXED EFFECTS
# ============================================================
fixed_effects <- data.frame(
  intercept = 1,
  SPI6 = dt$SPI6,
  pdsi = dt$pdsi,
  pr = dt$pr,
  ro = dt$ro,
  soil = dt$soil,
  vpd = dt$vpd,
  elevation = dt$elevation,
  slope = dt$slope,
  aspect = dt$aspect,
  distanceToRoads = dt$distanceToRoads,
  distanceToWater = dt$distanceToWater,
  fire_occurrence = dt$fire_occurrence,
  n_fires = dt$n_fires,
  county_population = dt$county_population,
  year_rw = dt$year_rw,
  month_rw = dt$month_rw,
  eco_id = dt$eco_id
)
fixed_effects <- cbind(
  fixed_effects,
  land_matrix
)
# ============================================================
# 15. BUILD INLA STACK
# ============================================================
cat("\n=========================================\n")
cat("BUILDING INLA STACK\n")
cat("=========================================\n")
stack_est <- inla.stack(
  data = list(
    mesic_beta = dt$mesic_beta
  ),
  A = list(
    1,
    A_spatial
  ),
  effects = list(
    fixed_effects,
    spatial =
      spatial_index$spatial
  ),
  tag = "est"
)
cat("INLA stack created.\n")

# ============================================================
# 16. MODEL FORMULA
# ============================================================
fixed_terms <- c(
  "intercept",
  "SPI6",
  "pdsi",
  "pr",
  "ro",
  "soil",
  "vpd",
  "elevation",
  "slope",
  "aspect",
  "distanceToRoads",
  "distanceToWater",
  "fire_occurrence",
  "n_fires",
  "county_population",
  land_terms
)

formula_text <- paste0(
  "mesic_beta ~ -1 + ",
  paste(
    fixed_terms,
    collapse = " + "
  ),

  " + f(year_rw, model = 'rw1', constr = TRUE)",
  " + f(month_rw, model = 'rw1', constr = TRUE)",
  " + f(eco_id, model = 'iid')",
  " + f(spatial, model = spde)"
)
formula <- as.formula(
  formula_text
)

cat("\n=========================================\n")
cat("MODEL FORMULA\n")
cat("=========================================\n")
print(  formula)
# ============================================================
# 17. FIT MODEL
# ============================================================
cat("\n=========================================\n")
cat("FITTING 30-KM MESH MODEL\n")
cat("=========================================\n")
start_time <- Sys.time()
result <- inla(
  formula,
  family = "beta",
  data = inla.stack.data(
    stack_est
  ),
  control.predictor = list(
    A = inla.stack.A(
      stack_est
    ),
    compute = TRUE
  ),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    config = FALSE
  ),

  #control.inla = list(strategy = "adaptive", #   int.strategy = "eb"),

  num.threads = "14:1",
  verbose = TRUE
)
end_time <- Sys.time()

# ============================================================
# 18. RUNTIME
# ============================================================
runtime_seconds <- as.numeric(
  difftime(
    end_time,
    start_time,
    units = "secs"
  )
)
cat("\n=========================================\n")
cat("MODEL COMPLETE\n")
cat("=========================================\n")
cat("Runtime seconds:",   runtime_seconds,   "\n")
cat( "Runtime minutes:",  runtime_seconds / 60,  "\n" )
# ============================================================
# 19. MODEL METRICS
# ============================================================
cat(
  "\nDIC:",
  result$dic$dic,
  "\n"
)
cat(
  "WAIC:",
  result$waic$waic,
  "\n"
)
# ============================================================
# 20. FIXED EFFECTS
# ============================================================
cat("\n=========================================\n")
cat("FIXED EFFECTS\n")
cat("=========================================\n")

print(result$summary.fixed)
# ============================================================
# 21. HYPERPARAMETERS
# ============================================================
cat("\n=========================================\n")
cat("HYPERPARAMETERS\n")
cat("=========================================\n")
print(result$summary.hyperpar)
# ============================================================
# 22. SAVE MODEL
# ============================================================
model_file <- file.path(
  output_dir,
  "test_30km_beta_spde_year_month_rw1.rds"
)
saveRDS(result,  model_file)
cat(  "\nModel saved:\n",  model_file,  "\n")
# ============================================================
# 23. SAVE FIXED EFFECTS
# ============================================================
fixed_table <- as.data.table(
  result$summary.fixed,
  keep.rownames = "predictor"
)
fwrite(  fixed_table,  file.path(    output_dir,    "fixed_effects.csv" )
)
# ============================================================
# 24. SAVE HYPERPARAMETERS
# ============================================================
hyper_table <- as.data.table(
  result$summary.hyperpar,
  keep.rownames = "parameter"
)
fwrite(
  hyper_table,
  file.path(
    output_dir,
    "hyperparameters.csv"
  )
)
# ============================================================
# 25. SAVE MODEL METRICS
# ============================================================
spatial_range_row <- grep(
  "Range for spatial",
  rownames(
    result$summary.hyperpar
  )
)

spatial_sd_row <- grep(
  "Stdev for spatial",
  rownames(
    result$summary.hyperpar
  )
)

metrics <- data.table(
  model =
    "test_30km_beta_spde_year_month_rw1",
  observations =
    nrow(dt),
  mesh_nodes =
    mesh$n,
  years =
    uniqueN(dt$year_rw),
  months =
    uniqueN(dt$month_rw),
  ecoregions =
    uniqueN(dt$eco_id),
  land_tenure_levels =
    nlevels(dt$land_tenure),
  dic =
    result$dic$dic,
  waic =
    result$waic$waic,
  spatial_range_mean =
    result$summary.hyperpar[
      spatial_range_row,
      "mean"
    ],

  spatial_sd_mean =
    result$summary.hyperpar[
      spatial_sd_row,
      "mean"
    ],

  runtime_seconds =
    runtime_seconds,
  runtime_minutes =
    runtime_seconds / 60
)
fwrite(  metrics,  file.path(    output_dir,    "model_metrics.csv"  )
)
# ============================================================
# 26. DONE
# ============================================================
cat("\n=========================================\n")
cat("30-KM MODEL DONE\n")
cat("=========================================\n")
