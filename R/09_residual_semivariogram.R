# ============================================================
# 08_residual_semivariogram_30km_model.R
#
# RESIDUAL SPATIAL SEMIVARIOGRAM
# FOR 30-KM INLA-SPDE TEST MODEL
#
# This script:
#   1. Reads the original long dataset
#   2. Reproduces the same 10,000-row sample used for the model
#   3. Reproduces the complete-case filtering
#   4. Loads the fitted INLA model
#   5. Calculates residuals
#   6. Averages residuals by spatial location
#   7. Calculates an empirical spatial semivariogram
#   8. Saves the plot and semivariogram table
#
# ============================================================


# ============================================================
# 1. LIBRARIES
# ============================================================

library(data.table)
library(gstat)
library(ggplot2)
source("config/config.R")



data_file <- file.path(

  processed_dir,

  "Mesic_dataset_250k.csv"

)



model_dir_250k <- file.path(

  model_dir,

  "test_30km_model_250k"

)



model_file <- file.path(

  model_dir_250k,

  "test_30km_beta_spde_year_month_rw1.rds"

)



output_dir <- file.path(

  model_dir_250k,

  "semivariogram"

)

# ============================================================
# 2. PATHS
# ============================================================

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. READ DATA
# ============================================================

cat("\n=========================================\n")
cat("READING DATA\n")
cat("=========================================\n")

dt <- fread(data_file)

setDT(dt)

cat(
  "Original rows:",
  nrow(dt),
  "\n"
)


# ============================================================
# 4. REPRODUCE MODEL SAMPLE
# ============================================================
#
# IMPORTANT:
# The original 30-km model used:
#
#   set.seed(123)
#   sample 10,000 rows
#
# We must reproduce exactly the same sample so the fitted
# values correspond to the same observations.
#
# ============================================================

set.seed(123)

dt <- dt[
  sample(
    
    .N,
    size = min(250000, .N)
  )
]

cat(
  "Rows after sampling:",
  nrow(dt),
  "\n"
)


# ============================================================
# 5. LAND TENURE
# ============================================================

dt[
  trimws(land_tenure) == "",
  land_tenure := NA_character_
]

dt[
  ,
  land_tenure := factor(land_tenure)
]

dt[
  ,
  land_tenure := relevel(
    land_tenure,
    ref = "Federal"
  )
]


# ============================================================
# 6. ECOREGION
# ============================================================

dt[
  ,
  eco_factor := factor(ecoImg)
]

dt[
  ,
  eco_id := as.integer(eco_factor)
]


# ============================================================
# 7. FIRE OCCURRENCE
# ============================================================

dt[
  ,
  fire_occurrence := as.integer(
    fire_occurrence
  )
]


# ============================================================
# 8. TEMPORAL INDICES
# ============================================================

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
  year_rw := match(
    year,
    year_levels
  )
]

dt[
  ,
  month_rw := match(
    month,
    month_levels
  )
]


# ============================================================
# 9. COMPLETE CASE FILTER
# ============================================================
#
# Same variables used in the original model.
#
# ============================================================

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


cat(
  "Before complete-case filter:",
  before,
  "\n"
)

cat(
  "After complete-case filter:",
  after,
  "\n"
)

cat(
  "Removed:",
  before - after,
  "\n"
)


# ============================================================
# 10. LOAD FITTED MODEL
# ============================================================

cat("\n=========================================\n")
cat("LOADING MODEL\n")
cat("=========================================\n")

result <- readRDS(
  model_file
)

cat(
  "Model loaded:",
  model_file,
  "\n"
)


# ============================================================
# 11. CHECK MODEL/DATA DIMENSIONS
# ============================================================

n_fitted <- nrow(
  result$summary.fitted.values
)

n_data <- nrow(dt)


cat(
  "Data observations:",
  n_data,
  "\n"
)

cat(
  "Model fitted values:",
  n_fitted,
  "\n"
)


if (n_fitted != n_data) {
  
  stop(
    paste0(
      "\nERROR:\n",
      "Number of fitted values does not match number of data rows.\n",
      "Data rows: ",
      n_data,
      "\n",
      "Fitted values: ",
      n_fitted,
      "\n\n",
      "The preprocessing does not exactly reproduce the ",
      "dataset used to fit the model."
    )
  )
}


# ============================================================
# 12. EXTRACT FITTED VALUES
# ============================================================

dt[
  ,
  fitted := result$summary.fitted.values$mean
]


# ============================================================
# 13. CALCULATE RESIDUALS
# ============================================================

dt[
  ,
  residual := mesic_beta - fitted
]


cat("\nResidual summary:\n")

print(
  summary(
    dt$residual
  )
)


# ============================================================
# 14. CREATE ONE VALUE PER SPATIAL LOCATION
# ============================================================
#
# The long dataset contains repeated measurements through
# years/months at the same spatial locations.
#
# For a purely spatial semivariogram, average the residuals
# over time at each location.
#
# ============================================================

if ("location_id" %in% names(dt)) {
  
  residual_spatial <- dt[
    ,
    .(
      x = first(x),
      y = first(y),
      residual = mean(
        residual,
        na.rm = TRUE
      ),
      n_observations = .N
    ),
    by = location_id
  ]
  
} else {
  
  # Fallback if location_id is not available:
  # use unique x/y coordinate combinations.
  
  residual_spatial <- dt[
    ,
    .(
      residual = mean(
        residual,
        na.rm = TRUE
      ),
      n_observations = .N
    ),
    by = .(
      x,
      y
    )
  ]
}


cat("\n=========================================\n")
cat("SPATIAL LOCATIONS\n")
cat("=========================================\n")

cat(
  "Unique spatial locations:",
  nrow(residual_spatial),
  "\n"
)


# ============================================================
# 15. REMOVE INVALID RESIDUALS
# ============================================================

residual_spatial <- residual_spatial[
  is.finite(residual) &
    is.finite(x) &
    is.finite(y)
]

cat(
  "Locations used:",
  nrow(residual_spatial),
  "\n"
)


# ============================================================
# 16. EMPIRICAL SEMIVARIOGRAM
# ============================================================
#
# x and y are projected coordinates in meters.
#
# cutoff = 500 km
# width  = 10 km
#
# Therefore:
#
#   0-10 km
#   10-20 km
#   20-30 km
#   ...
#   490-500 km
#
# ============================================================

cat("\n=========================================\n")
cat("CALCULATING SEMIVARIOGRAM\n")
cat("=========================================\n")


vg_empirical <- variogram(
  residual ~ 1,
  locations = ~ x + y,
  data = residual_spatial,
  cutoff = 500000,
  width = 10000
)


# Distance returned by gstat is in the same units as x/y.
# Coordinates are meters, so convert to km.

vg_empirical$distance_km <- (
  vg_empirical$dist / 1000
)


cat("\nEmpirical semivariogram:\n")

print(
  vg_empirical
)


# ============================================================
# 17. SAVE SEMIVARIOGRAM TABLE
# ============================================================

vg_table <- as.data.table(
  vg_empirical
)

fwrite(
  vg_table,
  file.path(
    output_dir,
    "residual_semivariogram.csv"
  )
)


# ============================================================
# 18. PLOT SEMIVARIOGRAM
# ============================================================

p <- ggplot(
  vg_empirical,
  aes(
    x = distance_km,
    y = gamma
  )
) +
  
  geom_point(
    size = 2.5
  ) +
  
  geom_line(
    linewidth = 0.7
  ) +
  
  labs(
    title =
      "Residual Spatial Semivariogram",
    
    subtitle =
      "30-km INLA-SPDE model",
    
    x =
      "Distance (km)",
    
    y =
      "Semivariance"
  ) +
  
  theme_bw(
    base_size = 14
  )


print(p)


# ============================================================
# 19. SAVE FIGURE
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "residual_semivariogram.png"
  ),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)


# ============================================================
# 20. SAVE SPATIAL RESIDUALS
# ============================================================

fwrite(
  residual_spatial,
  file.path(
    output_dir,
    "spatial_mean_residuals.csv"
  )
)


# ============================================================
# 21. DONE
# ============================================================

cat("\n=========================================\n")
cat("SEMIVARIOGRAM COMPLETE\n")
cat("=========================================\n")

cat(
  "\nOutputs saved in:\n",
  output_dir,
  "\n"
)

cat(
  "\nFiles:\n",
  "  residual_semivariogram.csv\n",
  "  residual_semivariogram.png\n",
  "  spatial_mean_residuals.csv\n"
)

cat("\n=========================================\n")
