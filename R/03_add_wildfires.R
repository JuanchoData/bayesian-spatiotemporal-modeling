# ============================================================
# 6_add_wildfires.R
#
# ADD HISTORICAL WILDFIRE VARIABLES TO MESIC DATA
#
# Creates:
#   fire_occurrence = 1 if location burned before observation year
#                     0 otherwise
#
#   n_fires = number of distinct previous wildfire events
#
# IMPORTANT:
#   A fire is counted only when:
#
#       FIRE_YEAR < observation year
#
#   Example:
#       point observation = 2005
#       fires from 1984, 1992, 2001 are counted
#       a fire from 2005 is NOT counted
#       fires after 2005 are NOT counted
#
# This avoids temporal leakage because CurrentHFD only gives
# us FIRE_YEAR, not necessarily the exact month/date.
#
#
# LAND TENURE:
#
# KEEP:
#   land_tenure
#
# REMOVE:
#   tenure_source_layer
#   tenure_agency_code
#   tenure_unit_name
#   tenure_conflict
#   n_tenure_matches
#   n_polygon_matches
#
#
# Input:

# data/processed/Mesic_TerraClimate_LandTenure_ALL.csv



# Wildfire data:

# data/wildfires/



# Output:

# data/processed/Mesic_TerraClimate_LandTenure_FIRE.csv
# ============================================================
# 1. PACKAGES
# ============================================================

library(sf)
library(data.table)

sf_use_s2(FALSE)

source("config/config.R")



input_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_LandTenure_ALL.csv"

)



wildfire_dir <- file.path(

  data_dir,

  "wildfires"

)



output_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_LandTenure_FIRE.csv"

)

# ============================================================
# 2. FILE PATHS
# ============================================================

# ============================================================
# 3. READ POINT DATA
# ============================================================

cat("\n")
cat("========================================\n")
cat("READING POINT DATA\n")
cat("========================================\n")


pts <- fread(
  points_file,
  showProgress = TRUE
)


cat(
  "\nRows:",
  format(
    nrow(pts),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Columns:",
  ncol(pts),
  "\n"
)


cat("\nOriginal columns:\n")

print(
  names(pts)
)



# ============================================================
# 4. CHECK REQUIRED VARIABLES
# ============================================================

required_vars <- c(
  "longitude",
  "latitude",
  "year"
)


missing_vars <- setdiff(
  required_vars,
  names(pts)
)


if (length(missing_vars) > 0) {
  
  stop(
    paste(
      "Missing required variables:",
      paste(
        missing_vars,
        collapse = ", "
      )
    )
  )
  
}



# ============================================================
# 5. STANDARDIZE DATA TYPES
# ============================================================

cat("\n")
cat("========================================\n")
cat("CHECKING OBSERVATION YEARS\n")
cat("========================================\n")


pts[
  ,
  year := as.integer(year)
]

pts[
  source_year == 2025,
  year := 2025L
]

pts[
  ,
  longitude := as.numeric(longitude)
]


pts[
  ,
  latitude := as.numeric(latitude)
]


cat("\nObservation years:\n")

print(
  pts[
    ,
    .N,
    by = year
  ][
    order(year)
  ]
)



# ============================================================
# 6. REMOVE UNNEEDED LAND-TENURE COLUMNS
# ============================================================

cat("\n")
cat("========================================\n")
cat("CLEANING LAND TENURE VARIABLES\n")
cat("========================================\n")


# land_tenure is the FINAL classification:
#
# federal
# private
# state-local
# tribal
# etc.
#
# The following variables are intermediate metadata/
# diagnostics and are not needed in the final modeling table.


tenure_columns_remove <- c(
  "tenure_source_layer",
  "tenure_agency_code",
  "tenure_unit_name",
  "tenure_conflict",
  "n_tenure_matches",
  "n_polygon_matches"
)


# Only remove columns that actually exist

tenure_columns_remove <- intersect(
  tenure_columns_remove,
  names(pts)
)


if (length(tenure_columns_remove) > 0) {
  
  pts[
    ,
    (tenure_columns_remove) := NULL
  ]
  
}


cat(
  "Removed tenure columns:",
  paste(
    tenure_columns_remove,
    collapse = ", "
  ),
  "\n"
)


if ("land_tenure" %in% names(pts)) {
  
  cat("\nFinal land_tenure classification:\n")
  
  print(
    pts[
      ,
      .N,
      by = land_tenure
    ][
      order(-N)
    ]
  )
  
} else {
  
  warning(
    "land_tenure column was not found."
  )
  
}



# ============================================================
# 7. CHECK COORDINATES
# ============================================================

cat("\n")
cat("========================================\n")
cat("CHECKING COORDINATES\n")
cat("========================================\n")


bad_coordinates <- pts[
  is.na(longitude) |
    is.na(latitude) |
    longitude < -180 |
    longitude > 180 |
    latitude < -90 |
    latitude > 90
]


cat(
  "Rows with invalid coordinates:",
  format(
    nrow(bad_coordinates),
    big.mark = ","
  ),
  "\n"
)


if (nrow(bad_coordinates) > 0) {
  
  stop(
    "Invalid or missing longitude/latitude values detected."
  )
  
}


rm(bad_coordinates)

gc()



# ============================================================
# 8. SAVE ORIGINAL ROW ORDER
# ============================================================

pts[
  ,
  original_order := .I
]



# ============================================================
# 9. CREATE UNIQUE LOCATION ID
# ============================================================

cat("\n")
cat("========================================\n")
cat("CREATING UNIQUE SPATIAL LOCATIONS\n")
cat("========================================\n")


# Do NOT use row number as the spatial ID.
#
# Multiple temporal observations can share the same
# longitude/latitude.
#
# We create one location_id per unique coordinate pair.


locations <- unique(
  pts[
    ,
    .(
      longitude,
      latitude
    )
  ]
)


locations[
  ,
  location_id := .I
]


cat(
  "Unique spatial locations:",
  format(
    nrow(locations),
    big.mark = ","
  ),
  "\n"
)


# Attach location_id to original table

setkey(
  locations,
  longitude,
  latitude
)


pts[
  locations,
  on = .(
    longitude,
    latitude
  ),
  location_id := i.location_id
]


if (anyNA(pts$location_id)) {
  
  stop(
    "Some point records did not receive a location_id."
  )
  
}



# ============================================================
# 10. READ WILDFIRE SHAPEFILE
# ============================================================

cat("\n")
cat("========================================\n")
cat("READING WILDFIRE DATA\n")
cat("========================================\n")


fires <- st_read(
  fire_file,
  quiet = FALSE
)


cat(
  "\nNumber of wildfire polygons:",
  format(
    nrow(fires),
    big.mark = ","
  ),
  "\n"
)


cat("\nWildfire CRS:\n")

print(
  st_crs(fires)
)


cat("\nWildfire fields:\n")

print(
  names(fires)
)



# ============================================================
# 11. PREPARE FIRE VARIABLES
# ============================================================

cat("\n")
cat("========================================\n")
cat("PREPARING FIRE ATTRIBUTES\n")
cat("========================================\n")


# Exact fields found in CurrentHFD.shp:
#
# FIRE_YEAR = year of wildfire
# FireID    = unique wildfire event ID


if (!"FIRE_YEAR" %in% names(fires)) {
  
  stop(
    "FIRE_YEAR was not found in wildfire shapefile."
  )
  
}


if (!"FireID" %in% names(fires)) {
  
  stop(
    "FireID was not found in wildfire shapefile."
  )
  
}


fires$fire_year <- suppressWarnings(
  as.integer(
    fires$FIRE_YEAR
  )
)


fires$fire_id <- as.character(
  fires$FireID
)


# ============================================================
# 12. REMOVE INVALID FIRE RECORDS
# ============================================================

fires <- fires[
  !is.na(fires$fire_year) &
    !is.na(fires$fire_id) &
    fires$fire_id != "",
]


cat(
  "Valid wildfire polygons:",
  format(
    nrow(fires),
    big.mark = ","
  ),
  "\n"
)


cat("\nWildfire year range:\n")

print(
  range(
    fires$fire_year,
    na.rm = TRUE
  )
)


cat(
  "\nUnique FireID events:",
  format(
    length(
      unique(
        fires$fire_id
      )
    ),
    big.mark = ","
  ),
  "\n"
)


# ============================================================
# 13. REMOVE FIRES AFTER FINAL OBSERVATION YEAR
# ============================================================

max_observation_year <- max(
  pts$year,
  na.rm = TRUE
)


fires <- fires[
  fires$fire_year <= max_observation_year,
]


cat(
  "\nWildfire polygons through",
  max_observation_year,
  ":",
  format(
    nrow(fires),
    big.mark = ","
  ),
  "\n"
)

# ============================================================
# 14. CHECK FIRE IDs WITH MULTIPLE POLYGONS
# ============================================================

cat("\n")
cat("========================================\n")
cat("CHECKING MULTIPART FIRE EVENTS\n")
cat("========================================\n")


fire_attributes <- as.data.table(
  st_drop_geometry(
    fires
  )
)


duplicate_fire_ids <- fire_attributes[
  ,
  .N,
  by = .(
    fire_id
  )
][
  N > 1
][
  order(-N)
]


cat(
  "Fire IDs represented by multiple polygons:",
  format(
    nrow(duplicate_fire_ids),
    big.mark = ","
  ),
  "\n"
)


# This is NOT a problem.
#
# We later count unique FireID values,
# not polygon pieces.


if (nrow(duplicate_fire_ids) > 0) {
  
  cat("\nExamples:\n")
  
  print(
    head(
      duplicate_fire_ids,
      10
    )
  )
  
}


rm(
  fire_attributes,
  duplicate_fire_ids
)

gc()



# ============================================================
# 15. CONVERT UNIQUE LOCATIONS TO SF
# ============================================================

cat("\n")
cat("========================================\n")
cat("CONVERTING LOCATIONS TO SF\n")
cat("========================================\n")


locations_sf <- st_as_sf(
  locations,
  coords = c(
    "longitude",
    "latitude"
  ),
  crs = 4326,
  remove = FALSE
)



# ============================================================
# 16. TRANSFORM POINTS TO FIRE CRS
# ============================================================

# CurrentHFD is already projected into an Albers
# Equal Area CRS.
#
# Transforming the points is much cheaper than
# transforming the ~918 MB wildfire shapefile.


locations_sf <- st_transform(
  locations_sf,
  st_crs(fires)
)


cat("\nPoint CRS after transformation:\n")

print(
  st_crs(locations_sf)
)



# ============================================================
# 17. SPATIAL INTERSECTION
# ============================================================

cat("\n")
cat("========================================\n")
cat("INTERSECTING POINTS WITH WILDFIRES\n")
cat("========================================\n")


# Process points in chunks to control memory use.

chunk_size <- 50000L


n_locations <- nrow(
  locations_sf
)


chunk_starts <- seq(
  from = 1L,
  to = n_locations,
  by = chunk_size
)


n_chunks <- length(
  chunk_starts
)


cat(
  "Number of chunks:",
  n_chunks,
  "\n"
)


fire_hits_list <- vector(
  "list",
  n_chunks
)



# ============================================================
# 18. LOOP THROUGH LOCATION CHUNKS
# ============================================================

for (k in seq_along(chunk_starts)) {
  
  
  start_i <- chunk_starts[k]
  
  
  end_i <- min(
    start_i + chunk_size - 1L,
    n_locations
  )
  
  
  cat("\n")
  cat("----------------------------------------\n")
  
  cat(
    "Chunk",
    k,
    "of",
    n_chunks,
    "\n"
  )
  
  
  cat(
    "Locations",
    format(
      start_i,
      big.mark = ","
    ),
    "to",
    format(
      end_i,
      big.mark = ","
    ),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Extract chunk
  # ----------------------------------------------------------
  
  p_chunk <- locations_sf[
    start_i:end_i,
  ]
  
  
  # ----------------------------------------------------------
  # Spatial intersection
  # ----------------------------------------------------------
  
  hits <- st_intersects(
    p_chunk,
    fires,
    sparse = TRUE
  )
  
  
  n_hits <- lengths(
    hits
  )
  
  
  cat(
    "Locations intersecting >=1 wildfire:",
    format(
      sum(
        n_hits > 0
      ),
      big.mark = ","
    ),
    "\n"
  )
  
  
  cat(
    "Raw polygon intersections:",
    format(
      sum(n_hits),
      big.mark = ","
    ),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # No fire intersections
  # ----------------------------------------------------------
  
  if (sum(n_hits) == 0) {
    
    
    fire_hits_list[[k]] <- NULL
    
    
    rm(
      p_chunk,
      hits,
      n_hits
    )
    
    
    gc()
    
    
    next
    
  }
  
  
  # ----------------------------------------------------------
  # Convert sparse intersections to table
  # ----------------------------------------------------------
  
  point_indices <- rep(
    seq_along(hits),
    times = n_hits
  )
  
  
  fire_indices <- unlist(
    hits,
    use.names = FALSE
  )
  
  
  tmp <- data.table(
    
    location_id =
      p_chunk$location_id[
        point_indices
      ],
    
    fire_index =
      fire_indices
    
  )
  
  
  # ----------------------------------------------------------
  # Attach fire attributes
  # ----------------------------------------------------------
  
  tmp[
    ,
    fire_year :=
      fires$fire_year[
        fire_index
      ]
  ]
  
  
  tmp[
    ,
    fire_id :=
      fires$fire_id[
        fire_index
      ]
  ]
  
  
  # ----------------------------------------------------------
  # Remove duplicate polygon pieces from same wildfire
  # ----------------------------------------------------------
  
  tmp <- unique(
    tmp[
      ,
      .(
        location_id,
        fire_id,
        fire_year
      )
    ]
  )
  
  
  cat(
    "Unique location-fire events:",
    format(
      nrow(tmp),
      big.mark = ","
    ),
    "\n"
  )
  
  
  fire_hits_list[[k]] <- tmp
  
  
  # ----------------------------------------------------------
  # Memory cleanup
  # ----------------------------------------------------------
  
  rm(
    p_chunk,
    hits,
    n_hits,
    point_indices,
    fire_indices,
    tmp
  )
  
  
  gc()
  
}



# ============================================================
# 19. COMBINE ALL FIRE INTERSECTIONS
# ============================================================

cat("\n")
cat("========================================\n")
cat("COMBINING FIRE INTERSECTIONS\n")
cat("========================================\n")


fire_history <- rbindlist(
  fire_hits_list,
  use.names = TRUE,
  fill = TRUE
)


rm(
  fire_hits_list
)


gc()


cat(
  "Total unique location-fire records:",
  format(
    nrow(fire_history),
    big.mark = ","
  ),
  "\n"
)



# ============================================================
# 20. REMOVE DUPLICATES AGAIN FOR SAFETY
# ============================================================

if (nrow(fire_history) > 0) {
  
  
  fire_history <- unique(
    fire_history,
    by = c(
      "location_id",
      "fire_id",
      "fire_year"
    )
  )
  
}



# ============================================================
# 21. SUMMARY OF SPATIAL FIRE HISTORY
# ============================================================

if (nrow(fire_history) > 0) {
  
  
  cat("\n")
  cat("========================================\n")
  cat("FIRE INTERSECTIONS BY FIRE YEAR\n")
  cat("========================================\n")
  
  
  print(
    
    fire_history[
      ,
      .(
        fire_events = .N,
        
        locations_burned =
          uniqueN(
            location_id
          )
      ),
      by = fire_year
    ][
      order(
        fire_year
      )
    ]
    
  )
  
}



# ============================================================
# 22. CREATE UNIQUE LOCATION-YEAR OBSERVATIONS
# ============================================================

cat("\n")
cat("========================================\n")
cat("CREATING LOCATION-YEAR TABLE\n")
cat("========================================\n")


observations <- unique(
  pts[
    ,
    .(
      location_id,
      year
    )
  ]
)


setorder(
  observations,
  location_id,
  year
)


cat(
  "Location-year combinations:",
  format(
    nrow(observations),
    big.mark = ","
  ),
  "\n"
)



# ============================================================
# 23. INITIALIZE FIRE VARIABLES
# ============================================================

observations[
  ,
  n_fires := 0L
]


observations[
  ,
  fire_occurrence := 0L
]



# ============================================================
# 24. CALCULATE PRIOR FIRE COUNT
# ============================================================

cat("\n")
cat("========================================\n")
cat("CALCULATING HISTORICAL FIRE VARIABLES\n")
cat("========================================\n")


if (nrow(fire_history) > 0) {
  
  
  # ----------------------------------------------------------
  # Join observations to their intersecting fires
  # ----------------------------------------------------------
  #
  # allow.cartesian is necessary because one location
  # may have multiple wildfire events.
  # ----------------------------------------------------------
  
  
  fire_obs <- merge(
    observations[
      ,
      .(
        location_id,
        year
      )
    ],
    
    fire_history,
    
    by = "location_id",
    
    all.x = FALSE,
    
    allow.cartesian = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Keep only fires BEFORE observation year
  # ----------------------------------------------------------
  
  fire_obs <- fire_obs[
    fire_year < year
  ]
  
  
  cat(
    "Historical location-fire-observation combinations:",
    format(
      nrow(fire_obs),
      big.mark = ","
    ),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Count distinct FireID events
  # ----------------------------------------------------------
  
  if (nrow(fire_obs) > 0) {
    
    
    fire_counts <- fire_obs[
      ,
      .(
        n_fires =
          uniqueN(
            fire_id
          )
      ),
      by = .(
        location_id,
        year
      )
    ]
    
    
    # --------------------------------------------------------
    # Add counts to observations
    # --------------------------------------------------------
    
    observations[
      fire_counts,
      on = .(
        location_id,
        year
      ),
      n_fires := i.n_fires
    ]
    
    
  }
  
  
  rm(
    fire_obs
  )
  
  
  if (exists("fire_counts")) {
    
    rm(
      fire_counts
    )
    
  }
  
  
  gc()
  
}



# ============================================================
# 25. CREATE BINARY FIRE OCCURRENCE
# ============================================================

observations[
  ,
  fire_occurrence :=
    as.integer(
      n_fires > 0
    )
]


observations[
  ,
  n_fires :=
    as.integer(
      n_fires
    )
]



# ============================================================
# 26. FIRE SUMMARY BY OBSERVATION YEAR
# ============================================================

cat("\n")
cat("========================================\n")
cat("FIRE HISTORY BY OBSERVATION YEAR\n")
cat("========================================\n")


fire_summary <- observations[
  ,
  .(
    
    observations = .N,
    
    previously_burned =
      sum(
        fire_occurrence
      ),
    
    percent_previously_burned =
      round(
        100 *
          mean(
            fire_occurrence
          ),
        2
      ),
    
    mean_number_fires =
      round(
        mean(
          n_fires
        ),
        3
      ),
    
    max_number_fires =
      max(
        n_fires
      )
    
  ),
  by = year
][
  order(year)
]


print(
  fire_summary
)



# ============================================================
# 27. DISTRIBUTION OF NUMBER OF FIRES
# ============================================================

cat("\n")
cat("========================================\n")
cat("DISTRIBUTION OF N_FIRES\n")
cat("========================================\n")


print(
  
  observations[
    ,
    .N,
    by = n_fires
  ][
    order(
      n_fires
    )
  ]
  
)



# ============================================================
# 28. JOIN FIRE VARIABLES BACK TO ORIGINAL DATA
# ============================================================

cat("\n")
cat("========================================\n")
cat("ADDING FIRE VARIABLES TO FULL DATA\n")
cat("========================================\n")


pts[
  observations,
  on = .(
    location_id,
    year
  ),
  `:=`(
    
    fire_occurrence =
      i.fire_occurrence,
    
    n_fires =
      i.n_fires
    
  )
]



# ============================================================
# 29. SAFETY CHECK FOR MISSING FIRE VALUES
# ============================================================

pts[
  is.na(
    fire_occurrence
  ),
  fire_occurrence := 0L
]


pts[
  is.na(
    n_fires
  ),
  n_fires := 0L
]



# ============================================================
# 30. RESTORE ORIGINAL ROW ORDER
# ============================================================

setorder(
  pts,
  original_order
)



# ============================================================
# 31. REMOVE INTERNAL PROCESSING VARIABLES
# ============================================================

# location_id is only needed internally.
# original_order is also temporary.


pts[
  ,
  c(
    "location_id",
    "original_order"
  ) := NULL
]



# ============================================================
# 32. FINAL LAND TENURE CHECK
# ============================================================

cat("\n")
cat("========================================\n")
cat("FINAL LAND TENURE CATEGORIES\n")
cat("========================================\n")


if ("land_tenure" %in% names(pts)) {
  
  print(
    
    pts[
      ,
      .N,
      by = land_tenure
    ][
      order(-N)
    ]
    
  )
  
}



# ============================================================
# 33. FINAL FIRE CHECK
# ============================================================

cat("\n")
cat("========================================\n")
cat("FINAL FIRE VARIABLES\n")
cat("========================================\n")


print(
  
  pts[
    ,
    .(
      
      rows = .N,
      
      previous_fire_rows =
        sum(
          fire_occurrence
        ),
      
      percentage =
        round(
          100 *
            mean(
              fire_occurrence
            ),
          2
        ),
      
      mean_n_fires =
        round(
          mean(
            n_fires
          ),
          3
        ),
      
      max_n_fires =
        max(
          n_fires
        )
      
    ),
    by = year
  ][
    order(year)
  ]
  
)



# ============================================================
# 34. SHOW FINAL COLUMNS
# ============================================================

cat("\n")
cat("========================================\n")
cat("FINAL COLUMNS\n")
cat("========================================\n")


print(
  names(pts)
)


cat(
  "\nFinal number of columns:",
  ncol(pts),
  "\n"
)



# ============================================================
# 35. SAVE FINAL CSV
# ============================================================

cat("\n")
cat("========================================\n")
cat("SAVING FINAL DATASET\n")
cat("========================================\n")


fwrite(
  pts,
  output_file
)


cat(
  "\nSaved successfully:\n",
  output_file,
  "\n"
)


cat(
  "\nFinal rows:",
  format(
    nrow(pts),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Final columns:",
  ncol(pts),
  "\n"
)


cat("\nDONE\n")
