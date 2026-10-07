# ============================================================
# 06_prepare_long_inla_data.R
#
# Convert wide Mesic + TerraClimate table to long INLA format
#
# Output:
#   one row per location / year / month
#
# Months:
#   June      = 6
#   July      = 7
#   August    = 8
#   September = 9
# ============================================================

library(data.table)

source("config/config.R")



input_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_CLEAN_NORMALIZED_XY.csv"

)



output_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_LONG_INLA.csv"

)
# ============================================================
# 1. PATHS
# ============================================================
# ============================================================
# 2. READ DATA
# ============================================================

cat("\n=========================================\n")
cat("READING WIDE DATA\n")
cat("=========================================\n")

dt <- fread(input_file)

cat("Rows:", nrow(dt), "\n")
cat("Columns:", ncol(dt), "\n")

cat("\nYears:\n")
print(sort(unique(dt$year)))


# ============================================================
# 3. CREATE LOCATION ID
# ============================================================

cat("\n=========================================\n")
cat("CREATING LOCATION ID\n")
cat("=========================================\n")

# Same x/y location receives same ID across years
dt[, location_id := .GRP, by = .(x, y)]

cat("Unique locations:", uniqueN(dt$location_id), "\n")


# ============================================================
# 4. DEFINE MONTHS
# ============================================================

month_lookup <- data.table(
  month = c(6L, 7L, 8L, 9L),
  month_name = c(
    "jun",
    "jul",
    "aug",
    "sep"
  )
)


# ============================================================
# 5. CHECK YEARS
# ============================================================

expected_years <- c(
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

actual_years <- sort(unique(dt$year))

cat("\nExpected years:\n")
print(expected_years)

cat("\nActual years:\n")
print(actual_years)


# ============================================================
# 6. REMOVE OLD OUTPUT IF IT EXISTS
# ============================================================

if (file.exists(output_file)) {
  file.remove(output_file)
}


# ============================================================
# 7. VARIABLES TO KEEP
# ============================================================

static_variables <- c(
  "location_id",
  "x",
  "y",
  "longitude",
  "latitude",
  "elevation",
  "slope",
  "aspect",
  "distanceToRoads",
  "distanceToWater",
  "ecoImg",
  "county_name",
  "county_population",
  "land_tenure",
  "fire_occurrence",
  "n_fires"
)


# ============================================================
# 8. CONVERT WIDE -> LONG
# ============================================================

cat("\n=========================================\n")
cat("CONVERTING TO LONG FORMAT\n")
cat("=========================================\n")

first_write <- TRUE

for (yr in expected_years) {
  
  cat("\n-----------------------------------------\n")
  cat("YEAR:", yr, "\n")
  cat("-----------------------------------------\n")
  
  # Rows belonging to this year
  dty <- dt[year == yr]
  
  cat("Rows for year:", nrow(dty), "\n")
  
  if (nrow(dty) == 0) {
    cat("No rows found. Skipping year.\n")
    next
  }
  
  
  # ----------------------------------------------------------
  # SPI6 YEAR
  #
  # Available fields use SPI6_2024_* for the final 2025 mesic
  # observations.
  # ----------------------------------------------------------
  
  spi_year <- yr
  
  if (yr == 2025) {
    spi_year <- 2024
  }
  
  
  # ----------------------------------------------------------
  # PROCESS EACH MONTH
  # ----------------------------------------------------------
  
  for (i in seq_len(nrow(month_lookup))) {
    
    mon <- month_lookup$month[i]
    mon_name <- month_lookup$month_name[i]
    
    cat(
      "Processing:",
      yr,
      "-",
      sprintf("%02d", mon),
      "\n"
    )
    
    
    # --------------------------------------------------------
    # COLUMN NAMES
    # --------------------------------------------------------
    
    mesic_col <- paste0(
      "mesic_",
      yr,
      sprintf("%02d", mon)
    )
    
    spi_col <- paste0(
      "SPI6_",
      spi_year,
      "_",
      sprintf("%02d", mon)
    )
    
    pdsi_col <- paste0("pdsi_", mon_name)
    pr_col   <- paste0("pr_", mon_name)
    ro_col   <- paste0("ro_", mon_name)
    soil_col <- paste0("soil_", mon_name)
    vpd_col  <- paste0("vpd_", mon_name)
    
    
    # --------------------------------------------------------
    # CHECK COLUMNS
    # --------------------------------------------------------
    
    required_cols <- c(
      mesic_col,
      spi_col,
      pdsi_col,
      pr_col,
      ro_col,
      soil_col,
      vpd_col
    )
    
    missing_cols <- setdiff(
      required_cols,
      names(dty)
    )
    
    if (length(missing_cols) > 0) {
      
      stop(
        paste(
          "Missing columns for",
          yr,
          mon,
          ":",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
    
    
    # --------------------------------------------------------
    # CREATE LONG BLOCK
    # --------------------------------------------------------
    
    out <- dty[
      ,
      c(
        .(
          year = yr,
          month = mon,
          
          # Exact 0-1 proportion
          mesic_prop = get(mesic_col),
          
          SPI6 = get(spi_col),
          
          pdsi = get(pdsi_col),
          pr   = get(pr_col),
          ro   = get(ro_col),
          soil = get(soil_col),
          vpd  = get(vpd_col)
        ),
        
        .SD
      ),
      .SDcols = static_variables
    ]
    
    
    # --------------------------------------------------------
    # BETA RESPONSE
    #
    # Beta likelihood requires:
    #
    #       0 < y < 1
    #
    # Therefore preserve mesic_prop exactly, but create a
    # beta-safe version for INLA.
    # --------------------------------------------------------
    
    out[
      ,
      mesic_beta :=
        pmin(
          pmax(
            mesic_prop,
            1e-6
          ),
          1 - 1e-6
        )
    ]
    
    
    # --------------------------------------------------------
    # COLUMN ORDER
    # --------------------------------------------------------
    
    setcolorder(
      out,
      c(
        "location_id",
        "year",
        "month",
        
        "mesic_prop",
        "mesic_beta",
        
        "SPI6",
        "pdsi",
        "pr",
        "ro",
        "soil",
        "vpd",
        
        "x",
        "y",
        "longitude",
        "latitude",
        
        "elevation",
        "slope",
        "aspect",
        
        "distanceToRoads",
        "distanceToWater",
        
        "ecoImg",
        
        "county_name",
        "county_population",
        
        "land_tenure",
        
        "fire_occurrence",
        "n_fires"
      )
    )
    
    
    # --------------------------------------------------------
    # WRITE TO DISK
    #
    # Write incrementally so we do not need to keep the
    # complete long dataset in RAM.
    # --------------------------------------------------------
    
    fwrite(
      out,
      output_file,
      append = !first_write,
      col.names = first_write
    )
    
    first_write <- FALSE
    
    cat(
      "Written rows:",
      nrow(out),
      "\n"
    )
    
    rm(out)
    gc()
  }
  
  rm(dty)
  gc()
}


# ============================================================
# 9. FINAL CHECK
# ============================================================

cat("\n=========================================\n")
cat("CHECKING OUTPUT\n")
cat("=========================================\n")

check <- fread(
  output_file,
  nrows = 100000
)

cat("\nColumns:\n")
print(names(check))

cat("\nYears:\n")
print(sort(unique(check$year)))

cat("\nMonths:\n")
print(sort(unique(check$month)))

cat("\nMesic proportion range:\n")
print(
  range(
    check$mesic_prop,
    na.rm = TRUE
  )
)

cat("\nBeta response range:\n")
print(
  range(
    check$mesic_beta,
    na.rm = TRUE
  )
)

cat("\nOutput file:\n")
cat(output_file, "\n")

cat("\n=========================================\n")
cat("LONG INLA DATASET COMPLETE\n")
cat("=========================================\n")
