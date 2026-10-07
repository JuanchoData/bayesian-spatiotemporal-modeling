library(data.table)
library(sf)

source("config/config.R")



input_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_CLEAN_NORMALIZED.csv"

)



output_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_CLEAN_NORMALIZED_XY.csv"

)


cat("\n========================================\n")
cat("READING DATA\n")
cat("========================================\n")

dt <- fread(input_file)

cat("Rows:", format(nrow(dt), big.mark = ","), "\n")
cat("Columns:", ncol(dt), "\n")


# ============================================================
# PROJECT LONGITUDE/LATITUDE TO METERS
# EPSG:4326 -> EPSG:5070
# ============================================================

cat("\n========================================\n")
cat("PROJECTING COORDINATES TO EPSG:5070\n")
cat("========================================\n")

pts_sf <- st_as_sf(
  dt,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

pts_5070 <- st_transform(
  pts_sf,
  crs = 5070
)

xy <- st_coordinates(
  pts_5070
)

dt[, x := xy[, 1]]
dt[, y := xy[, 2]]


# ============================================================
# CHECK COORDINATE RANGES
# ============================================================

cat("\n========================================\n")
cat("PROJECTED COORDINATE RANGES\n")
cat("========================================\n")

cat("\nX range (meters):\n")
print(range(dt$x, na.rm = TRUE))

cat("\nY range (meters):\n")
print(range(dt$y, na.rm = TRUE))

cat("\nX extent (km):\n")
print(diff(range(dt$x, na.rm = TRUE)) / 1000)

cat("\nY extent (km):\n")
print(diff(range(dt$y, na.rm = TRUE)) / 1000)


# ============================================================
# SAVE
# ============================================================

cat("\n========================================\n")
cat("SAVING DATA\n")
cat("========================================\n")

fwrite(
  dt,
  output_file
)

cat("\nSaved:\n", output_file, "\n")
cat("\nDONE\n")
