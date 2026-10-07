# ============================================================
# 8_clean_normalize_predictors.R
#
# INPUT:
# Mesic_TerraClimate_LandTenure_FIRE_POP.csv
#
# ACTIONS:
#   1. Remove unnecessary identifiers / processing fields
#   2. Preserve all mesic response variables unchanged
#   3. Preserve categorical / binary / spatial identifiers
#   4. Normalize continuous numeric predictors using:
#
#        (x - mean(x)) / (2 * sd(x))
#
#   5. Save scaling parameters for reproducibility
#
# OUTPUT:
# Mesic_TerraClimate_CLEAN_NORMALIZED.csv
#
# ============================================================


library(data.table)
source("config/config.R")



input_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_LandTenure_FIRE_POP.csv"

)



output_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_CLEAN_NORMALIZED.csv"

)



scaling_file <- file.path(

  processed_dir,

  "predictor_scaling_parameters.csv"

)

# ============================================================
# 1. PATHS
# ============================================================

# ============================================================
# 2. READ DATA
# ============================================================

cat("\n========================================\n")
cat("READING DATA\n")
cat("========================================\n")


dt <- fread(
  input_file,
  showProgress = TRUE
)


cat(
  "Rows:",
  format(nrow(dt), big.mark = ","),
  "\n"
)


cat(
  "Columns:",
  ncol(dt),
  "\n"
)


# ============================================================
# 3. REMOVE UNNEEDED VARIABLES
# ============================================================

cat("\n========================================\n")
cat("REMOVING UNNEEDED VARIABLES\n")
cat("========================================\n")


remove_vars <- c(
  "system:index",
  "COUNTYFP",
  "FIPS",
  "STATEFP",
  ".geo",
  "source_year",
  "source_part",
  "rand"
)


# Only remove variables that exist

remove_existing <- intersect(
  remove_vars,
  names(dt)
)


cat(
  "Removing:",
  paste(
    remove_existing,
    collapse = ", "
  ),
  "\n"
)


dt[
  ,
  (remove_existing) := NULL
]


cat(
  "Columns after removal:",
  ncol(dt),
  "\n"
)


# ============================================================
# 4. IDENTIFY MESIC VARIABLES
# ============================================================

cat("\n========================================\n")
cat("IDENTIFYING MESIC RESPONSE VARIABLES\n")
cat("========================================\n")


mesic_vars <- grep(
  "^mesic_[0-9]{6}$",
  names(dt),
  value = TRUE
)


cat(
  "Mesic variables found:",
  length(mesic_vars),
  "\n"
)


print(
  mesic_vars
)


# ============================================================
# 5. VARIABLES THAT MUST NOT BE NORMALIZED
# ============================================================

# These are intentionally preserved in original units/coding.

do_not_scale <- c(
  mesic_vars,
  
  # temporal identifier
  "year",
  
  # spatial coordinates needed for mesh
  "longitude",
  "latitude",
  
  # categorical / IDs
  "ecoImg",
  "county_name",
  "land_tenure",
  
  # binary predictor
  "fire_occurrence"
)


do_not_scale <- intersect(
  do_not_scale,
  names(dt)
)


cat("\nVariables NOT normalized:\n")

print(
  do_not_scale
)


# ============================================================
# 6. FIND NUMERIC VARIABLES
# ============================================================

numeric_vars <- names(dt)[
  vapply(
    dt,
    is.numeric,
    logical(1)
  )
]


cat("\nNumeric variables found:\n")

print(
  numeric_vars
)


# ============================================================
# 7. SELECT CONTINUOUS NUMERIC PREDICTORS TO NORMALIZE
# ============================================================

scale_vars <- setdiff(
  numeric_vars,
  do_not_scale
)


cat("\n========================================\n")
cat("NUMERIC PREDICTORS TO NORMALIZE\n")
cat("========================================\n")


print(
  scale_vars
)


cat(
  "\nNumber of predictors to normalize:",
  length(scale_vars),
  "\n"
)


# ============================================================
# 8. CHECK STANDARD DEVIATIONS
# ============================================================

cat("\n========================================\n")
cat("CHECKING STANDARD DEVIATIONS\n")
cat("========================================\n")


scaling_parameters <- rbindlist(
  lapply(
    scale_vars,
    function(v) {
      
      vals <- dt[[v]]
      
      data.table(
        variable = v,
        
        mean = mean(
          vals,
          na.rm = TRUE
        ),
        
        sd = sd(
          vals,
          na.rm = TRUE
        ),
        
        min = min(
          vals,
          na.rm = TRUE
        ),
        
        max = max(
          vals,
          na.rm = TRUE
        ),
        
        missing =
          sum(
            is.na(vals)
          )
      )
    }
  )
)


print(
  scaling_parameters
)


# ============================================================
# 9. REMOVE ZERO-VARIANCE VARIABLES FROM SCALING
# ============================================================

zero_sd <- scaling_parameters[
  is.na(sd) |
    sd == 0,
  variable
]


if (length(zero_sd) > 0) {
  
  cat("\nWARNING: Variables with zero/undefined SD:\n")
  
  print(
    zero_sd
  )
  
  scale_vars <- setdiff(
    scale_vars,
    zero_sd
  )
}


# ============================================================
# 10. SAVE SCALING PARAMETERS
# ============================================================

cat("\n========================================\n")
cat("SAVING SCALING PARAMETERS\n")
cat("========================================\n")


fwrite(
  scaling_parameters,
  scaling_file
)


cat(
  "Saved scaling parameters:\n",
  scaling_file,
  "\n"
)


# ============================================================
# 11. NORMALIZE CONTINUOUS PREDICTORS
#
# Formula:
#
#   z = (x - mean(x)) / (2 * sd(x))
#
# ============================================================

cat("\n========================================\n")
cat("NORMALIZING PREDICTORS\n")
cat("========================================\n")


for (v in scale_vars) {
  
  cat(
    "Normalizing:",
    v,
    "\n"
  )
  
  
  mu <- scaling_parameters[
    variable == v,
    mean
  ]
  
  
  sigma <- scaling_parameters[
    variable == v,
    sd
  ]
  
  
  set(
    dt,
    j = v,
    value =
      (
        dt[[v]] - mu
      ) /
      (
        2 * sigma
      )
  )
}


# ============================================================
# 12. VERIFY MESIC VALUES WERE NOT CHANGED
# ============================================================

cat("\n========================================\n")
cat("MESIC VARIABLES PRESERVED\n")
cat("========================================\n")


cat(
  "Number of mesic columns:",
  length(mesic_vars),
  "\n"
)


# Show ranges only

mesic_check <- rbindlist(
  lapply(
    mesic_vars,
    function(v) {
      
      data.table(
        variable = v,
        
        min = min(
          dt[[v]],
          na.rm = TRUE
        ),
        
        max = max(
          dt[[v]],
          na.rm = TRUE
        )
      )
    }
  )
)


print(
  mesic_check
)


# ============================================================
# 13. VERIFY NORMALIZED PREDICTORS
# ============================================================

cat("\n========================================\n")
cat("NORMALIZATION CHECK\n")
cat("========================================\n")


normalization_check <- rbindlist(
  lapply(
    scale_vars,
    function(v) {
      
      data.table(
        
        variable = v,
        
        mean_after =
          mean(
            dt[[v]],
            na.rm = TRUE
          ),
        
        sd_after =
          sd(
            dt[[v]],
            na.rm = TRUE
          ),
        
        min_after =
          min(
            dt[[v]],
            na.rm = TRUE
          ),
        
        max_after =
          max(
            dt[[v]],
            na.rm = TRUE
          )
      )
    }
  )
)


print(
  normalization_check
)


# With scaling by 2 SD:
#
# mean_after should be approximately 0
# sd_after should be approximately 0.5


# ============================================================
# 14. FINAL VARIABLE LIST
# ============================================================

cat("\n========================================\n")
cat("FINAL VARIABLES\n")
cat("========================================\n")


final_variables <- data.table(
  
  number =
    seq_along(
      names(dt)
    ),
  
  variable =
    names(dt)
)


print(
  final_variables
)


cat(
  "\nFinal number of columns:",
  ncol(dt),
  "\n"
)


cat(
  "Final number of rows:",
  format(
    nrow(dt),
    big.mark = ","
  ),
  "\n"
)


# ============================================================
# 15. SAVE FINAL DATASET
# ============================================================

cat("\n========================================\n")
cat("SAVING CLEAN NORMALIZED DATASET\n")
cat("========================================\n")


fwrite(
  dt,
  output_file
)


cat(
  "\nSaved:\n",
  output_file,
  "\n"
)


cat("\nDONE\n")
