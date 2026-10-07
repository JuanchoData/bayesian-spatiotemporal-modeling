# ============================================================

# 01_merge_datasets.R

#

# PURPOSE:

#   Combine yearly/partitioned CSV files into one dataset.

#

# EXPECTED INPUT:

#   data/raw/*.csv

#

# EXPECTED FILE NAMES:

#   ..._1985_A1.csv

#   ..._1985_A2.csv

#   ..._1985_B1.csv

#   ..._1985_B2.csv

#   ...

#

# ACTIONS:

#   1. Read all CSV files from data/raw/

#   2. Standardize column names

#   3. Extract source year and partition from filename

#   4. Standardize 2025 mesic variable names

#   5. Combine all files

#   6. Extract longitude/latitude from .geo

#   7. Validate coordinate uniqueness

#   8. Save combined dataset

#

# OUTPUT:

#   data/processed/Mesic_TerraClimate_ALL.csv

#

# ============================================================





library(data.table)





# ============================================================

# 1. LOAD PROJECT CONFIGURATION

# ============================================================



source("config/config.R")





# ============================================================

# 2. PATHS

# ============================================================



raw_dir <- file.path(

  data_dir,

  "raw"

)



processed_dir <- file.path(

  data_dir,

  "processed"

)



dir.create(

  raw_dir,

  recursive = TRUE,

  showWarnings = FALSE

)



dir.create(

  processed_dir,

  recursive = TRUE,

  showWarnings = FALSE

)





output_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_ALL.csv"

)





# ============================================================

# 3. FIND INPUT FILES

# ============================================================



cat("\n========================================\n")

cat("FINDING INPUT FILES\n")

cat("========================================\n")





files <- list.files(

  raw_dir,

  pattern = "\\.csv$",

  full.names = TRUE

)





files <- sort(files)





cat(

  "Files found:",

  length(files),

  "\n"

)





if (length(files) == 0) {



  stop(

    "No CSV files found in: ",

    raw_dir

  )

}





cat("\nInput files:\n")



print(

  basename(files)

)





# ============================================================

# 4. COMBINE INPUT FILES

# ============================================================



cat("\n========================================\n")

cat("READING AND COMBINING FILES\n")

cat("========================================\n")





DT_list <- vector(

  "list",

  length(files)

)





for (i in seq_along(files)) {



  filename <- basename(files[i])



  cat(

    "[",

    i,

    "/",

    length(files),

    "] ",

    filename,

    "\n",

    sep = ""

  )





  # ----------------------------------------------------------

  # Read file

  # ----------------------------------------------------------



  dt <- fread(

    files[i],

    showProgress = FALSE

  )





  # ----------------------------------------------------------

  # Remove possible UTF-8 BOM from column names

  # ----------------------------------------------------------



  setnames(

    dt,

    names(dt),

    sub(

      "^\ufeff",

      "",

      names(dt)

    )

  )





  # ----------------------------------------------------------

  # Extract source year from filename

  #

  # Example:

  # something_1985_A1.csv -> 1985

  # ----------------------------------------------------------



  yr <- as.integer(

    sub(

      ".*_([0-9]{4})_[AB][12]\\.csv$",

      "\\1",

      filename

    )

  )





  # ----------------------------------------------------------

  # Extract source partition

  #

  # Example:

  # something_1985_A1.csv -> A1

  # ----------------------------------------------------------



  part <- sub(

    ".*_([AB][12])\\.csv$",

    "\\1",

    filename

  )





  # ----------------------------------------------------------

  # Validate filename parsing

  # ----------------------------------------------------------



  if (is.na(yr)) {



    stop(

      "Could not extract year from filename: ",

      filename

    )

  }





  if (!part %in% c(

    "A1",

    "A2",

    "B1",

    "B2"

  )) {



    stop(

      "Could not extract partition from filename: ",

      filename

    )

  }





  # ----------------------------------------------------------

  # Standardize 2025 mesic names if necessary

  #

  # Example:

  # mesicWRP_202506 -> mesic_202506

  # ----------------------------------------------------------



  wrp_cols <- grep(

    "^mesicWRP_[0-9]{6}$",

    names(dt),

    value = TRUE

  )





  if (length(wrp_cols) > 0) {



    setnames(

      dt,

      old = wrp_cols,

      new = sub(

        "^mesicWRP_",

        "mesic_",

        wrp_cols

      )

    )

  }





  # ----------------------------------------------------------

  # Add source metadata

  # ----------------------------------------------------------



  dt[

    ,

    source_year := yr

  ]





  dt[

    ,

    source_part := part

  ]





  # ----------------------------------------------------------

  # Store in list

  # ----------------------------------------------------------



  DT_list[[i]] <- dt

}





# ============================================================

# 5. COMBINE ALL FILES

# ============================================================



cat("\nCombining files...\n")





DT <- rbindlist(

  DT_list,

  use.names = TRUE,

  fill = TRUE

)





rm(

  DT_list,

  dt

)



gc()





cat(

  "\nRows:",

  format(

    nrow(DT),

    big.mark = ","

  ),

  "\n"

)





cat(

  "Columns:",

  ncol(DT),

  "\n"

)





cat("\nRows by source year:\n")



print(

  DT[

    ,

    .N,

    by = source_year

  ][

    order(source_year)

  ]

)





cat("\nRows by source year and partition:\n")



print(

  DT[

    ,

    .N,

    by = .(

      source_year,

      source_part

    )

  ][

    order(

      source_year,

      source_part

    )

  ]

)





# ============================================================

# 6. CHECK .geo FIELD

# ============================================================



if (!".geo" %in% names(DT)) {



  stop(

    "Column '.geo' not found. ",

    "Longitude/latitude cannot be extracted."

  )

}





# ============================================================

# 7. EXTRACT LONGITUDE / LATITUDE FROM .geo

# ============================================================



cat("\n========================================\n")

cat("EXTRACTING LONGITUDE / LATITUDE\n")

cat("========================================\n")





coords_txt <- sub(

  '.*coordinates""?:\\[([^]]+)\\].*',

  '\\1',

  DT$.geo

)





DT[

  ,

  longitude := as.numeric(

    sub(

      ",.*$",

      "",

      coords_txt

    )

  )

]





DT[

  ,

  latitude := as.numeric(

    sub(

      "^.*,",

      "",

      coords_txt

    )

  )

]





rm(coords_txt)



gc()





# ============================================================

# 8. COORDINATE VALIDATION

# ============================================================



cat("\n========================================\n")

cat("COORDINATE VALIDATION\n")

cat("========================================\n")





cat(

  "Missing longitude:",

  sum(

    is.na(DT$longitude)

  ),

  "\n"

)





cat(

  "Missing latitude:",

  sum(

    is.na(DT$latitude)

  ),

  "\n"

)





cat(

  "Longitude range:",

  paste(

    range(

      DT$longitude,

      na.rm = TRUE

    ),

    collapse = " to "

  ),

  "\n"

)





cat(

  "Latitude range:",

  paste(

    range(

      DT$latitude,

      na.rm = TRUE

    ),

    collapse = " to "

  ),

  "\n"

)





# ============================================================

# 9. CHECK LOCATION UNIQUENESS

# ============================================================



cat("\n========================================\n")

cat("LOCATION UNIQUENESS CHECK\n")

cat("========================================\n")





location_check <- DT[

  ,

  .(

    rows = .N,



    unique_geo =

      uniqueN(

        .geo

      ),



    unique_lonlat =

      uniqueN(

        paste(

          longitude,

          latitude,

          sep = "_"

        )

      )

  ),

  by = source_year

][

  order(source_year)

]





print(

  location_check

)





# ============================================================

# 10. CHECK LOCATION UNIQUENESS BY PARTITION

# ============================================================



cat("\n========================================\n")

cat("LOCATION CHECK BY PARTITION\n")

cat("========================================\n")





location_part_check <- DT[

  ,

  .(

    rows = .N,



    unique_geo =

      uniqueN(

        .geo

      ),



    unique_lonlat =

      uniqueN(

        paste(

          longitude,

          latitude,

          sep = "_"

        )

      )

  ),

  by = .(

    source_year,

    source_part

  )

][

  order(

    source_year,

    source_part

  )

]





print(

  location_part_check

)





# ============================================================

# 11. FAIL IF COORDINATES WERE COLLAPSED

# ============================================================



bad_locations <- location_check[

  unique_lonlat != unique_geo

]





if (nrow(bad_locations) > 0) {



  print(

    bad_locations

  )



  stop(

    "Coordinate validation failed: ",

    "unique longitude/latitude count does not match unique geometry count."

  )

}





# ============================================================

# 12. SAVE COMBINED DATASET

# ============================================================



cat("\n========================================\n")

cat("SAVING COMBINED DATASET\n")

cat("========================================\n")





fwrite(

  DT,

  output_file

)





cat(

  "\nSaved:\n",

  output_file,

  "\n"

)





cat(

  "\nFinal rows:",

  format(

    nrow(DT),

    big.mark = ","

  ),

  "\n"

)





cat(

  "Final columns:",

  ncol(DT),

  "\n"

)





cat("\nDONE\n")
