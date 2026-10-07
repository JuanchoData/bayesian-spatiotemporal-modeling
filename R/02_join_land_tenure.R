# ============================================================

# JOIN BLM LAND TENURE TO MESIC POINTS

#

# INPUT:

#   Mesic_TerraClimate_ALL.csv
#
# POLYGONS:
#   BLM_SMA_StudyArea.gpkg
#
# OUTPUT:
#   Mesic_TerraClimate_LandTenure_ALL.csv
#
# Strategy:
#   - Read 100,000 points at a time
#   - Convert longitude/latitude -> EPSG:5070
#   - Use st_intersects() with the 361 SMA polygons
#   - Preserve overlap conflicts
#   - Save every completed chunk separately
#   - Resume automatically if job is restarted
# ============================================================
library(data.table)

library(sf)
source("config/config.R")



input_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_ALL.csv"

)



polygon_file <- file.path(

  processed_dir,

  "BLM_SMA_StudyArea.gpkg"

)



output_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_LandTenure_ALL.csv"

)



chunk_dir <- file.path(

  processed_dir,

  "land_tenure_chunks"

)




# ============================================================

# 1. PATHS

# ============================================================

dir.create(

  chunk_dir,

  recursive = TRUE,

  showWarnings = FALSE

)





# ============================================================

# 2. SETTINGS

# ============================================================



chunk_size <- 100000L





# ============================================================

# 3. LOAD SMA POLYGONS

# ============================================================



cat("\n====================================================\n")

cat("LOADING BLM SMA POLYGONS\n")

cat("====================================================\n\n")





sma <- st_read(

  polygon_file,

  quiet = TRUE

)





cat(

  "Polygons loaded:",

  nrow(sma),

  "\n"

)





cat(

  "CRS:",

  st_crs(sma)$epsg,

  "\n"

)





# Make sure polygons use EPSG:5070

if (st_crs(sma)$epsg != 5070) {



  sma <- st_transform(

    sma,

    5070

  )

}





needed_polygon_fields <- c(

  "land_tenure",

  "source_layer",

  "ADMIN_AGENCY_CODE",

  "ADMIN_UNIT_NAME"

)





missing_polygon_fields <- setdiff(

  needed_polygon_fields,

  names(sma)

)





if (length(missing_polygon_fields) > 0) {



  stop(

    "Missing polygon fields: ",

    paste(

      missing_polygon_fields,

      collapse = ", "

    )

  )

}





sma <- sma[

  ,

  needed_polygon_fields

]





sma_attributes <- as.data.table(

  st_drop_geometry(sma)

)





cat("\nPolygon tenure classes:\n")



print(

  table(

    sma_attributes$land_tenure,

    useNA = "ifany"

  )

)





# ============================================================

# 4. READ INPUT HEADER

# ============================================================



header <- fread(

  input_file,

  nrows = 0

)





column_names <- names(header)





if (

  !"longitude" %in% column_names ||

  !"latitude" %in% column_names

) {



  stop(

    "Input must contain longitude and latitude."

  )

}





rm(header)

gc()





# ============================================================

# 5. COUNT INPUT ROWS

# ============================================================



cat("\nCounting mesic rows...\n")





total_rows <- as.integer(

  system(

    paste(

      "wc -l <",

      shQuote(input_file)

    ),

    intern = TRUE

  )

) - 1L





cat(

  "Total rows:",

  total_rows,

  "\n"

)





n_chunks <- ceiling(

  total_rows / chunk_size

)





cat(

  "Chunk size:",

  chunk_size,

  "\n"

)





cat(

  "Number of chunks:",

  n_chunks,

  "\n"

)





# ============================================================

# 6. PROCESS CHUNKS

# ============================================================



cat("\n====================================================\n")

cat("STARTING SPATIAL JOIN\n")

cat("====================================================\n")





for (chunk_id in seq_len(n_chunks)) {





  start_row <- (

    (chunk_id - 1L) *

      chunk_size

  )





  rows_to_read <- min(

    chunk_size,

    total_rows - start_row

  )





  chunk_file <- file.path(

    chunk_dir,

    sprintf(

      "chunk_%05d.csv",

      chunk_id

    )

  )





  cat("\n----------------------------------------------------\n")



  cat(

    "CHUNK",

    chunk_id,

    "OF",

    n_chunks,

    "\n"

  )



  cat(

    "Rows:",

    start_row + 1L,

    "to",

    start_row + rows_to_read,

    "\n"

  )





  # ==========================================================

  # Resume support

  # ==========================================================



  if (file.exists(chunk_file)) {



    cat(

      "Already completed -> skipping\n"

    )



    next

  }





  # ==========================================================

  # Read chunk

  # ==========================================================



  if (start_row == 0L) {



    dt <- fread(

      input_file,

      nrows = rows_to_read

    )



  } else {



    dt <- fread(

      input_file,

      skip = start_row + 1L,

      nrows = rows_to_read,

      header = FALSE,

      col.names = column_names

    )

  }





  dt[

    ,

    .join_id := seq_len(.N)

  ]





  # ==========================================================

  # Initialize output attributes

  # ==========================================================



  dt[

    ,

    `:=`(

      land_tenure = NA_character_,

      tenure_source_layer = NA_character_,

      tenure_agency_code = NA_character_,

      tenure_unit_name = NA_character_,

      n_polygon_matches = 0L,

      n_tenure_matches = 0L,

      tenure_conflict = FALSE

    )

  ]





  valid_coords <- (

    is.finite(dt$longitude) &

      is.finite(dt$latitude)

  )





  cat(

    "Valid coordinates:",

    sum(valid_coords),

    "\n"

  )





  # ==========================================================

  # Spatial processing

  # ==========================================================



  if (any(valid_coords)) {





    dt_valid <- dt[

      valid_coords

    ]





    # --------------------------------------------------------

    # Longitude / latitude -> sf

    # --------------------------------------------------------



    pts <- st_as_sf(

      dt_valid,

      coords = c(

        "longitude",

        "latitude"

      ),

      crs = 4326,

      remove = FALSE

    )





    # --------------------------------------------------------

    # Project points to CONUS Albers

    # --------------------------------------------------------



    pts <- st_transform(

      pts,

      5070

    )





    cat(

      "Finding polygon intersections...\n"

    )





    # --------------------------------------------------------

    # Sparse intersection list

    #

    # More memory efficient than st_join(), because it does

    # not duplicate complete point rows for overlapping

    # polygons.

    # --------------------------------------------------------



    hits <- st_intersects(

      pts,

      sma,

      sparse = TRUE

    )





    n_hits <- lengths(hits)





    cat(

      "Points intersecting >=1 polygon:",

      sum(n_hits > 0),

      "\n"

    )





    cat(

      "Points with >1 polygon intersection:",

      sum(n_hits > 1),

      "\n"

    )





    # --------------------------------------------------------

    # Only build relation table for matched points

    # --------------------------------------------------------



    if (any(n_hits > 0)) {





      relation <- data.table(



        .join_id = rep(

          pts$.join_id,

          n_hits

        ),



        polygon_index = as.integer(

          unlist(

            hits,

            use.names = FALSE

          )

        )

      )





      relation[

        ,

        land_tenure :=

          sma_attributes$land_tenure[

            polygon_index

          ]

      ]





      relation[

        ,

        source_layer :=

          sma_attributes$source_layer[

            polygon_index

          ]

      ]





      relation[

        ,

        ADMIN_AGENCY_CODE :=

          sma_attributes$ADMIN_AGENCY_CODE[

            polygon_index

          ]

      ]





      relation[

        ,

        ADMIN_UNIT_NAME :=

          sma_attributes$ADMIN_UNIT_NAME[

            polygon_index

          ]

      ]





      # ======================================================

      # Summarize multiple polygon intersections

      # ======================================================



      tenure_summary <- relation[

        ,

        {



          tenure_values <- sort(

            unique(

              na.omit(

                land_tenure

              )

            )

          )





          source_values <- sort(

            unique(

              na.omit(

                source_layer

              )

            )

          )





          agency_values <- sort(

            unique(

              na.omit(

                ADMIN_AGENCY_CODE

              )

            )

          )





          unit_values <- sort(

            unique(

              na.omit(

                ADMIN_UNIT_NAME

              )

            )

          )





          n_classes <- length(

            tenure_values

          )





          final_tenure <- if (

            n_classes == 1L

          ) {



            tenure_values[1]



          } else {



            NA_character_

          }





          list(



            land_tenure =

              final_tenure,



            tenure_source_layer =

              if (

                length(source_values) > 0

              ) {



                paste(

                  source_values,

                  collapse = ";"

                )



              } else {



                NA_character_

              },



            tenure_agency_code =

              if (

                length(agency_values) > 0

              ) {



                paste(

                  agency_values,

                  collapse = ";"

                )



              } else {



                NA_character_

              },



            tenure_unit_name =

              if (

                length(unit_values) > 0

              ) {



                paste(

                  unit_values,

                  collapse = ";"

                )



              } else {



                NA_character_

              },



            n_polygon_matches =

              .N,



            n_tenure_matches =

              n_classes,



            tenure_conflict =

              n_classes > 1L

          )

        },



        by = .join_id

      ]





      # ======================================================

      # Join results back to original chunk

      # ======================================================



      dt[

        tenure_summary,

        on = ".join_id",

        `:=`(



          land_tenure =

            i.land_tenure,



          tenure_source_layer =

            i.tenure_source_layer,



          tenure_agency_code =

            i.tenure_agency_code,



          tenure_unit_name =

            i.tenure_unit_name,



          n_polygon_matches =

            i.n_polygon_matches,



          n_tenure_matches =

            i.n_tenure_matches,



          tenure_conflict =

            i.tenure_conflict

        )

      ]





      rm(

        relation,

        tenure_summary

      )

    }





    rm(

      dt_valid,

      pts,

      hits,

      n_hits

    )





    gc()

  }





  # ==========================================================

  # Remove temporary identifier

  # ==========================================================



  dt[

    ,

    .join_id := NULL

  ]





  # ==========================================================

  # Save completed chunk

  # ==========================================================



  fwrite(

    dt,

    chunk_file,
    na = "NA"

  )





  cat(

    "Saved:",

    chunk_file,

    "\n"

  )





  cat("\nChunk tenure counts:\n")



  print(

    table(

      dt$land_tenure,

      useNA = "ifany"

    )

  )





  rm(dt)

  gc()

}





# ============================================================

# 7. VERIFY ALL CHUNKS EXIST

# ============================================================



cat("\n====================================================\n")

cat("VERIFYING CHUNKS\n")

cat("====================================================\n\n")





chunk_files <- file.path(

  chunk_dir,

  sprintf(

    "chunk_%05d.csv",

    seq_len(n_chunks)

  )

)





missing_chunks <- chunk_files[

  !file.exists(chunk_files)

]





if (length(missing_chunks) > 0) {



  stop(

    "Missing chunks: ",

    paste(

      basename(missing_chunks),

      collapse = ", "

    )

  )

}





cat(

  "All",

  length(chunk_files),

  "chunks completed.\n"

)





# ============================================================

# 8. COMBINE CHUNKS INTO FINAL CSV

# ============================================================



cat("\n====================================================\n")

cat("COMBINING CHUNKS\n")

cat("====================================================\n\n")





if (file.exists(output_file)) {



  file.remove(

    output_file

  )

}





for (i in seq_along(chunk_files)) {





  cat(

    "Combining chunk",

    i,

    "of",

    length(chunk_files),

    "\n"

  )





  part <- fread(

    chunk_files[i],
    na.strings = "NA"
    

  )





  fwrite(

    part,

    output_file,

    append = i > 1,

    col.names = i == 1,
    na = "NA"

  )





  rm(part)

  gc()

}





# ============================================================

# 9. FINAL VALIDATION

# ============================================================



cat("\n====================================================\n")

cat("FINAL VALIDATION\n")

cat("====================================================\n\n")





validation_list <- lapply(

  chunk_files,

  function(f) {



    x <- fread(

      f,

      select = c(

        "land_tenure",

        "tenure_conflict"

      )

    )





    result <- x[

      ,

      .N,

      by = .(

        land_tenure,

        tenure_conflict

      )

    ]





    rm(x)



    result

  }

)





validation <- rbindlist(

  validation_list

)[

  ,

  .(

    N = sum(N)

  ),

  by = .(

    land_tenure,

    tenure_conflict

  )

]





setorder(

  validation,

  land_tenure,

  tenure_conflict

)





print(

  validation

)





classified <- validation[

  !is.na(land_tenure),

  sum(N)

]





total <- validation[

  ,

  sum(N)

]





cat(

  "\nClassified points:",

  classified,

  "\n"

)





cat(

  "Total points:",

  total,

  "\n"

)





cat(

  "Percent classified:",

  round(

    100 * classified / total,

    2

  ),

  "%\n"

)





cat(

  "Conflict points:",

  validation[

    tenure_conflict == TRUE,

    sum(N)

  ],

  "\n"

)





# ============================================================

# 10. DONE

# ============================================================



cat("\n====================================================\n")

cat("DONE\n")

cat("====================================================\n\n")





cat(

  "Final output:\n",

  output_file,

  "\n"

)





cat(

  "\nChunk directory retained for restart/audit:\n",

  chunk_dir,

  "\n"

)
