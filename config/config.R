# ============================================================

# PROJECT CONFIGURATION

# ============================================================



project_dir <- Sys.getenv(

  "BAYES_SPATIOTEMPORAL_PROJECT",

  unset = getwd()

)



data_dir <- file.path(

  project_dir,

  "data"

)



processed_dir <- file.path(

  data_dir,

  "processed"

)



output_dir <- file.path(

  project_dir,

  "outputs"

)



model_dir <- file.path(

  project_dir,

  "models"

)
