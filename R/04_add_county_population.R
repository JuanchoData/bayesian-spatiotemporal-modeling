library(data.table)
source("config/config.R")



input_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_LandTenure_FIRE.csv"

)



population_file <- file.path(

  processed_dir,

  "county_population_lookup.csv"

)



output_file <- file.path(

  processed_dir,

  "Mesic_TerraClimate_LandTenure_FIRE_POP.csv"

)


cat("\n========================================\n")

cat("READING MAIN DATA\n")

cat("========================================\n")



x <- fread(input_file)



cat("Rows:", format(nrow(x), big.mark = ","), "\n")

cat("Columns:", ncol(x), "\n")



# Standardize identifiers

x[, STATEFP := sprintf("%02d", as.integer(STATEFP))]

x[, COUNTYFP := sprintf("%03d", as.integer(COUNTYFP))]

x[, FIPS := paste0(STATEFP, COUNTYFP)]

x[, year := as.integer(year)]



# Remove Broomfield County, CO

x <- x[

  FIPS != "08014"

]



cat(

  "Rows after removing Broomfield:",

  format(nrow(x), big.mark = ","),

  "\n"

)



cat("\n========================================\n")

cat("READING POPULATION LOOKUP\n")

cat("========================================\n")



pop <- fread(population_file)



pop[, FIPS := sprintf("%05d", as.integer(FIPS))]

pop[, year := as.integer(year)]

pop[, county_population := as.numeric(county_population)]



cat("Lookup rows:", nrow(pop), "\n")



# Check duplicates

dup <- pop[, .N, by = .(FIPS, year)][N > 1]



if (nrow(dup) > 0) {

  print(dup)

  stop("Duplicate FIPS-year combinations found.")

}



cat("\n========================================\n")

cat("JOINING COUNTY POPULATION\n")

cat("========================================\n")



x[, original_order := .I]



x[

  pop,

  on = .(FIPS, year),

  county_population := i.county_population

]



missing <- x[

  is.na(county_population)

]



cat(

  "Rows missing county population:",

  format(nrow(missing), big.mark = ","),

  "\n"

)



if (nrow(missing) > 0) {



  cat("\nMissing county-year combinations:\n")



  print(

    unique(

      missing[

        ,

        .(

          FIPS,

          county_name,

          year

        )

      ]

    )[order(year, FIPS)]

  )



  stop("Population join incomplete.")

}



# Restore original order

setorder(x, original_order)

x[, original_order := NULL]



cat("\n========================================\n")

cat("POPULATION SUMMARY\n")

cat("========================================\n")



print(

  x[

    ,

    .(

      rows = .N,

      counties = uniqueN(FIPS),

      min_population = min(county_population),

      median_population = median(county_population),

      max_population = max(county_population)

    ),

    by = year

  ][order(year)]

)



cat("\n========================================\n")

cat("FINAL DATASET\n")

cat("========================================\n")



cat("Rows:", format(nrow(x), big.mark = ","), "\n")

cat("Columns:", ncol(x), "\n")



cat("\nFinal variables:\n")

print(names(x))



cat("\n========================================\n")

cat("SAVING\n")

cat("========================================\n")



fwrite(

  x,

  output_file

)



cat("\nSaved:\n", output_file, "\n")

cat("\nDONE\n")
