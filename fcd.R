# A look at FCD

{
  library("tidyverse")
}

# FCD penetration rate
# 1. Compare daily traffic at TRPs with 100 % coverage
#   a. Find TRPs that have 100 % coverage
#   b. Find OSM way IDs for each TRP.
#   c. Match this with FCD data fetched in Saga.
# 2. Look for variation in time and space, and find prediction variables


# Will compare only data of good quality
trp_latest_date <- 
  get_trps_latest_data() |> 
  dplyr::filter(
    latest_data_by_hour > "2026-05-01"
  )
