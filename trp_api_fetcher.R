# Fetch useful info from TRP-API and store locally
# Remember to update cookie-data before running

source("H:/Programmering/R/byindeks/get_from_trp_api.R")


# TRS info ----
{
  trs_id <- get_trs_info_simple()

  readr::write_rds(
    trs_id,
    "trs_trp/trs.rds"
  )
}


# TRP info ----
{
  trp_info <- get_trp_direction_reference()

  readr::write_rds(
    trp_info,
    "trs_trp/trp.rds"
  )
}


# TRS and TRP
{
  trs_trp_ids <- get_trs_and_trp_id()

  readr::write_rds(
    trs_trp_ids,
    "trs_trp/trs_trp_ids.rds"
  )

}