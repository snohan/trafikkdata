# Prepare vbv data for a speed report

{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  options(lubridate.week.start = 1)
  library(writexl)
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  source("trp_info/trp_info.R")

  decimal_point <- function(number) {
    stringr::str_replace(as.character(number), ",", "\\.")
  }
}


# Given a TRP ID and a folder with its vbv data
trp_id_chosen <- "47061V704629"
vbv_folder <- "spesialbestillinger/rasen"
vegbilde_url <- "https://vegbilder.atlas.vegvesen.no?lat=60.52524326&lng=11.50873199&view=image&zoom=15&year=2025&imageId=Vegbilder_360_2025.2025-06-18T15.04.12_FV00024_S6D1_m04094_360_1"

source("fartsrapport_aggreger.R")

