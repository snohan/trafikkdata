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
trp_id_chosen <- "97174V1215671"
vbv_folder <- "spesialbestillinger/rasen"
vegbilde_url <- "https://vegbilder.atlas.vegvesen.no/?year=2025&lat=61.58557224&lng=9.80156209&zoom=16&view=image&imageId=Vegbilder_360_2025.2025-08-21T11.33.30_FV02522_S5D1_m01423_360_2"

source("fartsrapport/fartsrapport_aggreger.R")

