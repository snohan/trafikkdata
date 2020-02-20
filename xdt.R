# Look at xdt

library(tidyverse)
library(jsonlite)

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")

trp <- getPoints()

# mdt jan 2020
trp_distinct <- trp %>%
  dplyr::filter(!is.na(validFrom)) %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::filter(!str_detect(road_reference, "KD")) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom))

mdt_2020 <- trp_distinct$trp_id %>%
  get_mdt_for_trp_list("2020")

mdt_all <- mdt_2020 %>%
  dplyr::mutate(lane = "all")

mdt_2020_lane <- mdt_2020$trp_id %>%
  get_mdt_by_lane_for_trp_list("2020")

mdt_lane <- mdt_2020_lane %>%
  dplyr::mutate(lane = as.character(lane))

mdt_all_and_lane <-bind_rows(mdt_all,
                             mdt_lane) %>%
  dplyr::mutate(length_quality = round(100 * valid_length_volume / mdt,
                                       digits = 0))

trp_device <- get_trs_device() %>%
  dplyr::select(trp_id, deviceType)

trp_mdt <- trp_distinct %>%
  dplyr::select(trp_id, name, road_reference, county_name) %>%
  dplyr::left_join(trp_device) %>%
  dplyr::right_join(mdt_all_and_lane) %>%
  dplyr::filter(!is.na(mdt))

write.csv2(trp_mdt, file = "trp_mdt_202001.csv", row.names = F)
trp_mdt <- read.csv2("trp_mdt_202001.csv")

trp_mdt_spread <- trp_mdt %>%
  dplyr::filter(lane != "all") %>%
  dplyr::group_by(trp_id, deviceType) %>%
  dplyr::summarise(no_lanes = n(),
                   sd = sd(length_quality))



# trp trøndelag
trp_trondelag <- trp %>%
  dplyr::filter(county_no == 50) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::select(-validFrom, -validTo)

write.csv2(trp_trondelag, file = "trp_trondelag.csv")
