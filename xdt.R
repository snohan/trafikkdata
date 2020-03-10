# Look at xdt

library(tidyverse)
library(jsonlite)

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/get_from_trp_api.R")

trp <- getPoints()

trp_distinct <- trp %>%
  dplyr::filter(!is.na(validFrom)) %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::filter(!str_detect(road_reference, "KD")) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom))

mdt_2020 <- trp_distinct$trp_id %>%
  get_mdt_for_trp_list("2020")

mdt_all <- mdt_2020 %>%
  dplyr::mutate(lane = "all") %>%
  dplyr::mutate(length_quality = round(100 * valid_length_volume / mdt,
                                       digits = 0))

# mdt_2020_lane <- mdt_2020$trp_id %>%
#   get_mdt_by_lane_for_trp_list("2020")
#
# mdt_lane <- mdt_2020_lane %>%
#   dplyr::mutate(lane = as.character(lane))
#
# mdt_all_and_lane <-bind_rows(mdt_all,
#                              mdt_lane) %>%
#   dplyr::mutate(length_quality = round(100 * valid_length_volume / mdt,
#                                        digits = 0))

trp_device <- get_trs_device() %>%
  dplyr::select(trp_id, trs_id, deviceType)

trp_mdt <- trp_distinct %>%
  dplyr::select(trp_id, name, road_reference, county_name) %>%
  dplyr::left_join(trp_device) %>%
  #dplyr::right_join(mdt_all_and_lane) %>%
  dplyr::right_join(mdt_all) %>%
  dplyr::filter(!is.na(mdt))

write.csv2(trp_mdt, file = "trp_mdt_2020-02.csv", row.names = F)
trp_mdt <- read.csv2("trp_mdt_2020-02.csv")

# Looking at the city index trps
city_trp_raw <-
  read_csv2("H:/Programmering/R/byindeks/data_points_raw/cities_points.csv",
            locale = readr::locale(encoding = "latin1"))

city_trp <- city_trp_raw %>%
  select(trp_id) %>%
  unique()

trp_distinct_all <- trp %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom))

city_trp_def <- city_trp_raw %>%
  left_join(trp_distinct_all, by = c("trp_id")) %>%
  select(city_area_name, agreement_start, legacyNortrafMpn,
         name, road_reference, validFrom, kommentar)

city_mdt <- trp_mdt %>%
  ungroup() %>%
  right_join(city_trp) %>%
  filter(!is.na(name)) %>%
  select(trs_id, name, county_name, deviceType, month, coverage, mdt, length_quality) %>%
  arrange(length_quality) %>%
  filter(deviceType == "LOOP_MONITOR")

write.csv2(city_mdt, file = "city_mdt_2020-02.csv", row.names = F)


# city
trp_distinct_city <- trp %>%
  #dplyr::filter(!is.na(validFrom)) %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  #dplyr::filter(!str_detect(road_reference, "KD")) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom))

city_trp_all <- city_trp_raw %>%
  left_join(trp_distinct_city) %>%
  select(city_area_name, name, road_reference, kommentar) %>%
  filter(kommentar != "virtuelt")

# TRPs with differing quality on its lanes
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
