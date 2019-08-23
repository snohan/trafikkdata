# Points and speed

# Packages ####
library(tidyverse)
library(jsonlite)

# Source files ####
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")

trps <- getPoints()
# TODO: filter periodic

trp_periodic_vehicle <- getPointsFromTRPAPI_filtered()

trps_first <- trps %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::group_by(trp_id) %>%
  dplyr::summarise(start_data = min(validFrom))

# Initial attempt at querying NVDB. 500 at a time should be ok now.
trps_vehicle <- trps %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::select(-road_network_position, -road_network_link) %>%
  dplyr::filter(!(trp_id %in% c("68557V444232", "06510V72810", "19393V72810",
                                "68101V930654"))) %>%
  # To fartsgrenseobjekter ved Blommenholm Neiden tollstasjon!
  # Edit: Spørringa takler dette nå.
  # Storlersbakken er nedlagt og uten gyldig vegreferanse
  dplyr::slice(1:500) %>%
  dplyr::mutate(speed_limit = mapply(getSpeedLimit_roadlink, road_link_position))

trp_speed_1751_n <- trps_vehicle

# De med manglende responser legger vi til manuelt
trp_missing <- trps %>%
  dplyr::select(-road_network_position, -road_network_link) %>%
  dplyr::filter(trp_id %in% c("68557V444232", "68101V930654")) %>%
  dplyr::mutate(speed_limit = "80")

trp_speed <- bind_rows(
  trp_speed_1_100,
  trp_speed_101_200,
  trp_speed_201_300,
  trp_speed_301_400,
  trp_speed_401_500,
  trp_speed_501_600,
  trp_speed_601_650,
  trp_speed_650_675,
  trp_speed_676_700,
  trp_speed_701_800,
  trp_speed_801_851,
  trp_speed_851_1000,
  trp_speed_1001_1250,
  trp_speed_1251_1500,
  trp_speed_1501_1750,
  trp_speed_1751_n,
  trp_missing
)

write.csv2(trp_speed, file = "punkter_med_fartsgrense.csv")

trp_speed_80 <- trp_speed %>%
  dplyr::filter(speed_limit == 80) %>%
  dplyr::left_join(trps_first) %>%
  dplyr::mutate(start_data = ymd(start_data)) %>%
  dplyr::filter(start_data < "2018-01-01") %>%
  dplyr::filter(!(trp_id %in% trp_periodic_vehicle$trp_id)) %>%
  dplyr::select(-road_link_position)

write.csv2(trp_speed_80, file = "punkter_med_fartsgrense_80.csv")


