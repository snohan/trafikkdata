# Prepare c-vbv data for sharing
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
library(writexl)

points_metadata <- get_points() %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

# Read kibana exported csv file
data_from_csv <- read.csv2("spesialbestillinger/agderpd.csv") %>%
  dplyr::filter(valid_event == "true") %>%
  dplyr::rename(trp_id = traffic_registration_point_id) %>%
  dplyr::left_join(points_metadata) %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                timestamp = event_timestamp, lane, length, speed, vehicle_class = vehicle_type,
                valid_length, valid_speed, valid_class = valid_classification,
                wrong_direction, datalogger_type) %>%
  dplyr::arrange(trp_id, timestamp)

writexl::write_xlsx(data_from_csv, path = "spesialbestillinger/agderpd.xlsx")

