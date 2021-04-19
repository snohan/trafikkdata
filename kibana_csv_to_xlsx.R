# Prepare c-vbv data for sharing
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
library(writexl)

points_metadata <- get_points() %>%
  #dplyr::select(trp_id, name, road_reference, county_name, municipality_name) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

points_direction <- get_trps_with_direction() %>%
  dplyr::select(trp_id, metering_direction_changed, lane_parity_kibana, direction)

# TODO: join direction_names per lane
# Does Kibana give us changed lane numbers when metering has changed? NO!
# So we need to swap lane numbers here if metering has changed.
# No need to swap lanes when using data from the API, but always use lanes according to numbering.
# Always use direction names according to metering.

# Read kibana exported csv file
data_from_csv <- read.csv2("spesialbestillinger/agderpd2.csv") %>%
  dplyr::filter(valid_event == "true") %>%
  dplyr::rename(trp_id = traffic_registration_point_id) %>%
  dplyr::left_join(points_metadata) %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                timestamp = event_timestamp, time_gap, lane, length, speed, vehicle_class = vehicle_type_raw,
                valid_length, valid_speed, valid_class = valid_classification,
                wrong_direction, datalogger_type) %>%
  dplyr::arrange(trp_id, timestamp) %>%
  dplyr::mutate(is_lane_odd = purrr::map(lane, ~ length(is_odd(.)) > 0),
                lane_parity_kibana = dplyr::case_when(is_lane_odd == TRUE ~ "odd",
                                               is_lane_odd == FALSE ~ "even")) %>%
  dplyr::left_join(points_direction) %>%
  dplyr::mutate(lane_number = dplyr::case_when(
    metering_direction_changed == TRUE & lane_parity_kibana == "odd" ~ lane + 1,
    metering_direction_changed == TRUE & lane_parity_kibana == "even" ~ lane - 1,
    metering_direction_changed == FALSE & lane_parity_kibana == "odd" ~ lane,
    metering_direction_changed == FALSE & lane_parity_kibana == "even" ~ lane,
  )) %>%
  dplyr::select(-is_lane_odd, -lane_parity_kibana, -metering_direction_changed, -lane) %>%
  dplyr::relocate(direction:lane_number, .before = timestamp)

writexl::write_xlsx(data_from_csv, path = "spesialbestillinger/agderpd2.xlsx")

