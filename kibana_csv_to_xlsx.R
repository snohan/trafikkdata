# Prepare c-vbv data for sharing


# Setup ----
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
library(readxl)
library(writexl)

# Points ----
points_metadata <- get_points() %>%
  #dplyr::select(trp_id, name, road_reference, county_name, municipality_name) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

points_direction <- get_trps_with_direction() %>%
  dplyr::select(trp_id, metering_direction_changed, lane_parity_kibana, direction)

add_correct_direction_names <- function(data_from_kibana) {
  data_from_kibana %>%
    dplyr::mutate(is_lane_odd = purrr::map(lane, ~ length(is_odd(.)) > 0),
                  lane_parity_kibana = dplyr::case_when(is_lane_odd == TRUE ~ "odd",
                                                        is_lane_odd == FALSE ~ "even")) %>%
    dplyr::left_join(points_direction, by = c("trp_id", "lane_parity_kibana")) %>%
    dplyr::mutate(lane_number = dplyr::case_when(
      metering_direction_changed == TRUE & lane_parity_kibana == "odd" ~ lane + 1,
      metering_direction_changed == TRUE & lane_parity_kibana == "even" ~ lane - 1,
      metering_direction_changed == FALSE & lane_parity_kibana == "odd" ~ lane,
      metering_direction_changed == FALSE & lane_parity_kibana == "even" ~ lane,
    ))
}

# TODO: join direction_names per lane
# Does Kibana give us changed lane numbers when metering has changed? NO!
# So we need to swap lane numbers here if metering has changed.
# No need to swap lanes when using data from the API, but always use lanes according to numbering.
# Always use direction names according to metering.


