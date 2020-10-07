# Les inn kontrollerte enkeltpasseringer og lag en komplett aggregert fil

library(tidyverse)
library(lubridate)
options(digits.secs=3)
library(vcd)
# kp_eksempel_damsgaardtunnelen.csv

kp <- readr::read_csv2("kroppanbru.csv") %>%
  dplyr::mutate(event_timestamp = event_timestamp %>%
                  lubridate::ymd_hms(),
                interval_start = lubridate::floor_date(event_timestamp, unit = "5 minutes"))

kp_valid_events <- kp %>%
  dplyr::filter(valid_event == TRUE) %>%
  dplyr::group_by(interval_start, lane) %>%
  dplyr::summarise(valid_events = n())

kp_valid_speed <- kp %>%
  dplyr::filter(valid_speed == TRUE) %>%
  dplyr::group_by(interval_start, lane) %>%
  dplyr::summarise(valid_speed = n())

kp_valid_length <- kp %>%
  dplyr::filter(valid_length == TRUE) %>%
  dplyr::group_by(interval_start, lane) %>%
  dplyr::summarise(valid_length = n())

kp_valid_classification <- kp %>%
  dplyr::filter(valid_classification == TRUE) %>%
  dplyr::group_by(interval_start, lane) %>%
  dplyr::summarise(valid_classification = n())

#kp_direction

kp_length_groups <- kp %>%
  dplyr::filter(valid_length == TRUE) %>%
  dplyr::mutate(length_group = dplyr::case_when(
    length < 5.6 ~ "[..,5.6)",
    length < 7.6 ~ "[5.6,7.6)",
    length < 12.5 ~ "[7.6,12.5)",
    length < 16 ~ "[12.5,16.0)",
    length < 24 ~ "[16.0,24.0)",
    TRUE ~ "[24.0,..)"
  )) %>%
  dplyr::group_by(interval_start, lane, length_group) %>%
  dplyr::summarise(valid_length_group_vehicles = n()) %>%
  tidyr::pivot_wider(names_from = length_group, values_from = valid_length_group_vehicles) %>%
  dplyr::select(interval_start, lane, '[..,5.6)', '[5.6,7.6)', '[7.6,12.5)', '[12.5,16.0)', '[16.0,24.0)', '[24.0,..)')


kp_vehicle_classes <- kp %>%
  dplyr::filter(valid_classification == TRUE) %>%
  dplyr::mutate(vehicle_class = dplyr::case_when(
    vehicle_type == 1 ~ "motorcycle",
    vehicle_type == 2 ~ "car",
    vehicle_type == 3 ~ "car_with_trailer",
    vehicle_type == 4 ~ "van",
    vehicle_type == 5 ~ "van_with_trailer",
    vehicle_type == 6 ~ "bus",
    vehicle_type == 71 ~ "small_truck",
    vehicle_type == 72 ~ "truck",
    vehicle_type == 8 ~ "truck_with_trailer",
    vehicle_type == 9 ~ "truck_with_semitrailer",
    TRUE ~ "class_unknown"
  )) %>%
  dplyr::group_by(interval_start, lane, vehicle_class) %>%
  dplyr::summarise(valid_vehicle_class_vehicles = n()) %>%
  tidyr::pivot_wider(names_from = vehicle_class, values_from = valid_vehicle_class_vehicles) %>%
  #dplyr::mutate(bus = NA,
  #              class_unknown = NA) %>%
  dplyr::select(interval_start, lane,
                motorcycle, car, car_with_trailer, van, van_with_trailer, bus,
               small_truck, truck, truck_with_trailer, truck_with_semitrailer, class_unknown)

kp_speed_values <- kp %>%
  dplyr::filter(valid_speed == TRUE) %>%
  dplyr::group_by(interval_start, lane) %>%
  dplyr::summarise(mean_speed = mean(speed, na.rm = TRUE),
                   percentile_85 = quantile(speed, probs = .85))


aggregated_data <- kp_valid_events %>%
  dplyr::left_join(kp_valid_length) %>%
  dplyr::left_join(kp_valid_speed) %>%
  dplyr::left_join(kp_valid_classification) %>%
  dplyr::left_join(kp_length_groups) %>%
  dplyr::left_join(kp_vehicle_classes) %>%
  dplyr::left_join(kp_speed_values)


write.csv2(aggregated_data, file = "eksempelfil_damsgaardtunnelen.csv",
           row.names = FALSE)

# Sammenheng lengde og klasse

kp_length_and_vehicle_class <- kp %>%
  #dplyr::filter(lane == 4) %>%
  dplyr::filter(valid_length == TRUE,
                valid_classification) %>%
  dplyr::mutate(length_group = dplyr::case_when(
    length < 5.6 ~ "[..,5.6)",
    length < 7.6 ~ "[5.6,7.6)",
    length < 12.5 ~ "[7.6,12.5)",
    length < 16 ~ "[12.5,16.0)",
    length < 24 ~ "[16.0,24.0)",
    TRUE ~ "[24.0,..)"
  )) %>%
  dplyr::mutate(vehicle_class = dplyr::case_when(
    vehicle_type == 1 ~ "motorcycle",
    vehicle_type == 2 ~ "car",
    vehicle_type == 3 ~ "car_with_trailer",
    vehicle_type == 4 ~ "van",
    vehicle_type == 5 ~ "van_with_trailer",
    vehicle_type == 6 ~ "bus",
    vehicle_type == 71 ~ "small_truck",
    vehicle_type == 72 ~ "truck",
    vehicle_type == 8 ~ "truck_with_trailer",
    vehicle_type == 9 ~ "truck_with_semitrailer",
    TRUE ~ "class_unknown"
  )) %>%
  dplyr::select(length_group, vehicle_class)


contingency_table <- table(kp_length_and_vehicle_class$length_group,
                           kp_length_and_vehicle_class$vehicle_class)

vcd::assoc(contingency_table, shade = T, legend = T)
