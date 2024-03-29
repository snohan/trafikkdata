#
source("H:/Programmering/R/trafikkdata/kibana_csv_to_xlsx.R")

# Read all files in folder
folder_in_focus <- "spesialbestillinger/nordland_pd"

data_from_csv <-
  list.files(path = folder_in_focus, pattern = "*.csv",
             full.names = TRUE) %>%
  map_df(~read_csv2(.))


# Kibana vbv data ----
# Read kibana exported csv file
data_from_csv_enriched <- data_from_csv %>%
  dplyr::filter(valid_event == TRUE) %>%
  dplyr::rename(trp_id = traffic_registration_point_id) %>%
  dplyr::left_join(points_metadata) %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                timestamp = event_timestamp,
                time_gap,
                lane,
                length,
                speed,
                vehicle_class = vehicle_type_raw,
                valid_length,
                valid_speed,
                valid_class = valid_classification,
                wrong_direction,
                datalogger_type
                ) %>%
  dplyr::arrange(trp_id, timestamp) %>%
  add_correct_direction_names() %>%
  # dplyr::mutate(is_lane_odd = purrr::map(lane, ~ length(is_odd(.)) > 0),
  #               lane_parity_kibana = dplyr::case_when(is_lane_odd == TRUE ~ "odd",
  #                                              is_lane_odd == FALSE ~ "even")) %>%
  # dplyr::left_join(points_direction) %>%
  # dplyr::mutate(lane_number = dplyr::case_when(
  #   metering_direction_changed == TRUE & lane_parity_kibana == "odd" ~ lane + 1,
  #   metering_direction_changed == TRUE & lane_parity_kibana == "even" ~ lane - 1,
  #   metering_direction_changed == FALSE & lane_parity_kibana == "odd" ~ lane,
  #   metering_direction_changed == FALSE & lane_parity_kibana == "even" ~ lane,
  # )) %>%
  dplyr::select(-is_lane_odd, -lane_parity_kibana, -metering_direction_changed, -lane) %>%
  dplyr::relocate(direction:lane_number, .before = timestamp)

writexl::write_xlsx(data_from_csv_enriched,
                    path = paste0(folder_in_focus, "/trafikkdata.xlsx"))


# Kibana aggregated data ----
data_from_csv_agg_meta <- data_from_csv_agg %>%
  dplyr::rename(trp_id = punkt) %>%
  dplyr::left_join(points_metadata) %>%
  dplyr::rename(lane = felt) %>%
  add_correct_direction_names() %>%
  # 15 min
  # dplyr::select(
  #   point_name = name,
  #   municipality_name,
  #   road_reference,
  #   lane_number,
  #   direction,
  #   date = dag,
  #   time_period_start = periode_start,
  #   traffic_volume = trafikkmengde
  #   ) %>%
# snittfart og fraktiler
dplyr::select(
  point_name = name,
  municipality_name,
  road_reference,
  lane_number = lane,
  direction,
  date = dato,
  mean_speed = snittfart,
  '5th_percentile' = '5th percentile of speed',
  '15th_percentile' = '15th percentile of speed',
  '50th_percentile' = '50th percentile of speed',
  '85th_percentile' = '85th percentile of speed',
  '95th_percentile' = '95th percentile of speed',
  vehicles_with_valid_speed
) %>%
  dplyr::arrange(road_reference, date,
                 #time_period_start,
                 lane_number)

writexl::write_xlsx(data_from_csv_agg_meta, path = paste0(folder_in_focus, "/varemessa.xlsx"))
