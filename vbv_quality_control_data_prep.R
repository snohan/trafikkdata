#

trp_oysand <- tibble::tibble(trp_id = c("23531V72241",
                                        "23679V72241",
                                        "23827V72241"),
                             trp_name = c("f05",
                                          "f07",
                                          "f09"))

oysand_2021w23_raw <- read_kibana_vbv("vbv_data/oysand_kp_2021w23.csv")

oysand_2021w23 <- oysand_2021w23_raw %>%
  dplyr::select(trp_id = traffic_registration_point_id,
                datalogger_type, firmware_version,
                dato, event_timestamp,
                lane, length, speed, vehicle_type_raw, qspeed,
                valid_event, valid_length, valid_speed, valid_classification) %>%
  dplyr::filter(valid_event == TRUE) %>%
  dplyr::left_join(trp_oysand, by = "trp_id") %>%
  dplyr::mutate(name_and_datalogger = paste0(datalogger_type, "_", trp_name),
                event_timestamp = with_tz(ymd_hms(event_timestamp, tz = "CET")),
                weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                lane = paste0("felt ", lane)) %>%
  make_norsikt_classes() %>%
  make_length_classes()


oysand_2021w23_agg_length_class_full <- oysand_2021w23 %>%
  dplyr::group_by(name_and_datalogger, weekday, length_class_full, lane) %>%
  dplyr::summarise(volume = n())

oysand_2021w23_agg_length_class_2 <- oysand_2021w23 %>%
  dplyr::group_by(name_and_datalogger, weekday, length_class_2, lane) %>%
  dplyr::summarise(volume = n())


data_interval <-
  lubridate::interval(
    start = floor_date(min(oysand_2021w23$event_timestamp), unit = "hour"),
    end = ceiling_date(max(oysand_2021w23$event_timestamp), unit = "hour"))
