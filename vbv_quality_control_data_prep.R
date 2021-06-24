#

trp_oysand <- tibble::tibble(trp_id = c("23531V72241",
                                        "23679V72241",
                                        "23827V72241"),
                             trp_name = c("f05",
                                          "f07",
                                          "f09"))

oysand_2021w23 <- read_kibana_vbv("vbv_data/oysand_kp_2021w23.csv") %>%
  dplyr::select(trp_id = traffic_registration_point_id,
                datalogger_type, firmware_version,
                dato, event_timestamp,
                lane, length, speed, vehicle_type, qspeed,
                valid_event, valid_length, valid_speed, valid_classification) %>%
  dplyr::filter(valid_event == TRUE) %>%
  dplyr::left_join(trp_oysand, by = "trp_id") %>%
  dplyr::mutate(name_and_datalogger = paste0(datalogger_type, "_", trp_name),
                event_timestamp = with_tz(ymd_hms(event_timestamp),
                                          tzone = "CET"))

data_interval <-
  lubridate::interval(
    start = floor_date(min(oysand_2021w23$event_timestamp), unit = "hour"),
    end = ceiling_date(max(oysand_2021w23$event_timestamp), unit = "hour"))
