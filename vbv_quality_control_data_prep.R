# Prepare data for the Rmd
source("trafficdata_functions.R")

# LM vbv feb 2020 ----
## Vbv data exported from Kibana

vbv_data_asker <-
  read_kibana_vbv("vbv_data/asker.csv")
vbv_data_soyland <-
  read_kibana_vbv("vbv_data/soyland.csv")
vbv_data_hollundsvegen <-
  read_kibana_vbv("vbv_data/hollundsvegen.csv")
vbv_data_svortland_vest <-
  read_kibana_vbv("vbv_data/svortland_vest.csv")
vbv_data_svortland_ost <-
  read_kibana_vbv("vbv_data/svortland_ost.csv")
vbv_postterminalen <-
  read_kibana_vbv("vbv_data/postterminalen.csv")
vbv_elgeseter <-
  read_kibana_vbv("vbv_data/elgeseter_bru.csv")

## Elgeseter bru 24.02.2020: mye snø og neppe noen faktiske mc eller skuter
vbv_elgeseter_2 <-
  read_kibana_vbv("vbv_data/elgeseter_bru_20200224.csv")
vbv_moholtlia <-
  read_kibana_vbv("vbv_data/moholtlia.csv")
vbv_f05 <-
  read_kibana_vbv("vbv_data/oysand_f05.csv")
vbv_skullerud <-
  read_kibana_vbv("vbv_data/skullerud.csv")

# EMU3 vbv aug 2021
vbv_strandgata <-
  read_kibana_vbv("vbv_data/strandgata.csv")
vbv_somaveien <-
  read_kibana_vbv("vbv_data/e39_somaveien.csv")
vbv_festningsgata <-
  read_kibana_vbv("vbv_data/festningsgata.csv")
vbv_ovre_sund_bru <-
  read_kibana_vbv("vbv_data/ovre_sund_bru.csv")
vbv_tastatorget  <-
  read_kibana_vbv("vbv_data/tastatorget.csv")

# Øysand June 2021 ----
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
  dplyr::mutate(
    datalogger_type = dplyr::case_when(
      datalogger_type == "LOOP_MONITOR" ~ "LM",
      datalogger_type == "EMU" ~ "EMU3",
      TRUE ~ "UNKNOWN"
    ),
    trp_name = stringr::str_to_upper(trp_name),
    name_and_datalogger = paste0(datalogger_type, "_", trp_name),
    lane_number = lane,
    lane = paste0(lane, " ", name_and_datalogger),
    event_timestamp = with_tz(ymd_hms(event_timestamp, tz = "CET")),
    weekday = lubridate::wday(event_timestamp,
                              label = TRUE,
                              abbr = FALSE)
    ) %>%
  make_norsikt_classes() %>%
  make_length_classes()


oysand_2021w23_agg_length_class_full <- oysand_2021w23 %>%
  dplyr::group_by(name_and_datalogger, weekday, length_class_full, lane_number) %>%
  dplyr::summarise(volume = n())

oysand_2021w23_agg_length_class_2 <- oysand_2021w23 %>%
  dplyr::group_by(name_and_datalogger, weekday, length_class_2, lane_number) %>%
  dplyr::summarise(volume = n())

oysand_2021w23_agg_length_class_2_heavy_ratio <- oysand_2021w23_agg_length_class_2 %>%
  dplyr::mutate(length_class_2 = if_else(length_class_2 == "[..,5.6)", "light", "heavy")) %>%
  tidyr::pivot_wider(names_from = length_class_2, values_from = volume) %>%
  dplyr::mutate(heavy_ratio = round(100 * heavy / (light + heavy), digits = 1))

data_interval <-
  lubridate::interval(
    start = floor_date(min(oysand_2021w23$event_timestamp), unit = "hour"),
    end = ceiling_date(max(oysand_2021w23$event_timestamp), unit = "hour"))

# Malins vbv ----
EMU3_F05 <- read_excelsheet("vbv_data/oysand_comparisons/emu3_unknown_length.xlsx", 1)
EMU3_F07 <- read_excelsheet("vbv_data/oysand_comparisons/emu3_unknown_length.xlsx", 2)

