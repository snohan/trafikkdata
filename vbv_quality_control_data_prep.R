# Prepare data for the Rmd
source("trafficdata_functions.R")
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")

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
vbv_finnevatnet <-
  read_kibana_vbv("vbv_data/finnevatnet.csv")
vbv_kyrkseterora <-
  read_kibana_vbv("vbv_data/kyrkseterora.csv")
vbv_stranda <-
  read_kibana_vbv("vbv_data/stranda.csv")

vbv_berkaak_emu_gml_fw  <-
  read_kibana_vbv("vbv_data/berkaak_emu_p534s2150.csv") %>%
  dplyr::mutate(weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                name_and_datalogger = "EMU P5.3.4/S2.15.0") %>%
  make_length_classes()

vbv_berkaak_emu_ny_fw  <-
  read_kibana_vbv("vbv_data/berkaak_emu_102.csv") %>%
  dplyr::mutate(weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                name_and_datalogger = "EMU 1.02") %>%
  make_length_classes()

vbv_berkaak_lm  <-
  read_kibana_vbv("vbv_data/berkaak_lm.csv") %>%
  dplyr::mutate(weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                 name_and_datalogger = "LM") %>%
  make_length_classes()

vbv_berkaak_emu  <-
  read_kibana_vbv("vbv_data/berkaak_emu.csv") %>%
  dplyr::mutate(weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                name_and_datalogger = "EMU") %>%
  make_length_classes()

vbv_brekktunnelen_lm  <-
  read_kibana_vbv("vbv_data/brekktunnelen_lm.csv") %>%
  dplyr::mutate(weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                name_and_datalogger = "LM") %>%
  make_length_classes()

vbv_brekktunnelen_emu  <-
  read_kibana_vbv("vbv_data/brekktunnelen_emu.csv") %>%
  dplyr::mutate(weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                name_and_datalogger = "EMU") %>%
  make_length_classes()


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

oysand_2021w23_nl2_hr <- oysand_2021w23 %>%
  dplyr::select(name_and_datalogger, weekday, norsikt_l2, lane = lane_number) %>%
  calculate_heavy_ratio_from_vbv_by_class()

data_interval <-
  lubridate::interval(
    start = floor_date(min(oysand_2021w23$event_timestamp), unit = "hour"),
    end = ceiling_date(max(oysand_2021w23$event_timestamp), unit = "hour"))

# Malins vbv ----
EMU3_F05 <- read_excelsheet("vbv_data/oysand_comparisons/emu3_unknown_length.xlsx", 1)
EMU3_F07 <- read_excelsheet("vbv_data/oysand_comparisons/emu3_unknown_length.xlsx", 2)

# F05
EMU3_F05_LM_data <- EMU3_F05 %>%
  dplyr::select(lane, length = length_LM, vehicle_type_raw = vehicle_type_raw_LM) %>%
  dplyr::mutate(name_and_datalogger = "LM_F09",
                vehicle_type_raw = as.character(vehicle_type_raw))

EMU3_F05_EMU3_data <- EMU3_F05 %>%
  dplyr::select(lane, length, vehicle_type_raw = vehicle_type_raw_EMU3) %>%
  dplyr::mutate(name_and_datalogger = "EMU3_F05",
                vehicle_type_raw = as.character(vehicle_type_raw))

EMU3_F05_all_data <- dplyr::bind_rows(EMU3_F05_LM_data, EMU3_F05_EMU3_data) %>%
  dplyr::mutate(lane = paste0("felt ", lane),
                lane = factor(lane, levels = lane_order),
                weekday = "mandag") %>%
  make_length_classes() %>%
  make_norsikt_classes()

EMU3_F05_length_hr <- EMU3_F05_all_data %>%
  calculate_heavy_ratio_from_vbv_by_length()

EMU3_F05_class_hr <- EMU3_F05_all_data %>%
  calculate_heavy_ratio_from_vbv_by_class()

EMU3_F05_all_classifications <- EMU3_F05 %>%
  dplyr::select(lane, length_LM, length_EMU3 = length,
                vehicle_type_raw_LM, vehicle_type_raw = vehicle_type_raw_EMU3,
                length_diff,
                length_class_2_EMU3 = length_class_2,
                length_class_full_EMU3 = length_class_full) %>%
  make_norsikt_classes() %>%
  dplyr::rename(norsikt_l2_EMU3 = norsikt_l2,
                norsikt_l3_EMU3 = norsikt_l3,
                vehicle_type_raw_EMU3 = vehicle_type_raw,
                length = length_LM) %>%
  make_length_classes() %>%
  dplyr::rename(length_class_2_LM = length_class_2,
                length_class_full_LM = length_class_full,
                length_LM = length,
                vehicle_type_raw = vehicle_type_raw_LM) %>%
  make_norsikt_classes() %>%
  dplyr::rename(norsikt_l2_LM = norsikt_l2,
                norsikt_l3_LM = norsikt_l3,
                vehicle_type_raw_LM = vehicle_type_raw)

EMU3_F05_different_length_class_longer <- EMU3_F05_all_classifications %>%
  dplyr::filter(length_EMU3 < 27,
                length_class_2_LM == "[..,5.6)" & length_class_2_EMU3 == "[5.6,..)")

EMU3_F05_different_length_class_shorter <- EMU3_F05_all_classifications %>%
  dplyr::filter(length_EMU3 < 27,
                length_class_2_EMU3 == "[..,5.6)" & length_class_2_LM == "[5.6,..)")

EMU3_F05_under_3 <- EMU3_F05_all_classifications %>%
  dplyr::filter(length_EMU3 < 3 | length_LM < 3)


# F07
EMU3_F07_LM_data <- EMU3_F07 %>%
  dplyr::select(lane, length = length_LM, vehicle_type_raw = vehicle_type_raw_LM) %>%
  dplyr::mutate(name_and_datalogger = "LM_F09",
                vehicle_type_raw = as.character(vehicle_type_raw))

EMU3_F07_EMU3_data <- EMU3_F07 %>%
  dplyr::select(lane, length, vehicle_type_raw = vehicle_type_raw_EMU3) %>%
  dplyr::mutate(name_and_datalogger = "EMU3_F07",
                vehicle_type_raw = as.character(vehicle_type_raw))

EMU3_F07_length_hr <- dplyr::bind_rows(EMU3_F07_LM_data, EMU3_F07_EMU3_data) %>%
  dplyr::mutate(lane = paste0("felt ", lane),
                lane = factor(lane, levels = lane_order),
                weekday = "mandag") %>%
  make_length_classes() %>%
  calculate_heavy_ratio_from_vbv_by_length()

EMU3_F07_class_hr <- dplyr::bind_rows(EMU3_F07_LM_data, EMU3_F07_EMU3_data) %>%
  dplyr::mutate(lane = paste0("felt ", lane),
                lane = factor(lane, levels = lane_order),
                weekday = "mandag") %>%
  make_norsikt_classes() %>%
  calculate_heavy_ratio_from_vbv_by_class()

EMU3_F07_all_classifications <- EMU3_F07 %>%
  dplyr::select(lane, length_LM, length_EMU3 = length,
                vehicle_type_raw_LM, vehicle_type_raw = vehicle_type_raw_EMU3,
                length_diff,
                length_class_2_EMU3 = length_class_2,
                length_class_full_EMU3 = length_class_full) %>%
  make_norsikt_classes() %>%
  dplyr::rename(norsikt_l2_EMU3 = norsikt_l2,
                norsikt_l3_EMU3 = norsikt_l3,
                vehicle_type_raw_EMU3 = vehicle_type_raw,
                length = length_LM) %>%
  make_length_classes() %>%
  dplyr::rename(length_class_2_LM = length_class_2,
                length_class_full_LM = length_class_full,
                length_LM = length,
                vehicle_type_raw = vehicle_type_raw_LM) %>%
  make_norsikt_classes() %>%
  dplyr::rename(norsikt_l2_LM = norsikt_l2,
                norsikt_l3_LM = norsikt_l3,
                vehicle_type_raw_LM = vehicle_type_raw)

EMU3_F07_different_length_class_longer <- EMU3_F07_all_classifications %>%
  dplyr::filter(length_EMU3 < 27,
                length_class_2_LM == "[..,5.6)" & length_class_2_EMU3 == "[5.6,..)")

EMU3_F07_different_length_class_shorter <- EMU3_F07_all_classifications %>%
  dplyr::filter(length_EMU3 < 27,
                length_class_2_EMU3 == "[..,5.6)" & length_class_2_LM == "[5.6,..)")

EMU3_F07_under_3 <- EMU3_F07_all_classifications %>%
  dplyr::filter(length_EMU3 < 3 | length_LM < 3)




# Heavy ratios ----
berkaak_fw_hr_by_length <- dplyr::bind_rows(vbv_berkaak_emu_gml_fw, vbv_berkaak_emu_ny_fw) %>%
  calculate_heavy_ratio_from_vbv_by_length()

berkaak_fw_hr_by_class <- dplyr::bind_rows(vbv_berkaak_emu_gml_fw, vbv_berkaak_emu_ny_fw) %>%
  calculate_heavy_ratio_from_vbv_by_class()

#
berkaak_hr_by_length <- dplyr::bind_rows(vbv_berkaak_lm, vbv_berkaak_emu) %>%
  calculate_heavy_ratio_from_vbv_by_length()

berkaak_hr_by_class <- dplyr::bind_rows(vbv_berkaak_lm, vbv_berkaak_emu) %>%
  calculate_heavy_ratio_from_vbv_by_class()

brekktunnelen_hr_by_length <- dplyr::bind_rows(vbv_brekktunnelen_lm, vbv_brekktunnelen_emu) %>%
  calculate_heavy_ratio_from_vbv_by_length()

brekktunnelen_hr_by_class <- dplyr::bind_rows(vbv_brekktunnelen_lm, vbv_brekktunnelen_emu) %>%
  calculate_heavy_ratio_from_vbv_by_class()
