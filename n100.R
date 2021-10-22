# Wrangle vbv data for N100

# Setup ----
source("H:/Programmering/R/trafikkdata/kibana_csv_to_xlsx.R")

# Read trp list ----
# Data from a list of trps, written to Excelfile
trps <- readxl::read_xlsx("spesialbestillinger/n100/punkter.xlsx") %>%
  dplyr::filter(is.na(ekskludert))

# Check coverage ----
# Must choose one Wednesday in May or June, or September 2021 or 2020
# Which day do most trps have good coverage?
dt_coverage_2021_05_05 <-
  get_dt_for_trp_list(trps$trp_id,
                      "2021-05-05T00:00:00+02:00",
                      "2021-05-06T00:00:00+02:00") %>%
  dplyr::right_join(trps, by = c("point_id" = "trp_id")) %>%
  dplyr::select(trp_id = point_id, name, from, coverage) %>%
  tidyr::replace_na(list(from = "2021-05-05", coverage = 0))

dt_coverage_2021_05_19 <-
  get_dt_for_trp_list(trps$trp_id,
                      "2021-05-19T00:00:00+02:00",
                      "2021-05-20T00:00:00+02:00") %>%
  dplyr::right_join(trps, by = c("point_id" = "trp_id")) %>%
  dplyr::select(trp_id = point_id, name, from, coverage) %>%
  tidyr::replace_na(list(from = "2021-05-19", coverage = 0))

dt_coverage_2021_05_26 <-
  get_dt_for_trp_list(trps$trp_id,
                      "2021-05-26T00:00:00+02:00",
                      "2021-05-27T00:00:00+02:00") %>%
  dplyr::right_join(trps, by = c("point_id" = "trp_id")) %>%
  dplyr::select(trp_id = point_id, name, from, coverage) %>%
  tidyr::replace_na(list(from = "2021-05-26", coverage = 0))

dt_coverage_2021_06_02 <-
  get_dt_for_trp_list(trps$trp_id,
                      "2021-06-02T00:00:00+02:00",
                      "2021-06-03T00:00:00+02:00") %>%
  dplyr::right_join(trps, by = c("point_id" = "trp_id")) %>%
  dplyr::select(trp_id = point_id, name, from, coverage) %>%
  tidyr::replace_na(list(from = "2021-06-02", coverage = 0))

dt_coverage_2021_06_09 <-
  get_dt_for_trp_list(trps$trp_id,
                      "2021-06-09T00:00:00+02:00",
                      "2021-06-10T00:00:00+02:00") %>%
  dplyr::right_join(trps, by = c("point_id" = "trp_id")) %>%
  dplyr::select(trp_id = point_id, name, from, coverage) %>%
  tidyr::replace_na(list(from = "2021-06-09", coverage = 0))


dt_coverage <- dplyr::bind_rows(
  dt_coverage_2021_05_05,
  dt_coverage_2021_05_19,
  dt_coverage_2021_05_26,
  dt_coverage_2021_06_02,
  dt_coverage_2021_06_09
  ) %>%
  dplyr::filter(coverage < 95) %>%
  tidyr::pivot_wider(names_from = "from",
                     names_prefix = "coverage_",
                     values_from = "coverage")

dt_coverage %>%
  writexl::write_xlsx(path = "spesialbestillinger/n100/punkter_med_mangler.xlsx")


# Read and wrangle Kibana csv files ----
# Read all files in folder
folder_in_focus <- "spesialbestillinger/n100/data_raw"

data_from_csv <-
  list.files(path = folder_in_focus, pattern = "*.csv",
             full.names = TRUE) %>%
  map_df(~read_csv2(.))

data_from_csv_meta <- data_from_csv %>%
  dplyr::mutate(event_timestamp = event_timestamp %>%
                  lubridate::ymd_hms() %>%
                  hms::as_hms()) %>%
  dplyr::rename(trp_id = traffic_registration_point_id) %>%
  dplyr::left_join(points_metadata, by = "trp_id") %>%
  add_correct_direction_names() %>%
  dplyr::select(event_timestamp, trp_id, name, road_reference,
                direction, lane_number, length, speed,
                valid_length, valid_speed)


# Write one file per trp
write_xlsx_per_trp <- function(data_meta) {

  trps_with_data <- data_meta %>%
    dplyr::select(trp_id) %>%
    dplyr::distinct()

  for (n in 1:nrow(trps_with_data)) {

    trp_id_n <- trps_with_data$trp_id[n]
    trp_id_n_filepath <- paste0("spesialbestillinger/n100/data_tidy/",
                                trp_id_n,
                                ".xlsx")

    data_meta %>%
      dplyr::filter(trp_id == trp_id_n) %>%
      writexl::write_xlsx(path = trp_id_n_filepath)
  }
}

data_from_csv_meta %>%
  write_xlsx_per_trp()




