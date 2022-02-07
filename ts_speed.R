# TS speed data

# 1. Read chosen trp_id
# 2. TRP meta data
# 3. Get speed limit from NVDB (check history?)
# 4. Get MDT and coverages from Traffic Data API
# 5. Aggregate mean speed, 85th percentile and
#    n vehicles in intervals above speed limit in Kibana

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")

# TRPs ----
trp <- get_points()

distinct_trps <- trp %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    road_category,
    road_category_and_number,
    road_reference,
    county_name,
    municipality_name,
    registration_frequency,
    operational_status,
    lane_numbers,
    road_link_position
  ) %>%
  dplyr::distinct(
    trp_id,
    .keep_all = T
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    lanes = sort(lane_numbers) %>%
      paste(collapse = ", ")
  ) %>%
  ungroup() %>%
  dplyr::select(
    -lane_numbers
  )

ts_chosen_trps <-
  readr::read_csv2(
    "ts/ts_chosen_trp_id.csv"
  )

ts_trp <-
  ts_chosen_trps %>%
  dplyr::left_join(
    distinct_trps
  ) %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no"),
    speed_limit =
      mapply(
        get_speedlimit_by_roadlink,
        road_link_position
      )
  )

ts_trp_speed_limit <-
  ts_trp %>%
  dplyr::select(
    trp_id,
    speed_limit
  )

# AADT ----
aadts <-
  get_aadt_for_trp_list(
    ts_chosen_trps$trp_id
  )

aadts_chosen <-
  aadts %>%
  dplyr::filter(
    year %in% c(2020, 2021)
  )

aadts_chosen_2021 <-
  aadts_chosen %>%
  dplyr::filter(
    year == 2021
  ) %>%
  dplyr::select(
    trp_id,
    adt_2021 = adt
  )

aadts_chosen_coverage <-
  aadts_chosen %>%
  dplyr::select(
    trp_id,
    year,
    coverage
  ) %>%
  dplyr::mutate(
    coverage = round(coverage)
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "coverage_",
    values_from = coverage
  )

ts_trp_aadt <-
  ts_trp %>%
  dplyr::left_join(
    aadts_chosen_2021,
    by = "trp_id"
  ) %>%
  dplyr::left_join(
    aadts_chosen_coverage,
    by = "trp_id"
  )

writexl::write_xlsx(
  ts_trp_aadt,
  path = "ts/ts_trp.xlsx"
)

ts_trp_aadt <-
  readxl::read_excel(
    path = "ts/ts_trp.xlsx"
  )

# TODO: speed limit history, but this is not complete in NVDB!
# Check speed quality in Kibana (not available in the API) - looks ok

# Read Kibana-exported CSVs ----
read_a_file <- function(file_name) {

  readr::read_csv2(
    paste0("ts_data_2022/", file_name)
  ) %>%
  dplyr::mutate(
    periode_start = as.character(periode_start)
  )

}


# Prepare speed intervals ----
speed_intervals <-
  list.files(
    "ts_data_2022",
    pattern = "^Fartsintervaller*") %>%
  purrr::map_df(
    ~ read_a_file(.)
  ) %>%
  dplyr::mutate(
    # datoformatet er ulikt om nedlastingen er gjort via Edge eller Chrome,
    # fordi Edge åpner det i Excel og da skjer det noe skjult!
    periode_start =
      lubridate::parse_date_time(
        periode_start,
        orders = c("dmy", "ymd")
      ),
    year = lubridate::year(periode_start),
    month_name =
      lubridate::month(
        periode_start,
        label = TRUE,
        abbr = TRUE
      ) %>%
      stringr::str_to_title(),
    upper_bound = stringr::str_extract(fartsintervall, "to \\d+") %>%
      stringr::str_sub(4, -1) %>%
      as.numeric()
  ) %>%
  dplyr::select(
    trp_id = punkt,
    year,
    month = month_name,
    n_vehicles = antall_med_godkjent_fart,
    upper_bound
  ) %>%
  dplyr::left_join(
    ts_trp_speed_limit,
    by = "trp_id"
  ) %>%
  dplyr::group_by(
    trp_id,
    year,
    month
  ) %>%
  dplyr::summarise(
    n_vehicles_above_speed_limit_by_0 =
      sum(n_vehicles[upper_bound > speed_limit]),
    n_vehicles_above_speed_limit_by_5 =
      sum(n_vehicles[upper_bound > speed_limit + 5]),
    n_vehicles_above_speed_limit_by_10 =
      sum(n_vehicles[upper_bound > speed_limit + 10]),
    n_vehicles_above_speed_limit_by_15 =
      sum(n_vehicles[upper_bound > speed_limit + 15]),
    n_vehicles_above_speed_limit_by_20 =
      sum(n_vehicles[upper_bound > speed_limit + 20]),
    n_vehicles_above_speed_limit_by_25 =
      sum(n_vehicles[upper_bound > speed_limit + 25]),
    n_vehicles_above_speed_limit_by_30 =
      sum(n_vehicles[upper_bound > speed_limit + 30]),
    n_vehicles_above_speed_limit_by_40 =
      sum(n_vehicles[upper_bound > speed_limit + 40]),
    n_vehicles_above_speed_limit_by_50 =
      sum(n_vehicles[upper_bound > speed_limit + 50]),
    n_vehicles_above_speed_limit_by_60 =
      sum(n_vehicles[upper_bound > speed_limit + 60]),
    n_vehicles_above_speed_limit_by_70 =
      sum(n_vehicles[upper_bound > speed_limit + 70])
  ) %>%
  tidyr::pivot_longer(
    cols = starts_with("n_vehicles"),
    names_to = "measurand",
    values_to = "value"
  ) %>%
  dplyr::arrange(
    year,
    factor(month,
           levels = c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun",
                      "Jul", "Aug", "Sep", "Okt", "Nov", "Des"))
  ) %>%
  tidyr::pivot_wider(
    names_from = month,
    values_from = value,
    values_fill = NA
  )


# Prepare means and percentiles ----
means_and_percentiles <-
  list.files(
    "ts_data_2022",
    pattern = "^Snitt*") %>%
  purrr::map_df(
    ~ read_a_file(.)
  ) %>%
  dplyr::mutate(
    periode_start =
      lubridate::parse_date_time(
        periode_start,
        orders = c("dmy", "ymd")
      ),
    year = lubridate::year(periode_start),
    month_name =
      lubridate::month(
        periode_start,
        label = TRUE,
        abbr = TRUE
      ) %>%
      stringr::str_to_title(),
    length_class =
      dplyr::case_when(
        length_class == "1,0 to 27,0" ~ "alle",
        length_class == "1,0 to 5,6" ~ "lette",
        length_class == "5,6 to 27,0" ~ "tunge"
      )
  ) %>%
  dplyr::select(
    trp_id = punkt,
    year,
    month = month_name,
    length_class,
    n_vehicles = vehicles_with_valid_speed,
    snittfart,
    fraktil_85 = "85th percentile of speed"
  ) %>%
  tidyr::pivot_longer(
    cols = c(n_vehicles, snittfart, fraktil_85),
    names_to = "measurand"
  ) %>%
  tidyr::pivot_wider(
    names_from = month,
    values_from = value,
    values_fill = NA
  ) %>%
  dplyr::mutate(
    measurand = paste0(measurand, "_", length_class)
  ) %>%
  dplyr::select(
    -length_class
  )


# Bind and add trp info ----
trp_speed_info <-
  dplyr::bind_rows(
    means_and_percentiles,
    speed_intervals
  ) %>%
  dplyr::left_join(
    ts_trp_aadt,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road = road_category_and_number,
    municipality = municipality_name,
    adt_2021,
    speed_limit,
    lanes,
    measurand,
    year,
    Jan:Des
  ) %>%
  dplyr::arrange(
    speed_limit,
    name,
    year
  )

writexl::write_xlsx(
  trp_speed_info,
  path = "ts/trp_speed_monthly.xlsx"
)







