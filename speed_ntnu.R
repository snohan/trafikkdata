# Speed data for Karina Lilleborge, NTNU

# Hourly, but just 7-8 and 19-20
# Mondays in October and Wednesdays in November 2023

# Total traffic
# TRPs in Trondheim

{
  base::Sys.setlocale(locale = "nb.utf8")
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  library(writexl)
}

speed_raw <-
  base::list.files(
    "kibanadata",
    pattern = "speed_ntnu",
    full.names = TRUE
  ) |>
  purrr::map_df(
    ~ readr::read_delim(
      .,
      locale = readr::locale(decimal_mark = ",")
    )
  ) |>
  dplyr::rename(
    sd_u = 'Upper sd',
    sd_l = 'Lower sd'
  ) |>
  dplyr::mutate(
    standard_deviation = (sd_u - sd_l) / 2,
    hour_start = lubridate::hour(date_hour)
  ) |>
  dplyr::select(
    -sd_u,
    -sd_l
  ) |>
  dplyr::filter(
    hour_start %in% c(7, 19)
  )

# Did Kibana fool me by not including all data?
speed_raw |>
  dplyr::summarise(
    n = n(),
    .by = trp_id
  )

valid_speed_ratio <-
  speed_raw |>
  dplyr::select(
    date_hour,
    trp_id,
    valid_speed,
    traffic_volume
  ) |>
  tidyr::pivot_wider(
    names_from = valid_speed,
    names_prefix = "valid_speed_",
    values_from = traffic_volume,
    values_fill = 0
  ) |>
  dplyr::mutate(
    valid_speed_ratio = valid_speed_TRUE / (valid_speed_TRUE + valid_speed_FALSE)
  ) |>
  dplyr::select(
    date_hour,
    trp_id,
    valid_speed_ratio
  )

speed_tidy <-
  speed_raw |>
  dplyr::filter(
    valid_speed == TRUE
  ) |>
  dplyr::left_join(
    valid_speed_ratio,
    by = join_by(date_hour, trp_id)
  ) |>
  dplyr::mutate(
    date = lubridate::date(date_hour),
    month = lubridate::month(date_hour, label = TRUE, abbr = FALSE),
    weekday = lubridate::wday(date_hour, label = TRUE, abbr = FALSE)
  ) |>
  dplyr::filter(
    month == "oktober" & weekday == "mandag" |
      month == "november" & weekday == "onsdag"
  ) |>
  dplyr::select(
    trp_id,
    date,
    hour_start,
    month,
    weekday,
    mean_speed,
    standard_deviation,
    traffic_volume,
    valid_speed_ratio
  ) |>
  dplyr::arrange(
    hour_start,
    date,
    trp_id
  )

trps <-
  base::unique(speed_raw$trp_id) |>
  get_trp_metadata_by_list() |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    lat, lon
  ) |>
  dplyr::distinct()

list(
  "gjennomsnittsfart" = speed_tidy,
  "punkter" = trps
) |>
  writexl::write_xlsx(
    "spesialbestillinger/speed_ntnu.xlsx"
  )
