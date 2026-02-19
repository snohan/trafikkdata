# Prepare vbv data for a speed report

{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  options(lubridate.week.start = 1)
  library(writexl)
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  source("trp_info/trp_info.R")

  decimal_point <- function(number) {
    stringr::str_replace(as.character(number), ",", "\\.")
  }
}


# Given a TRP ID and a folder with its vbv data
trp_id_chosen <- "47061V704629"
vbv_folder <- "spesialbestillinger/rasen"

trp_metadata_chosen <- 
  get_trp_metadata_by_list(list(trp_id_chosen)) |> 
  dplyr::select(trp_id, road_link_position, lat, lon) |> 
  dplyr::distinct()

trp_info_chosen <-
  trp_info |> 
  dplyr::filter(
    trp_id == trp_id_chosen
  ) |> 
  dplyr::select(
    trp_id, trp_name, road_reference, lane_internal, lane_according_to_current_metering
  ) |> 
  dplyr::left_join(
    trp_info_lane_direction_names,
    by = dplyr::join_by(trp_id, lane_according_to_current_metering)
  ) |> 
  dplyr::left_join(
    trp_metadata_chosen,
    by = "trp_id"
  )

speed_limit <- get_speedlimit_by_roadlink(trp_metadata_chosen$road_link_position)

data_raw <- 
  purrr::map(
    base::list.files(vbv_folder, full.names = TRUE),
    ~ readr::read_delim(.)
  ) |>
  purrr::list_rbind() |> 
  dplyr::filter(
    traffic_registration_point_id == trp_id_chosen,
    valid_event == TRUE
  ) |> 
  dplyr::select(
    trp_id = traffic_registration_point_id,
    timestamp = event_timestamp, lane,
    speed, valid_speed
  ) |> 
  dplyr::mutate(
    timestamp = lubridate::ymd_hms(timestamp),
    lane = lane / 100,
    speed = decimal_point(speed) |> as.numeric(),
    speed_interval =
      dplyr::case_when(
        speed <  40 ~  "< 40",
        speed <  50 ~  "40-50",
        speed <  60 ~  "50-60",
        speed <  70 ~  "60-70",
        speed <  80 ~  "70-80",
        speed <  90 ~  "80-90",
        speed < 100 ~  "90-100",
        speed < 110 ~ "100-110",
        speed < 120 ~ "110-120",
        TRUE ~ "> 120"
      ) |> factor(levels = c("< 40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "> 120")),
    day = lubridate::date(timestamp),
    day_of_week = lubridate::wday(timestamp, label = TRUE),
    hour_of_day = lubridate::hour(timestamp)
  ) |> 
  dplyr::left_join(
    trp_info_chosen |> 
      dplyr::select(trp_id, lane_internal, direction),
    by = dplyr::join_by(trp_id, lane == lane_internal)
  )

first_timestamp <- base::min(data_raw$timestamp)
last_timestamp  <- base::max(data_raw$timestamp)

aggregate_speed <- function(df, ...) {

  aggregates_df <-
    df |> 
    dplyr::group_by(...) |> 
    dplyr::summarise(
      arithmetic_mean = base::mean(speed),
      percentile_85 = stats::quantile(speed, 0.85),
      percentile_95 = stats::quantile(speed, 0.95),
      n_days = dplyr::n_distinct(day),
      n_vehicles = n(),
      n_above_limit = base::sum(speed > speed_limit),
      perc_above_limit = 100 * n_above_limit / n_vehicles,
      n_above_atk_crit = base::sum(speed > 1.1 * speed_limit),
      perc_above_atk_crit = 100 * n_above_atk_crit / n_above_limit,
      .groups = "drop"
    )
}

aggregates_total <- aggregate_speed(data_raw, valid_speed)

aggregates_weekday <- 
  aggregate_speed(data_raw, valid_speed, day_of_week) |> 
  dplyr::arrange(day_of_week)

aggregates_direction_day_of_week <- 
  aggregate_speed(data_raw, valid_speed, day_of_week, direction) |> 
  dplyr::arrange(day_of_week, direction)

aggregates_direction <- 
  aggregate_speed(data_raw, valid_speed, direction) |> 
  dplyr::arrange(direction)


aggregate_speed_intervals <- function(df, ...) {

  aggregates_df <-
    df |> 
    dplyr::group_by(speed_interval, ...) |> 
    dplyr::summarise(
      n_vehicles = n(),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      percentage = 100 * n_vehicles / base::sum(n_vehicles)
    )

}

speed_intervals <- aggregate_speed_intervals(data_raw)
