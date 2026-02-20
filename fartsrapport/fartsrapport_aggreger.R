# Aggregate speed data and save to file

# TRP info ----
trp_metadata_chosen <- 
  get_trp_metadata_by_list(list(trp_id_chosen)) |> 
  dplyr::select(
    trp_id, 
    name, road_reference,
    county_name, municipality_name,
    road_link_position, lat, lon
  ) |> 
  dplyr::distinct() |> 
  dplyr::mutate(
    speed_limit = get_speedlimit_by_roadlink(road_link_position),
    vegbilde_url = vegbilde_url
  )

trp_direction_info <-
  trp_info |> 
  dplyr::filter(
    trp_id == trp_id_chosen
  ) |> 
  dplyr::select(
    trp_id, lane_internal, lane_according_to_current_metering
  ) |> 
  dplyr::left_join(
    trp_info_lane_direction_names,
    by = dplyr::join_by(trp_id, lane_according_to_current_metering)
  )


# Speed data ----
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
    trp_direction_info |> 
      dplyr::select(trp_id, lane_internal, direction),
    by = dplyr::join_by(trp_id, lane == lane_internal)
  )

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

aggregates_direction_weekday <- 
  aggregate_speed(data_raw, valid_speed, day_of_week, direction) |> 
  dplyr::arrange(day_of_week, direction)

aggregates_direction <- 
  aggregate_speed(data_raw, valid_speed, direction) |> 
  dplyr::arrange(direction)

aggregates_hour_weekday <-
  aggregate_speed(data_raw, day_of_week, hour_of_day) |> 
  dplyr::mutate(
    n_above_limit_per_day = n_above_limit / n_days
  )


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


# Write ----
## TRP info
trp_metadata <-
  trp_metadata_chosen |> 
  dplyr::mutate(
    first_data = base::min(data_raw$timestamp) |> lubridate::date(),
    last_data  = base::max(data_raw$timestamp) |> lubridate::date()
  )

## Bundle to one file per TRP and analysis period
readr::write_rds(
  list(
    trp = trp_metadata,
    total = aggregates_total,
    weekday = aggregates_weekday,
    direction = aggregates_direction,
    direction_weekday = aggregates_direction_weekday,
    hour_weekday = aggregates_hour_weekday,
    intervals = speed_intervals
  ),
  file = paste0("fartsrapport/tidy/", trp_id_chosen, "_", trp_metadata$first_data , ".rds")
)

