# SHK speed data

# 1. TRP metadata.
# 2. Read Kibana vbv CSV.
# 3. Check for deviations in daily data.
# 4. Aggregate monthly mean speed, median, 85th percentile and n vehicles in intervals.

{
  base::Sys.setlocale(locale = "nb.utf8")
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  library(writexl)
}

add_vehicle_type_names <- function(df) {

  df |>
    dplyr::mutate(
      vehicle_type =
        dplyr::case_when(
          vehicle_type_raw == "1" ~ "Motorsykkel",
          vehicle_type_raw == "2" ~ "Personbil",
          vehicle_type_raw == "3" ~ "Varebil",
          vehicle_type_raw == "4" ~ "Personbil med henger",
          vehicle_type_raw == "5" ~ "Varebil med henger",
          vehicle_type_raw == "6" ~ "Buss",
          vehicle_type_raw == "7" ~ "Lastebil",
          vehicle_type_raw == "71" ~ "Liten lastebil",
          vehicle_type_raw == "72" ~ "Stor lastebil",
          vehicle_type_raw == "8" ~ "Lastebil med henger",
          vehicle_type_raw == "9" ~ "Lastebil med semitrailer",
          vehicle_type_raw == "above_7.6" ~ "Alle over 7,6 m"
        )
    )

}

speed_interval_levels <-
  c(
    "less_than_70",
    "from_70_75",
    "from_75_80",
    "from_80_85",
    "from_85_90",
    "from_90_"
  )

# TRP metadata ----
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

trp_directions <- get_trps_with_direction()

trp_chosen_shk <-
  distinct_trps |>
  dplyr::filter(
    trp_id %in% c("23527V1125822")
  ) |>
  dplyr::inner_join(
    trp_directions,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    direction,
    metering_direction_changed,
    lane_parity_api,
    lane_parity_kibana
  ) |>
  dplyr::mutate(
    with_trp_direction =
      dplyr::case_when(
        lane_parity_kibana == "odd" ~ TRUE,
        lane_parity_kibana == "even" ~ FALSE
      ),
    traffic_heading_with_metering =
      dplyr::case_when(
        lane_parity_kibana == "odd" & metering_direction_changed == FALSE ~ TRUE,
        lane_parity_kibana == "odd" & metering_direction_changed == TRUE ~ FALSE,
        lane_parity_kibana == "even" & metering_direction_changed == FALSE ~ FALSE,
        lane_parity_kibana == "even" & metering_direction_changed == TRUE ~ TRUE
      )
  )

# Read Kibana CSV ----
# TODO: add vehicle driving direction (traffic heading) according to metering
read_a_file <- function(file_name) {

  readr::read_csv2(
    paste0("vbv_data/", file_name)
  )

}

# TODO: should have included data logger type to convert vehicle_type_raw to readable names
vbv_data <-
  list.files(
    "vbv_data",
    pattern = "^vollstad*") |>
  purrr::map_df(
    ~ read_a_file(.)
  )

vbv_data_tidy <-
  vbv_data |>
  dplyr::filter(
    valid_length == TRUE,
    valid_speed == TRUE,
    valid_classification == TRUE,
    vehicle_type_raw > 2
  ) |>
  dplyr::mutate(
    event_timestamp = lubridate::as_datetime(event_timestamp),
    month = lubridate::floor_date(event_timestamp, "month") |> lubridate::as_date(),
    with_trp_direction =
      dplyr::case_when(
        with_traffic_registration_point_direction == TRUE & wrong_direction == FALSE ~ TRUE,
        with_traffic_registration_point_direction == TRUE & wrong_direction == TRUE ~ FALSE,
        with_traffic_registration_point_direction == FALSE & wrong_direction == FALSE ~ FALSE,
        with_traffic_registration_point_direction == FALSE & wrong_direction == TRUE ~ TRUE
      ),
    vehicle_type_raw = dplyr::case_when(
      vehicle_type_raw == 71 ~ 7,
      vehicle_type_raw == 72 ~ 7,
      TRUE ~ vehicle_type_raw
      ) |> as.character(),
    speed_interval = dplyr::case_when(
      speed < 70 ~ speed_interval_levels[1],
      speed < 75 ~ speed_interval_levels[2],
      speed < 80 ~ speed_interval_levels[3],
      speed < 85 ~ speed_interval_levels[4],
      speed < 90 ~ speed_interval_levels[5],
      TRUE ~ speed_interval_levels[6]
    ) |> factor(levels = speed_interval_levels)
  ) |>
  dplyr::rename(
    trp_id = traffic_registration_point_id
  ) |>
  dplyr::left_join(
    trp_chosen_shk,
    by = c("trp_id", "with_trp_direction")
  ) |>
  dplyr::select(
    trp_id,
    event_timestamp,
    month,
    traffic_heading_with_metering,
    length,
    speed,
    vehicle_type_raw,
    speed_interval
  )


# Check ----
# Finding deviations per day
#daily_data <-
  vbv_data_tidy |>
  dplyr::mutate(
    day = lubridate::floor_date(event_timestamp, "day") |> lubridate::as_date()
  ) |>
  dplyr::filter(
    month == "2022-05-01"
  ) |>
  ggplot2::ggplot(aes(day, speed, group = day)) +
  geom_boxplot()

vbv_data_tidy |>
  dplyr::mutate(
    hour = lubridate::floor_date(event_timestamp, "hour") |> lubridate::as_datetime(),
    hour_value = lubridate::hour(hour),
    day = lubridate::floor_date(event_timestamp, "day") |> lubridate::as_date()
  ) |>
  dplyr::filter(
    #day == "2022-09-07",
    day >= "2022-09-01",
    day <= "2022-09-05",
    #hour_value %in% seq.int(8,20),
    #traffic_heading_with_metering == TRUE
    traffic_heading_with_metering == FALSE
  ) |>
  ggplot2::ggplot(aes(hour, speed, group = hour)) +
  geom_boxplot()


# Aggregate ----
monthly_data_stats <-
  dplyr::bind_rows(
    dplyr::summarise(
      vbv_data_tidy,
      n_measured_vehicles = n(),
      mean_speed = mean(speed),
      standard_deviation_speed = sd(speed),
      median_speed = median(speed),
      percentile_15_speed = quantile(speed, 0.15),
      percentile_85_speed = quantile(speed, 0.85),
      .by = c(
        trp_id,
        traffic_heading_with_metering,
        month,
        vehicle_type_raw
      )
    ),
    dplyr::summarise(
      vbv_data_tidy,
      n_measured_vehicles = n(),
      mean_speed = mean(speed),
      standard_deviation_speed = sd(speed),
      median_speed = median(speed),
      percentile_15_speed = quantile(speed, 0.15),
      percentile_85_speed = quantile(speed, 0.85),
      .by = c(
        trp_id,
        traffic_heading_with_metering,
        month
      )
    ) |>
      dplyr::mutate(
        vehicle_type_raw = "above_7.6"
      )
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(mean_speed:percentile_85_speed),
      .fns = ~ round(.x, 1)
    )
  )

monthly_data_intervals <-
  dplyr::bind_rows(
    dplyr::summarise(
      vbv_data_tidy,
      n_vehicles = n(),
      .by = c(
        trp_id,
        traffic_heading_with_metering,
        month,
        vehicle_type_raw,
        speed_interval
      )
    ) |>
      tidyr::pivot_wider(
        names_from = speed_interval,
        values_from = n_vehicles
      ),
    dplyr::summarise(
      vbv_data_tidy,
      n_vehicles = n(),
      .by = c(
        trp_id,
        traffic_heading_with_metering,
        month,
        speed_interval
      )
    ) |>
      tidyr::pivot_wider(
        names_from = speed_interval,
        values_from = n_vehicles
      )|>
      dplyr::mutate(
        vehicle_type_raw = "above_7.6"
      )
  )

monthly_data <-
  dplyr::left_join(
    monthly_data_stats,
    monthly_data_intervals,
    by = c(
      "trp_id",
      "traffic_heading_with_metering",
      "month",
      "vehicle_type_raw"
    )
  ) |>
  add_vehicle_type_names() |>
  dplyr::left_join(
    trp_chosen_shk,
    by = c("trp_id", "traffic_heading_with_metering")
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    direction,
    month,
    vehicle_type,
    n_measured_vehicles:from_90_
  )


# Write ----
writexl::write_xlsx(
  monthly_data,
  "spesialbestillinger/vollstad.xlsx"
)
