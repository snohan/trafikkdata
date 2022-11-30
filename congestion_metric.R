# VbV data from Kibana

library(tidyverse)
library(hms)
base::Sys.setlocale(locale = "nb.utf8")

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
# Literature
# A Survey of Methods and Technologies for Congestion Estimation Based on Multisource Data Fusion
# https://www.mdpi.com/2076-3417/11/5/2306/htm

# https://research.geodan.nl/measuring-traffic-congestion-a-better-way/

# TRP ----
trp <-
  get_points_with_direction() |>
  dplyr::distinct(trp_id, .keep_all = T)


# Read ----
# some_data <-
#   readr::read_csv2(
#     "congestion_data/kanalbrua_2021.csv"
#     #"congestion_data/sundland_4.csv"
#   )

# Read Kibana-exported CSVs ----
read_a_file <- function(file_name) {

  readr::read_csv2(
    paste0("congestion_data/", file_name)
  )

}

# Choose which files to read
filename_root <- "kanalbrua_2021"

# Do read
vbv_data <-
  list.files(
    "congestion_data",
    pattern = paste0("^", filename_root, ".*csv")
  ) |>
  purrr::map_df(
    ~ read_a_file(.)
  )

# Smoothing by 5 in intervals
# Alternative smoothing: exponential average of vbv
aggregated_data <-
  vbv_data |>
  dplyr::rename(
    trp_id = traffic_registration_point_id
  ) |>
  dplyr::filter(
    valid_event == TRUE
  ) |>
  dplyr::mutate(
    event_timestamp =
      lubridate::ymd_hms(
        event_timestamp,
        tz = "CET"
        ),
     timestamp_floored =
      lubridate::floor_date(
        event_timestamp,
        "5 mins"
      ),
    interval_start = hms::as_hms(timestamp_floored),
    date =
      lubridate::date(event_timestamp),
    valid_speed =
      dplyr::if_else(
        valid_speed == TRUE,
        1,
        0,
        0
      ),
    # speed =
    #   dplyr::if_else(
    #     valid_speed == TRUE,
    #     speed,
    #     NA_real_,
    #     NA_real_
    #   )
  ) |>
  dplyr::group_by(
    trp_id,
    timestamp_floored,
    date,
    interval_start,
    lane
  ) |>
  dplyr::summarise(
    volume = n(),
    volume_with_valid_speed = sum(valid_speed),
    mean_speed = mean(speed),
    mean_time_gap = mean(time_gap),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    flow = volume / 5 * 60, # Explicitly using 5 min aggregates
    density = flow / mean_speed
  )


# When is traffic congested?
# Find maximum flow for a longer period, i.e. month.
# Find what mean speed is for that max flow.
# Then congestion occurs in all intervals with less than critical mean speed.
# To avoid single slow vehicles at night being tagged as congested, the
# mean time gap should be less than 5 s.

# TODO:
# Using personal car equivalents?

critical_values <-
  aggregated_data |>
  dplyr::group_by(
    lane
  ) |>
  dplyr::slice_max(
    order_by = flow,
    #n = 15,
    prop = 0.005
  ) |>
  dplyr::summarise(
    max_flow = median(flow),
    critical_speed = median(mean_speed),
    road_capacity = median(density),
    .groups = "drop"
  )

# critical_values_2 <-
#   aggregated_data |>
#   dplyr::group_by(
#     lane
#   ) |>
#   dplyr::summarise(
#     q3_flow = quantile(flow, probs = c(0.75)),
#     q1_flow = quantile(flow, probs = c(0.25)),
#     iqr = q3_flow - q1_flow,
#     max_flow = q3_flow + 1.5 * iqr,
#     #critical_speed = median(mean_speed),
#     #road_capacity = median(density),
#     .groups = "drop"
#   )


data_congested <-
  aggregated_data |>
  dplyr::left_join(
    critical_values,
    by = "lane"
  ) |>
  dplyr::mutate(
    congestion =
      dplyr::case_when(
        mean_speed < critical_speed &
          #mean_time_gap < 5 &
          density >= road_capacity ~ "Ja",
        TRUE ~ "Nei"
      ),
    weekday =
      lubridate::wday(
        date,
        label = TRUE,
        abbr = FALSE,
        week_start = 1
      )
  )

congestion_stats <-
  data_congested |>
  dplyr::group_by(
    trp_id,
    weekday,
    lane
  ) |>
  dplyr::count(
    congestion
  ) |>
  dplyr::ungroup() |>
  tidyr::complete(
    trp_id,
    weekday,
    lane,
    congestion,
    fill = list(n = 0)
  ) |>
  tidyr::pivot_wider(
    names_from = "congestion",
    values_from = "n"
  ) |>
  dplyr::mutate(
    congestion_percentage =
      Ja / (Ja + Nei) * 100
  )

readr::write_rds(
  congestion_stats,
  file =
    paste0("congestion_data/", filename_root, "_stats.rds")
)


find_trp_info_and_direction_names <- function(aggregated_data) {

  # Needs a "trp" df with all TRP info from Trafikkdata API

  trp_here <-
    trp |>
    dplyr::filter(
      trp_id == data_congested$trp_id[1]
    ) |>
    split_road_system_reference()

  trp_info <-
    base::paste0(
      trp_here$road_category_and_number,
      " ",
      stringr::str_to_title(trp_here$name),
      ", ",
      trp_here$municipality_name
    )

  trp_direction_names <-
    trp_here |>
    dplyr::select(
      from,
      to
    ) |>
    tidyr::pivot_longer(
      cols = c(from, to),
      names_to = "trp_direction",
      values_to = "direction_name_to"
    ) |>
    dplyr::mutate(
      direction_name_to = stringr::str_to_title(direction_name_to)
    )

  trp_directions <-
    data_congested |>
    dplyr::distinct(lane) |>
    dplyr::mutate(
      trp_direction = dplyr::if_else(lane %% 2 == 0, "from", "to")
    ) |>
    dplyr::left_join(
      trp_direction_names,
      by = "trp_direction"
    ) |>
    dplyr::mutate(
      name_string = paste0(
        "Felt ",
        lane,
        ": til ",
        direction_name_to
      )
    )

  lane_names <-
    trp_directions$name_string

  names(lane_names) <-
    trp_directions$lane

  result <- list(
    trp_info,
    lane_names
  )

  return(result)
}



# Write ----
find_trp_info_and_direction_names(data_congested) |>
  readr::write_rds(
    file = paste0("congestion_data/", filename_root, "_trp.rds")
  )

list(
  critical_values,
  data_congested
) |>
readr::write_rds(
  file = paste0("congestion_data/", filename_root, ".rds")
)

# Visually verify ----

## Density and flow ----
data_congested |>
  ggplot(
    aes(
      x = density,
      y = flow,
      color = congestion
    )
  ) +
  geom_point() +
  facet_wrap(
    vars(lane)
  ) +
  geom_hline(
    data = critical_values,
    aes(
      yintercept = max_flow
    ),
    color = "red"
  ) +
  geom_vline(
    data = critical_values,
    aes(
      xintercept = road_capacity
    ),
    color = "red"
  )


## Speed and density ----
data_congested |>
  ggplot(
    aes(
      x = density,
      y = mean_speed,
      color = congestion
    )
  ) +
  geom_point() +
  facet_wrap(
    vars(lane)
  ) +
  geom_hline(
    data = critical_values,
    aes(
      yintercept = critical_speed
    ),
    color = "red"
  ) +
  geom_vline(
    data = critical_values,
    aes(
      xintercept = road_capacity
    ),
    color = "red"
  )


## Look at a particular day ----
data_congested |>
  dplyr::filter(
    date == "2022-10-28"
  ) |>
  ggplot(
    aes(
      x = timestamp_floored,
      y = mean_speed,
      color = congestion,
      alpha = density
    )
  ) +
  geom_point() +
  facet_wrap(
    vars(lane)
  ) +
  geom_hline(
    data = critical_values,
    aes(
      yintercept = critical_speed
    ),
    color = "red"
  )


## Timegaps ----
data_congested |>
  dplyr::filter(
    date == "2021-10-29"
  ) |>
  ggplot(
    aes(
      x = timestamp_floored,
      y = mean_time_gap
    )
  ) +
  geom_point() +
  facet_wrap(
    vars(lane)
  )


