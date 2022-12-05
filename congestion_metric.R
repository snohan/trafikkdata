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

# Estimation of traffic stream space mean speed from time aggregations of double loop detector data

# We should have vbv value of
# Time between activation of loop 1 and 2
# Time that loop 1 was activated

# TRP ----
trp <-
  get_points_with_direction() |>
  dplyr::distinct(trp_id, .keep_all = T)


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

# Smoothing by intervals
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
    valid_speed_values =
      dplyr::if_else(
        valid_speed == 1,
        speed,
        NA_real_,
        NA_real_
      ),
    # Using personal car equivalents (pce) based on length
    pce_length =
      dplyr::case_when(
        valid_length == FALSE ~ 5,
        TRUE ~ length
      ),
    pce = dplyr::case_when(
      pce_length < 7.6 ~ 1,
      pce_length < 12.5 ~ 2,
      pce_length < 16 ~ 3.5,
      TRUE ~ 5
    )
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
    pce_volume = sum(pce),
    #mean_speed = mean(speed),
    space_mean_speed = 1 / mean(1 / valid_speed_values, na.rm = TRUE),
    mean_time_gap = mean(time_gap),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    #flow = volume / 5 * 60, # Explicitly using 5 min aggregates
    pce_flow = pce_volume / 5 * 60,
    #density = flow / mean_speed,
    pce_density = pce_flow / space_mean_speed
  )


# When is traffic congested?
# Find maximum flow for a longer period, i.e. month.
# Find what mean speed is for that max flow.
# Then congestion occurs in all intervals with less than critical mean speed.
# To avoid single slow vehicles at night being tagged as congested, the
# mean time gap should be less than 5 s.

# Here we use time mean speed. Theoretically this should be replaced by
# space mean speed, but this is not measurable. If some cars have higher speeds
# than others,
# these influence time mean speed more than they would the space mean.
# Therefore, using the time mean speed can overestimate the critcal speed,
# and thus underestimate flow and density.

# critical_values <-
#   aggregated_data |>
#   dplyr::group_by(
#     lane
#   ) |>
#   # TODO: polynomial regression (degree 2)
#   # TODO: use stats::optimize to find maximum
#   dplyr::slice_max(
#     order_by = pce_flow,
#     #n = 15,
#     prop = 0.005
#   ) |>
#   dplyr::summarise(
#     #max_flow = median(flow),
#     pce_max_flow = median(pce_flow),
#     #critical_speed = median(space_mean_speed),
#     #road_capacity = median(density),
#     # q = uk
#     pce_road_capacity = min(pce_density),
#     #pce_road_capacity = pce_max_flow / critical_speed,
#     critical_speed = pce_max_flow / pce_road_capacity,
#     .groups = "drop"
#   )

critical_values <-
  aggregated_data |>
  dplyr::group_by(
    lane
  ) |>
  dplyr::filter(
    pce_flow >= max(pce_flow) - 100
  ) |>
  dplyr::summarise(
    #max_flow = median(flow),
    pce_max_flow = min(pce_flow),
    #critical_speed = median(space_mean_speed),
    #road_capacity = median(density),
    # q = uk
    pce_road_capacity = min(pce_density),
    #pce_road_capacity = pce_max_flow / critical_speed,
    critical_speed = pce_max_flow / pce_road_capacity,
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
        space_mean_speed < critical_speed &
          #mean_time_gap < 5 &
          pce_density >= pce_road_capacity ~ "Ja",
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
      x = pce_density,
      y = pce_flow,
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
      yintercept = pce_max_flow
    ),
    color = "red"
  ) +
  geom_vline(
    data = critical_values,
    aes(
      xintercept = pce_road_capacity
    ),
    color = "red"
  )


## Speed and density ----
data_congested |>
  ggplot(
    aes(
      x = pce_density,
      y = space_mean_speed,
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
      xintercept = pce_road_capacity
    ),
    color = "red"
  )


## Look at a particular day ----
data_congested |>
  dplyr::filter(
    date == "2021-09-30"
  ) |>
  ggplot(
    aes(
      x = timestamp_floored,
      y = space_mean_speed,
      color = congestion,
      alpha = pce_density
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


