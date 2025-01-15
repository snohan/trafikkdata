# Calculate AADT for toll stations

# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
  library(jsonlite)
  library(lubridate)
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  library(writexl)
}


# Toll station meta data ----
toll_stations <-
  get_all_tolling_stations() |>
  dplyr::mutate(
    # Get rid of leading zeros
    nvdb_id = as.numeric(nvdb_id) |> as.character()
  )

toll_stations_selected <-
  toll_stations |>
  split_road_system_reference() |>
  dplyr::select(
    nvdb_id,
    operator_id,
    toll_station_id,
    toll_station_name,
    directions,
    road_category,
    road_reference,
    road_link_position
  ) |>
  dplyr::arrange(
    operator_id,
    as.numeric(toll_station_id)
  )


# 2023: Some toll stations have same ID, but measures traffic on different traffic links
# These must manually be mapped by lane to the correct traffic link
# 2024: no problem, they are distinguished by nvdb_id
# same_toll_station_id <-
#   toll_stations_selected |>
#   dplyr::summarise(
#     n = n(),
#     .by = c(operator_id, toll_station_id)
#   ) |>
#   dplyr::filter(
#     n > 1
#   )


# Toll station data ----
# Only yearly data is feasible to fetch with both class and direction (export limit from Power BI).
# This will be used to calculate heavy percentage.
# Will use daily traffic to calculate AADT and SE, see below.
yearly <-
  readr::read_csv(
    "toll/toll_station_yearly_2024.csv"
  ) |>
  dplyr::select(
    nvdb_id = 'NVDB ID',
    direction_text = 'toll station direction',
    class = 'vehicle class ID',
    traffic = 'Accepted passages'
  ) |>
  dplyr::filter(
    class %in% c(1, 2),
    !(nvdb_id == 0)
  ) |>
  dplyr::mutate(
    class = dplyr::case_when(
      class == 1 ~ "light",
      class == 2 ~ "heavy"
    ),
    # Get rid of leading zeros
    nvdb_id = as.numeric(nvdb_id) |> as.character(),
    # Need to merge "directions" that actually are lanes, see below for finding these.
    direction_text =
      dplyr::case_when(
        nvdb_id %in% c("1013962143", "141140381", "287649715", "760759643", "82443541", "82559833", "82559836", "906727246") ~ "merged_lanes",
        TRUE ~ direction_text
      )
  ) |>
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, direction_text, class)
  ) |>
  tidyr::pivot_wider(
    names_from = class,
    names_prefix = "traffic_",
    values_from = traffic
  ) |>
  dplyr::mutate(
    heavy_percentage = (100 * traffic_heavy / (traffic_light + traffic_heavy)) |> round(2)
  )

# 2023
# yearly_tidy <-
#   yearly |>
#   dplyr::select(
#     operator_id = 'operator ID',
#     toll_station_id = 'toll station code',
#     lane = 'toll station lane',
#     class = 'vehicle class ID',
#     year,
#     traffic = 'Accepted passages'
#   ) |>
#   dplyr::filter(
#     class %in% c(1, 2)
#   ) |>
#   dplyr::mutate(
#     operator_id = as.character(operator_id),
#     toll_station_id = as.character(toll_station_id)
#   )


## Coverage ----
# Using daily aggregates to calculate coverage and look for anomalies
daily <-
  base::list.files(
    "toll",
    "daily_2024",
    full.names = TRUE
  ) |>
  purrr::map_df(
    ~ readr::read_csv(.)
  )

daily_tidy <-
  daily |>
  dplyr::select(
    nvdb_id = 'NVDB ID',
    direction_text = 'toll station direction',
    month = 'month no.',
    day = 'day no. of year',
    #operator_id = 'operator ID',
    #toll_station_id = 'toll station code',
    traffic = 'Accepted passages'
  ) |>
  dplyr::filter(
    !(nvdb_id == 0)
  ) |>
  dplyr::mutate(
    # Get rid of leading zeros
    nvdb_id = as.numeric(nvdb_id) |> as.character(),
    # Need to merge "directions" that actually are lanes, see below for finding these.
    direction_text =
      dplyr::case_when(
        nvdb_id %in% c("1013962143", "141140381", "287649715", "760759643", "82443541", "82559833", "82559836", "906727246") ~ "merged_lanes",
        TRUE ~ direction_text
      )
  ) |>
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, direction_text, month, day)
  ) |>
  dplyr::mutate(
    median = median(traffic) |> round(-1),
    sd = sd(traffic) |> round(-1),
    .by = c(nvdb_id, direction_text)
  ) |>
  dplyr::mutate(
    outlier =
      dplyr::case_when(
        median > 300 & traffic < (0.1 * median) ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>
  dplyr::arrange(
    nvdb_id,
    direction_text,
    month,
    day
  )

# Which stations have anomalies, and how should they all be filtered?
daily_stats <-
  daily |>
  dplyr::select(
    nvdb_id = 'NVDB ID',
    direction_text = 'toll station direction',
    month = 'month no.',
    day = 'day no. of year',
    #operator_id = 'operator ID',
    #toll_station_id = 'toll station code',
    traffic = 'Accepted passages'
  ) |>
  dplyr::filter(
    !(nvdb_id == 0)
  ) |>
  dplyr::mutate(
    # Get rid of leading zeros
    nvdb_id = as.numeric(nvdb_id) |> as.character()
    #operator_id = as.character(operator_id),
    #toll_station_id = as.character(toll_station_id)
  ) |>
  dplyr::summarise(
    n_days = n(),
    sd = sd(traffic),
    min_day = min(traffic),
    max_day = max(traffic),
    mean = mean(traffic) |> round(-1),
    median = median(traffic) |> round(-1),
    min_day_to_median = (100 * min_day / median) |> round(2),
    #mean_median_diff = (1 - exp(log(mean) - log(median))) * 100,
    #.by = c(operator_id, toll_station_id)
    .by = c(nvdb_id, direction_text)
  ) |>
  dplyr::filter(
    # Remove opposite directions not intended to be registered
    median > 10
  ) |>
  dplyr::arrange(
    nvdb_id,
    direction_text
  ) |>
  # Need to look at some deviating values.
  # Assumption: for any station, most values are correct, so need only find the most deviating days
  # Assumption: low median have naturally high deviations in terms of percentages
  dplyr::filter(
    median > 300,
    min_day_to_median < 10
  )

# Finally, total AADT and SE
total_aadt <-
  daily_tidy |>
  dplyr::filter(
    outlier == FALSE
    # Only 300 of 200 000 removed!
  ) |>
  dplyr::summarise(
    n_days = n(),
    aadt = mean(traffic) |> round(-1),
    sd = sd(traffic) |> round(0),
    .by = c(nvdb_id, direction_text)
  ) |>
  dplyr::mutate(
    se = (sd / sqrt(n_days) * sqrt((366 - n_days) / (366 - 1))) |> round(0)
  ) |>
  dplyr::filter(
    aadt > 0
  )


# Toll station AADT ----
# 2023:
# aadt <-
#   yearly_tidy |>
#   dplyr::summarise(
#     traffic = sum(traffic),
#     .by = c(operator_id, toll_station_id, class)
#   ) |>
#   dplyr::left_join(
#     daily_tidy,
#     by = dplyr::join_by(operator_id, toll_station_id)
#   ) |>
#   dplyr::mutate(
#     aadt = (traffic / n_days) |> round(-1)
#   ) |>
#   dplyr::select(
#     operator_id,
#     toll_station_id,
#     class,
#     n_days,
#     aadt
#   ) |>
#   tidyr::pivot_wider(
#     names_from = "class",
#     names_prefix = "aadt_class_",
#     values_from = "aadt"
#   ) |>
#   dplyr::mutate(
#     aadt_total = aadt_class_1 + aadt_class_2,
#     heavy_ratio = ((aadt_class_2 / aadt_total) * 100) |> round(),
#     coverage = ((n_days / 365) * 100) |> round()
#   ) |>
#   dplyr::select(
#     -aadt_class_1,
#     -aadt_class_2,
#     -n_days
#   )
#
# toll_stations_aadt <-
#   toll_stations_single_link |>
#   dplyr::left_join(
#     aadt,
#     by = dplyr::join_by(operator_id, toll_station_id)
#   )

toll_station_aadt <-
  total_aadt |>
  dplyr::left_join(
    yearly,
    by = dplyr::join_by(
      nvdb_id, direction_text
    )
  ) |>
  dplyr::left_join(
    toll_stations_selected,
    by = dplyr::join_by(nvdb_id)
  ) |>
  dplyr::filter(
    # Remove Ryfast, labelled both directions, but data is per lane. Have TRPs anyway, so do not need them.
    # 100014 800
    # 100014 801
    !(operator_id == "100014" & toll_station_id %in% c("800", "801"))
  ) |>
  dplyr::select(
    nvdb_id,
    #operator_id,
    #toll_station_id,
    toll_station_name,
    road_reference,
    directions,
    direction_text,
    #n_days,
    aadt,
    #sd,
    se,
    heavy_percentage
  ) |>
  dplyr::mutate(
    total_aadt = sum(aadt),
    .by = nvdb_id
  ) |>
  dplyr::mutate(
    direction_percentage = (100 * aadt / total_aadt) |> round()
  ) |>
  # Removing "opposite" directions and specials
  dplyr::filter(
    direction_percentage >= 10
  ) |>
  dplyr::mutate(
    n_directions = n(),
    .by = nvdb_id
  ) |>
  dplyr::mutate(
    # Which "both directions"-stations should we specify the correct direction for?
    direction_imbalanced =
      dplyr::case_when(
        n_directions > 1 & total_aadt > 300 & direction_percentage < 40 ~ TRUE,
        n_directions > 1 & total_aadt > 300 & direction_percentage > 60 ~ TRUE,
        TRUE ~ FALSE
      ),
    per_lane =
      dplyr::case_when(
        directions != "Begge retninger" & n_directions > 1 ~ TRUE,
        TRUE ~ FALSE
      ),
    # per_lane_b =
    #   dplyr::case_when(
    #     directions == "Begge retninger" & n_directions > 2 ~ TRUE,
    #     TRUE ~ FALSE
    #   )
    # None of these
    directions =
      dplyr::case_when(
        # Filling in missing directions
        nvdb_id == "1016036098" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "1016036098" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "1016292474" & direction_text == "Mot Os" ~ "Med metrering",
        nvdb_id == "1016292474" & direction_text == "Mot Bergen" ~ "Mot metrering",
        nvdb_id == "1019360640" & direction_text == "To Molde" ~ "Med metrering",
        nvdb_id == "1019360640" & direction_text == "To Trondheim" ~ "Mot metrering",
        nvdb_id == "1019360641" & direction_text == "To Molde" ~ "Med metrering",
        nvdb_id == "1019360641" & direction_text == "To Trondheim" ~ "Mot metrering",
        nvdb_id == "710608427" & direction_text == "Northbound" ~ "Med metrering",
        nvdb_id == "710608427" & direction_text == "Southbound" ~ "Mot metrering",
        nvdb_id == "906727254" & direction_text == "South" ~ "Mot metrering",
        nvdb_id == "906727252" & direction_text == "East" ~ "Mot metrering",
        # Labelling correct direction for imbalanced station directions
        nvdb_id == "487458622" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "487458622" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790711" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "929790711" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790716" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "929790716" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790721" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "929790721" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790725" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "929790725" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790733" & direction_text == "From city centre" ~ "Med metrering", # should be opposite?
        nvdb_id == "929790733" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790752" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "929790752" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790762" & direction_text == "From city centre" ~ "Med metrering",
        nvdb_id == "929790762" & direction_text == "To city centre" ~ "Mot metrering",
        nvdb_id == "929790772" & direction_text == "From city centre" ~ "Mot metrering",
        nvdb_id == "929790772" & direction_text == "To city centre" ~ "Med metrering",
        TRUE ~ directions
      )
  )

# Some data are per lane, despite being labelled as per direction - merged!

# Done: label correct direction for imbalanced station directions. (look at map and interpret direction description)

# TODO: ?merge remaining "both directions"-stations to total AADT. This will be halfed in matching with links. Do not alter SE.
# TODO: if some are labelled "both directions" but have only one-way traffic, ? look up lane numbers in nvdb?

toll_station_aadt |>
  writexl::write_xlsx(
    "toll/toll_station_aadt_2024.xlsx"
  )


# JSON ----
toll_station_aadt |>
  dplyr::select(
    nvdb_id,
    directions,
    aadt,
    se,
    heavy_percentage
  ) |>
  jsonlite::write_json(
    path = "toll/toll_station_aadt_2024.json",
    prettify = TRUE
  )
