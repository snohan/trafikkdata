# Calculate AADT for toll stations

# Setup ----
{
  source("H:/Programmering/R/byindeks/rmd_setup.R")
  source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  library(writexl)
  #library(outliers)
}


# Toll station data ----
# Using yearly aggregates to calculate AADT
yearly <-
  readr::read_csv(
    "toll/yearly.csv"
  )

yearly_tidy <-
  yearly |>
  dplyr::select(
    operator_id = 'operator ID',
    toll_station_id = 'toll station code',
    lane = 'toll station lane',
    class = 'vehicle class ID',
    year,
    traffic = 'Accepted passages'
  ) |>
  dplyr::filter(
    class %in% c(1, 2)
  ) |>
  dplyr::mutate(
    operator_id = as.character(operator_id),
    toll_station_id = as.character(toll_station_id)
  )

# Using daily aggregates to calculate coverage and look for anomalies
daily <-
  base::list.files(
    "toll",
    "daily",
    full.names = TRUE
  ) |>
  purrr::map_df(
    ~ readr::read_csv(.)
  )

daily_tidy <-
  daily |>
  dplyr::select(
    year,
    month = 'month no.',
    day = 'day no. of year',
    operator_id = 'operator ID',
    toll_station_id = 'toll station code',
    traffic = 'Accepted passages'
  ) |>
  dplyr::mutate(
    operator_id = as.character(operator_id),
    toll_station_id = as.character(toll_station_id)
  ) |>
  dplyr::summarise(
    n_days = n(),
    min_day = min(traffic),
    max_day = max(traffic),
    mean = mean(traffic) |> round(-1),
    median = median(traffic) |> round(-1),
    mean_median_diff = (1 - exp(log(mean) - log(median))) * 100,
    .by = c(operator_id, toll_station_id)
  )
# There may be some days being outliers, but it doesn't seem to be a big issue.


# Toll station meta data ----
toll_stations <-
  get_all_tolling_stations()

toll_stations_selected <-
  toll_stations |>
  split_road_system_reference() |>
  dplyr::select(
    operator_id,
    toll_station_id,
    toll_station_name,
    directions,
    road_category,
    road_reference,
    road_link_position
  ) |>
  dplyr::filter(
    road_category %in% c("E", "R", "F"),
    !is.na(toll_station_id)
  ) |>
  dplyr::arrange(
    operator_id,
    as.numeric(toll_station_id)
  )

# Some toll stations have same ID, but measures traffic on different traffic links
# These must manually be mapped by lane to the correct traffic link
same_toll_station_id <-
  toll_stations_selected |>
  dplyr::summarise(
    n = n(),
    .by = c(operator_id, toll_station_id)
  ) |>
  dplyr::filter(
    n > 1
  )

toll_stations_single_link <-
  toll_stations_selected |>
  dplyr::anti_join(
    same_toll_station_id,
    by = dplyr::join_by(operator_id, toll_station_id)
  )


# Toll station AADT ----
aadt <-
  yearly_tidy |>
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(operator_id, toll_station_id, class)
  ) |>
  dplyr::left_join(
    daily_tidy,
    by = dplyr::join_by(operator_id, toll_station_id)
  ) |>
  dplyr::mutate(
    aadt = (traffic / n_days) |> round(-1)
  ) |>
  dplyr::select(
    operator_id,
    toll_station_id,
    class,
    n_days,
    aadt
  ) |>
  tidyr::pivot_wider(
    names_from = "class",
    names_prefix = "aadt_class_",
    values_from = "aadt"
  ) |>
  dplyr::mutate(
    aadt_total = aadt_class_1 + aadt_class_2,
    heavy_ratio = ((aadt_class_2 / aadt_total) * 100) |> round(),
    coverage = ((n_days / 365) * 100) |> round()
  ) |>
  dplyr::select(
    -aadt_class_1,
    -aadt_class_2,
    -n_days
  )

toll_stations_aadt <-
  toll_stations_single_link |>
  dplyr::left_join(
    aadt,
    by = dplyr::join_by(operator_id, toll_station_id)
  )

toll_stations_aadt |>
  writexl::write_xlsx(
    "toll/toll_station_aadt_2023.xlsx"
  )
