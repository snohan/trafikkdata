# Speed data for VegDim ----

# Speed values:
# Mean
# Standard deviation
# 75th percentile

# Per
# Month
# Light and heavy vehicle class (length)
# Lane
# Direction
# Total cross section of road

{
  base::Sys.setlocale(locale = "nb.utf8")
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  library(writexl)
}

# TRPs ----
chosen_trps <- c(
  "57808V2282277",
  "94187V1916131",
  "79196V443434",
  "14051V2499835",
  "09750V705194",
  "00509V885112",
  "72936V3118172",
  "29431V3118173",
  "47985V2491853"
)

# Reference direction and current metering: Need to handle direction names and lane numbers accordingly
# Fetch "metering_direction_changed". If TRUE, then change parity of Kibana lane numbers.
# 1. Flip lane numbers from Kibana, if metering_direction_changed is TRUE.
# 2. Join direction text which is according to current metering

trp_directions <- get_trps_with_direction()

trp_info <-
  get_trp_metadata_by_list(chosen_trps) |>
  dplyr::select(
    trp_id,
    name,
    from,
    to,
    road_reference
  ) |>
  dplyr::distinct() |>
  dplyr::inner_join(
    trp_directions,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    metering_direction_changed
  ) |>
  dplyr::distinct()

trp_info_direction <-
  get_trp_metadata_by_list(chosen_trps) |>
  dplyr::select(
    trp_id,
    name,
    from,
    to,
    road_reference
  ) |>
  dplyr::distinct() |>
  dplyr::inner_join(
    trp_directions,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    direction,
    lane_parity = lane_parity_api
  )


# Kibana CSVs ----
read_a_file <- function(file_name) {

  readr::read_delim(
    paste0("vegdim/", file_name),
    locale = readr::locale(decimal_mark = ",")
  )

}

# TODO: If vehicle class: should have included data logger type to convert vehicle_type_raw to readable names
vegdim_data <-
  list.files("vegdim") |>
  purrr::map_df(
    ~ read_a_file(.)
  ) |>
  dplyr::select(-min_speed) |>
  dplyr::rename(
    sd_u = 'Upper Standard Deviation of speed',
    sd_l = 'Lower Standard Deviation of speed'
  ) |>
  dplyr::mutate(
    # dplyr::across(
    #   where(is.numeric),
    #   ~ .x / 100
    # ),
    lane_parity = dplyr::if_else(lane %% 2 == 0, "even", "odd"),
    length_range = dplyr::case_when(
      stringr::str_detect(length_range, "< 5") ~ "lette",
      TRUE ~ "tunge"
    ),
    standard_deviation = (sd_u - sd_l) / 2
  ) |>
  dplyr::left_join(
    trp_info,
    by = join_by(trp_id)
  ) |>
  dplyr::mutate(
    lane = dplyr::case_when(
      metering_direction_changed == TRUE & lane_parity == "odd" ~ lane + 1,
      metering_direction_changed == TRUE & lane_parity == "even" ~ lane - 1,
      TRUE ~ lane
    )
  ) |>
  dplyr::left_join(
    trp_info_direction,
    by = join_by(trp_id, lane_parity)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    direction,
    lane,
    month,
    length_range,
    volume,
    mean_speed,
    standard_deviation,
    percentile_75 = '75th percentile of speed',
    valid_speed,
    valid_length
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(mean_speed, standard_deviation, percentile_75),
      ~ round(., 1)
    )
  )

# Check ratio of valid values
valid_data <-
  vegdim_data |>
  dplyr::mutate(
    valid = valid_speed & valid_length
  ) |>
  tidyr::pivot_wider(
    id_cols = c(trp_id, lane, length_range, month),
    names_from = valid,
    names_prefix = "valid_",
    values_from = volume,
    values_fn = sum,
    values_fill = 0
  ) |>
  dplyr::mutate(
    valid_ratio = (valid_TRUE / (valid_TRUE + valid_FALSE)) |> round(2)
  ) |>
  dplyr::select(
    trp_id, lane, length_range, month, valid_ratio
  )


# Write ----
vegdim_data |>
  dplyr::filter(
    valid_speed == TRUE,
    valid_length == TRUE
  ) |>
  dplyr::select(
    -valid_speed,
    -valid_length
  ) |>
  dplyr::left_join(
    valid_data,
    by = dplyr::join_by(trp_id, lane, length_range, month)
  ) |>
  dplyr::rename(
    volume_with_valid_speed_and_length = volume
  ) |>
  writexl::write_xlsx("vegdim.xlsx")
