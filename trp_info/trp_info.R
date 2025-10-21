# TRP metainfo, and map internal lanes to fit current metering

# Internal lane numbers from TRP API
# Made in trp_api_fetcher.R
trp_info <- readr::read_rds("trp.rds")


# Mobile TRPs lack info on lanes
trp_info_no_lane <-
  trp_info |>
  dplyr::select(
    trp_id,
    trp_name,
    road_reference,
    registration_frequency
  ) |>
  dplyr::distinct()

trp_info_lane <-
  trp_info |>
  dplyr::select(
    trp_id,
    lane_internal,
    lane_according_to_current_metering
  )

trp_info_lane_direction_names <-
  trp_info |>
  dplyr::mutate(
    from =
      dplyr::case_when(
        lane_according_to_current_metering %% 2 == 0 ~ to_according_to_metering,
        TRUE ~ from_according_to_metering
      ),
    to =
      dplyr::case_when(
        lane_according_to_current_metering %% 2 == 0 ~ from_according_to_metering,
        TRUE ~ to_according_to_metering
      ),
    direction = paste0("Fra ", from, " til ", to)
  ) |>
  dplyr::select(
    trp_id,
    #trp_name,
    #road_reference,
    #lane_internal,
    lane_according_to_current_metering,
    #from_according_to_metering,
    #to_according_to_metering
    direction
  )