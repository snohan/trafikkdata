# TRP metainfo, and map internal lanes to fit current metering


# Internal lane numbers from TRP API ----
# Made in trp_api_fetcher.R
trp_info <- readr::read_rds("trp_info/trp.rds")


# Handling mobile TRPs ----
# Mobile TRPs does not have the concept of internal lane numbers - their lane numbers are per definition according to road link
trp_info_mobile <-
  trp_info |>
  dplyr::filter(
    trp_type == "MOBILE"
  ) |>
  dplyr::mutate(
    # All mobile TRPs have reference direction given according to current road link
    direction_with_current_link = TRUE
  )

# All mobile TRPs have lane numbers 1 and/or 2.
# Adding both will sometimes be wrong (if they have only one lane),
# but when joining this to real data, the lanes that doesn't exist will not appear.
trp_info_mobile_lane <-
  dplyr::bind_rows(
    trp_info_mobile |>
      dplyr::mutate(
        lane_internal = 1
      ),
    trp_info_mobile |>
      dplyr::mutate(
        lane_internal = 2
      )
  ) |>
  dplyr::arrange(trp_id) |>
  dplyr::mutate(
    lane_according_to_current_metering =
      dplyr::case_when(
        same_direction_of_metering_and_link_now ~ lane_internal,
        !same_direction_of_metering_and_link_now & lane_internal %% 2 == 0 ~ 1,
        TRUE ~ 2
      )
  )


# Tables for joining with traffic data ----
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
  dplyr::bind_rows(
    trp_info |>
      dplyr::filter(
        trp_type != "MOBILE"
      ),
    trp_info_mobile_lane
  ) |>
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
