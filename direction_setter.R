library(tidyverse)
library(readxl)
library(writexl)

# Only dummy commission ----
# Read json file with dump from ES with timestamp from first NorTraf data per TRP, all products (nortraf_*)
first_data_in_nortraf <-
  jsonlite::fromJSON(
    "trp_direction/firstDataNortrafTrps.json",
    flatten = TRUE
  ) |>
  dplyr::select(
    trp_id = key,
    from = minFrom.value_as_string
  ) |>
  dplyr::mutate(
    from = lubridate::ymd_hms(from) |> lubridate::ceiling_date(unit = "day")
  )

trp_with_only_dummy_commission <-
  readxl::read_excel(
    path = "trp_direction/only_dummy.xlsx"
  ) |>
  dplyr::select(
    trp_id = "TRP-ID"
  ) |>
  dplyr::left_join(
    first_data_in_nortraf,
    by = join_by(trp_id)
  )

writexl::write_xlsx(
  trp_with_only_dummy_commission,
  path = "trp_direction/only_dummy_first_data.xlsx"
)


# TRP history ----
# Can it help filling in missing history?
trp_history <-
  jsonlite::fromJSON(
    "trp_direction/trp-history.json",
    flatten = TRUE
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(last_modified, valid_from, valid_to, reference_time),
      .fns = ~ stringr::str_sub(.x, 1, 19) |> lubridate::dmy_hms()
    )
  ) |>
  dplyr::arrange(
    trp_id
  ) |>
  dplyr::select(
    id,
    last_modified,
    trp_id,
    name,
    #valid_from,
    #valid_to,
    road_link_id,
    road_link_position,
    reference_time,
    metering_with_link_at_ref_time,
    created_by
  ) |>
  dplyr::distinct(
    trp_id,
    road_link_id,
    #road_link_position,
    reference_time,
    .keep_all = TRUE
  ) |>
  dplyr::group_by(
    trp_id
  ) |>
  dplyr::filter(
    n() > 1
  )
