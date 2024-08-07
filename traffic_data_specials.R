#


# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(writexl)
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
}

# MDT for SD ----
## Get data ----
trp_info <-
  get_points() |>
  dplyr::distinct(trp_id, .keep_all = T) |>
  split_road_system_reference() |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category_and_number,
    municipality_name,
    lat, lon
  )

chosen_trps <-
  c(
    "76361V1665570",
    "92394V885954",
    "25920V885956",
    "03102V885959",
    "85320V885198",
    "21008V885200",
    "65823V1668921",
    "96603V885224",
    "65984V885978",
    "06012V885929"
  )

years <- c(2019, 2021, 2023, 2024)

mdt <-
  purrr::map(years, ~ get_mdt_for_trp_list(chosen_trps, .x)) |>
  dplyr::bind_rows()

## Export ----
mdt_tidy <-
  mdt |>
  dplyr::select(
    trp_id,
    year,
    month,
    mdt
  ) |>
  tidyr::pivot_wider(
    names_from = month,
    names_prefix = "month_",
    values_from = mdt
  ) |>
  dplyr::left_join(
    trp_info,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    road_category_and_number,
    trp_name = name,
    municipality_name,
    year,
    month_1:month_12
  ) |>
  dplyr::arrange(
    road_category_and_number,
    trp_id,
    year
  ) |>
  dplyr::select(
    -trp_id
  )

chosen_trps_info <-
  trp_info |>
  dplyr::filter(
    trp_id %in% chosen_trps
  ) |>
  dplyr::mutate(
    url = paste0(
      "https://trafikkdata.atlas.vegvesen.no/#/kart?lat=",
      lat,
      "&lon=",
      lon,
      "&trpids=",
      trp_id,
      "&zoom=9"
    )
  ) |>
  dplyr::select(
    -lat, -lon
  )

list(
  punktinfo = chosen_trps_info,
  trafikkdata = mdt_tidy
) |>
  writexl::write_xlsx(
    path = "spesialbestillinger/mdt_nordland.xlsx"
  )
