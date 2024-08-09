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


# Asplan Viak Lindheim-Minde ----
the_data <-
  dplyr::bind_rows(
    readr::read_delim(
      "spesialbestillinger/lindheimminde_a.csv"
    ),
    readr::read_delim(
      "spesialbestillinger/lindheimminde_b.csv"
    ),
    readr::read_delim(
      "spesialbestillinger/lindheimminde_c.csv"
    ),
    readr::read_delim(
      "spesialbestillinger/lindheimminde_periodic.csv"
    )
  ) |>
  # Weird quirk in reading Kibana eksport: ignores decimal
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.numeric),
      ~ .x / 100
    )
  ) |>
  tidyr::pivot_wider(
    names_from = godkjent_fart,
    values_from = c(trafikkmengde, snittfart),
    values_fill = list(trafikkmengde = c(0), snittfart = NA)
  ) |>
  dplyr::mutate(
    trafikkmengde = trafikkmengde_TRUE + trafikkmengde_FALSE,
    prosentandel_godkjent_fart = round(trafikkmengde_TRUE / trafikkmengde * 100, 1)
  ) |>
  dplyr::select(
    trp_id,
    felt,
    dag,
    periode_start,
    trafikkmengde,
    snittfart = snittfart_TRUE,
    prosentandel_godkjent_fart
  ) |>
  dplyr::mutate(
    snittfart =
      dplyr::case_when(
        trafikkmengde <= 5 ~ NA_real_,
        TRUE ~ snittfart
      )
  )

# TRP metainfo, and map internal lanes to fit current metering
trp_info <- readr::read_rds("trs_trp/trp.rds")

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

the_data_tidy <-
  the_data |>
  dplyr::left_join(
    trp_info_no_lane,
    by = dplyr::join_by(
      trp_id == trp_id
    )
  ) |>
  dplyr::left_join(
    trp_info_lane,
    by = dplyr::join_by(
      trp_id == trp_id,
      felt == lane_internal
    )
  ) |>
  dplyr::mutate(
    felt =
      dplyr::case_when(
        !is.na(lane_according_to_current_metering) ~ lane_according_to_current_metering,
        TRUE ~ felt
      )
  ) |>
  dplyr::select(
    trp_id,
    trp_name,
    road_reference,
    registration_frequency,
    felt,
    dag,
    periode_start,
    trafikkmengde,
    snittfart,
    prosentandel_godkjent_fart
  )

the_data_tidy |>
  summarise(
    n = n(),
    mean_volume = mean(trafikkmengde),
    .by = trp_name
  )

writexl::write_xlsx(
  the_data_tidy,
  "spesialbestillinger/lindheimminde.xlsx"
)


# Heggedalsposten ----
heggedal <-
  dplyr::bind_rows(
    readr::read_delim(
      "spesialbestillinger/heggedalsposten_1.csv"
    ),
    readr::read_delim(
      "spesialbestillinger/heggedalsposten_2.csv"
    )
  ) |>
  # Weird quirk in reading Kibana eksport: ignores decimal
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.numeric),
      ~ .x / 100
    )
  ) |>
  dplyr::left_join(
    trp_info_no_lane,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    trp_name,
    road_reference,
    dag,
    periode_start,
    trafikkmengde,
    snittfart
  )

writexl::write_xlsx(
  heggedal,
  "spesialbestillinger/heggedal.xlsx"
)
