#


# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(writexl)
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")

  decimal_point <- function(number) {
    stringr::str_replace(as.character(number), ",", "\\.")
  }

  # TRP metainfo, and map internal lanes to fit current metering
  trp_info <- readr::read_rds("trs_trp/trp.rds")
  # Made in trp_api_fetcher.R

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
    dplyr::select(
      trp_id,
      trp_name,
      road_reference,
      lane_internal,
      lane_according_to_current_metering,
      from_according_to_metering,
      to_according_to_metering
    )


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
      "spesialbestillinger/heggedalsposten_1a.csv"
    ),
    readr::read_delim(
      "spesialbestillinger/heggedalsposten_2a.csv"
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
  # dplyr::left_join(
  #   trp_info_lane,
  #   by = dplyr::join_by(trp_id, felt == lane_internal)
  # ) |>
  dplyr::select(
    trp_id,
    trp_name,
    road_reference,
    felt,
    dag,
    periode_start,
    trafikkmengde,
    snittfart,
    fraktil_85 = '85th percentile of 85_fraktil'
  ) |>
  dplyr::mutate(
    snittfart =
      dplyr::case_when(
        trafikkmengde <= 5 ~ NA_real_,
        TRUE ~ snittfart
      ),
    fraktil_85 =
      dplyr::case_when(
        trafikkmengde <= 5 ~ NA_real_,
        TRUE ~ fraktil_85
      )
  )

# daily <-
#   heggedal |>
#   dplyr::summarise(
#     n = n(),
#     traffic = sum(trafikkmengde),
#     .by = c(trp_id, dag)
#   )

writexl::write_xlsx(
  heggedal,
  "spesialbestillinger/heggedal.xlsx"
)


# 15 min speed ----
# Zhong et. al.

the_data <-
  dplyr::bind_rows(
    readr::read_delim(
      "spesialbestillinger/molde_ferjekai_2024-07-10.csv"
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
  "spesialbestillinger/molde_ferjekai_2024-07-10.xlsx"
)


# Speed ----
the_data <-
  dplyr::bind_rows(
    readr::read_delim(
      "spesialbestillinger/atlanterhavsvegen.csv"
    )
  ) |>
  # Weird quirk in reading Kibana eksport: ignores decimal
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.numeric),
      ~ .x / 100
    )
  ) |>
  dplyr::rename(
    percentile_85 = '85th percentile of percentile_85'
  ) |>
  dplyr::mutate(
    mean_speed = decimal_point(mean_speed) |> as.numeric(),
    percentile_85 = decimal_point(percentile_85) |> as.numeric()
  ) |>
  tidyr::pivot_wider(
    names_from = valid_speed,
    values_from = c(traffic, mean_speed, percentile_85),
    values_fill = list(traffic = c(0), mean_speed = NA, percentile_85 = NA)
  ) |>
  dplyr::mutate(
    traffic_volume = traffic_TRUE + traffic_FALSE,
    percentage_valid_speed = round(traffic_TRUE / traffic_volume * 100, 1)
  ) |>
  dplyr::select(
    trp_id,
    trp_lane,
    day,
    #periode_start,
    traffic_volume,
    mean_speed = mean_speed_TRUE,
    percentile_85 = percentile_85_TRUE,
    percentage_valid_speed
  ) |>
  dplyr::mutate(
    mean_speed =
      dplyr::case_when(
        traffic_volume <= 5 ~ NA_real_,
        TRUE ~ mean_speed
      )
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
      trp_lane == lane_internal
    )
  ) |>
  dplyr::mutate(
    trp_lane =
      dplyr::case_when(
        !is.na(lane_according_to_current_metering) ~ lane_according_to_current_metering,
        TRUE ~ trp_lane
      )
  ) |>
  dplyr::select(
    trp_id,
    trp_name,
    road_reference,
    trp_lane,
    day,
    #period_start,
    traffic_volume,
    mean_speed,
    percentile_85,
    percentage_valid_speed
  )

the_data_tidy |>
  summarise(
    n = n(),
    mean_volume = mean(traffic_volume),
    .by = trp_name
  )

writexl::write_xlsx(
  the_data_tidy,
  "spesialbestillinger/atlanterhavsvegen.xlsx"
)


# 15 min speed and length ----
# Want valid speed per length class, but only for those with valid length.
# Need percentages for both valid speed and valid length.
# Will show only total volume, not total mean speed, just mean speed per length class.

# Årstadmodellen
subfolder <- "spesialbestillinger/aarstadmodellen"

the_data <-
  purrr::map(
    list.files(subfolder),
    ~ readr::read_delim(paste0(subfolder, "/", .))
  ) |>
  purrr::list_rbind() |>
  # Weird quirk in reading Kibana eksport: ignores decimal
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.numeric),
      ~ .x / 100
    ),
    lengdeklasse =
      dplyr::case_when(
        lengdeklasse == "≥ 0,00 and < 5,60" ~ "korte",
        TRUE ~ "lange"
      )
  )

# Must separate speed and length to get percentages of valid measurements correct.
the_length_data <-
  the_data |>
  dplyr::select(
    trp_id,
    felt,
    dag,
    periode_start,
    lengdeklasse,
    trafikkmengde,
    godkjent_lengde
  ) |>
  dplyr::summarise(
    trafikkmengde = sum(trafikkmengde),
    .by = c(trp_id, felt, dag, periode_start, lengdeklasse, godkjent_lengde)
  ) |>
  tidyr::pivot_wider(
    names_from = godkjent_lengde,
    names_prefix = "trafikkmengde_",
    values_from = c(trafikkmengde),
    values_fill = list(trafikkmengde = c(0))
  ) |>
  dplyr::mutate(
    trafikkmengde_total = sum(trafikkmengde_TRUE, trafikkmengde_FALSE),
    prosentandel_godkjent_lengde = round(sum(trafikkmengde_TRUE) / trafikkmengde_total * 100, 1),
    .by = c(trp_id, felt, dag, periode_start)
  ) |>
  dplyr::select(
    trp_id,
    felt,
    dag,
    periode_start,
    lengdeklasse,
    trafikkmengde_total,
    trafikkmengde_lengde = trafikkmengde_TRUE,
    prosentandel_godkjent_lengde
  )

the_speed_data <-
  the_data |>
  dplyr::filter(
    godkjent_lengde == TRUE & godkjent_fart == TRUE
  ) |>
  dplyr::select(
    trp_id,
    felt,
    dag,
    periode_start,
    lengdeklasse,
    trafikkmengde_godkjent_fart = trafikkmengde,
    snittfart
  ) |>
  dplyr::mutate(

  )

the_data_tidy <-
  the_length_data |>
  dplyr::left_join(
    the_speed_data,
    by = join_by(trp_id, felt, dag, periode_start, lengdeklasse)
  ) |>
  dplyr::mutate(
    prosentandel_godkjent_fart = round(trafikkmengde_godkjent_fart / trafikkmengde_lengde * 100, 1),
    snittfart =
      dplyr::case_when(
        trafikkmengde_godkjent_fart <= 5 ~ NA_real_,
        TRUE ~ snittfart
      ),
    prosentandel_godkjent_fart = dplyr::if_else(is.na(snittfart), NA_real_, prosentandel_godkjent_fart)
  ) |>
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
    felt,
    dag,
    periode_start,
    lengdeklasse,
    trafikkmengde_total,
    trafikkmengde_lengde,
    prosentandel_godkjent_lengde,
    snittfart,
    prosentandel_godkjent_fart
  )

the_data_summarised <-
  the_data_tidy |>
  summarise(
    n = n(),
    mean_volume = mean(trafikkmengde_total, na.rm = T),
    mean_speed = mean(snittfart, na.rm = T),
    .by = c(trp_name, felt, lengdeklasse)
  )

the_trp_names <-
  the_data_tidy |>
  dplyr::select(
    trp_name
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(trp_name)

writexl::write_xlsx(
  the_data_tidy,
  "spesialbestillinger/aarstadmodellen.xlsx"
)


# VBV Politiet ----
the_data <-
  dplyr::bind_rows(
    readr::read_delim(
      "spesialbestillinger/vbv_pd.csv"
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
    trp_info_lane_direction_names,
    by = dplyr::join_by(
      traffic_registration_point_id == trp_id,
      lane == lane_internal
    )
  ) |>
  dplyr::mutate(
    lane =
      dplyr::case_when(
        !is.na(lane_according_to_current_metering) ~ lane_according_to_current_metering,
        TRUE ~ lane
      ),
    from =
      dplyr::case_when(
        lane %% 2 == 0 ~ to_according_to_metering,
        TRUE ~ from_according_to_metering
      ),
    to =
      dplyr::case_when(
        lane %% 2 == 0 ~ from_according_to_metering,
        TRUE ~ to_according_to_metering
      ),
    vehicle_class =
      dplyr::case_when(
        norsikt_class_l4 == "MC, MP" ~ "Motorsykkel, moped",
        norsikt_class_l4 == "MC, MP WC" ~ "Motorsykkel, moped med henger",
        norsikt_class_l4 == "PC, LGV, LB" ~ "Personbil, lett varebil, lett buss",
        norsikt_class_l4 == "PC, LGV, LB WC" ~ "Personbil, lett varebil, lett buss med henger",
        norsikt_class_l4 == "HB WOC WC" ~ "Buss med eller uten henger",
        norsikt_class_l4 == "HGV, RT, EMS" ~ "Lastebil, trekkbil, annet tungt kjøretøy",
        norsikt_class_l4 == "HGV WC" ~ "Lastebil med henger",
        norsikt_class_l4 == "RT WC" ~ "Trekkbil med henger (semitrailer)"
      )
  ) |>
  dplyr::select(
    traffic_registration_point_id,
    trp_name,
    road_reference,
    lane,
    from,
    to,
    event_timestamp,
    vehicle_class,
    speed,
    length,
    valid_classification,
    valid_speed,
    valid_length
  )

writexl::write_xlsx(
  the_data,
  "spesialbestillinger/garmo.xlsx"
)
