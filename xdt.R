# Look at xdt

library(tidyverse)
library(jsonlite)
library(writexl)

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/get_from_trp_api.R")

# points ####
trp <- get_points()

trp_distinct <- trp %>%
  dplyr::filter(
    !is.na(validFrom),
    validTo >= "2021-09-01" | is.na(validTo),
    traffic_type == "VEHICLE",
    !str_detect(road_reference, "KD"),
    registration_frequency == "CONTINUOUS"
  ) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom))


# mdt ####
mdt_2020 <- trp_distinct$trp_id %>%
  get_mdt_for_trp_list("2020")

mdt_all <- mdt_2020 %>%
  dplyr::mutate(lane = "all") %>%
  dplyr::mutate(length_quality = round(100 * valid_length_volume / mdt,
                                       digits = 0))

# mdt_2020_lane <- mdt_2020$trp_id %>%
#   get_mdt_by_lane_for_trp_list("2020")
#
# mdt_lane <- mdt_2020_lane %>%
#   dplyr::mutate(lane = as.character(lane))
#
# mdt_all_and_lane <-bind_rows(mdt_all,
#                              mdt_lane) %>%
#   dplyr::mutate(length_quality = round(100 * valid_length_volume / mdt,
#                                        digits = 0))


# MDT Trøndelag PD ----
trp_data_span <-
  get_trp_data_time_span()

trp_info <-
  trp %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(
    which.min(validFrom)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    trp_data_span,
    by = "trp_id"
  ) %>%
  dplyr::filter(
    first_data_with_quality_metrics < "2017-01-01",
    latest_daily_traffic >= "2022-01-01",
    county_no == 50,
    traffic_type == "VEHICLE",
    !str_detect(road_reference, "KD"),
    registration_frequency == "CONTINUOUS"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    first_data,
    first_data_with_quality_metrics,
    latest_daily_traffic
  )

trps_chosen <-
  c(
    "15862V72153",
    "09480V72828",
    "40308V72241",
    "40541V72295",
    "40729V578598",
    "81077V72158",
    "14220V578146",
    "46610V578105",
    "48910V72822",
    "77685V578099",
    "99657V578673"
  )

mdts <-
  dplyr::bind_rows(
    get_mdt_for_trp_list(trps_chosen, "2017"),
    get_mdt_for_trp_list(trps_chosen, "2018"),
    get_mdt_for_trp_list(trps_chosen, "2019"),
    get_mdt_for_trp_list(trps_chosen, "2020"),
    get_mdt_for_trp_list(trps_chosen, "2021")
  )

trp_and_mdt <-
  mdts %>%
  dplyr::left_join(
    trp_info,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    year,
    month,
    mdt
  ) %>%
  dplyr::filter(
    !is.na(name)
  ) %>%
  dplyr::arrange(
    name
  )

writexl::write_xlsx(
  trp_and_mdt,
  "spesialbestillinger/utvalgte_mdt_trondelag.xlsx"
)


# mdt krs ----
trp_chosen <- c(
  "22540V121303",
  "98936V121303",
  "95764V121488",
  "47254V121508",
  "47077V121488"
)

mdts <-
  dplyr::bind_rows(
    get_mdt_for_trp_list(trp_chosen, "2022"),
    get_mdt_for_trp_list(trp_chosen, "2023")
  )

mdt_summed <-
  mdts |>
  dplyr::filter(
    month < 7
  ) |>
  dplyr::summarise(
    traffic = round(mean(mdt)),
    .by = c(trp_id, year)
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "mdt_",
    values_from = traffic
  ) |>
  dplyr::mutate(
    change_percentage = ((mdt_2023 / mdt_2022 - 1) * 100) |> round(1)
  )

trp_metadata <-
  get_trp_metadata_by_list(trp_chosen) |>
  dplyr::select(
    trp_id, name, from, to, road_reference
  ) |>
  dplyr::distinct() |>
  dplyr::left_join(
    mdt_summed,
    by = join_by(trp_id)
  )

writexl::write_xlsx(
  trp_metadata,
  "spesialbestillinger/krs.xlsx"
)


# device_type ####
trp_device <- get_trs_device() %>%
  dplyr::select(trp_id, trs_id, deviceType)

trp_mdt <- trp_distinct %>%
  dplyr::select(trp_id, name, road_reference, county_name) %>%
  dplyr::left_join(trp_device) %>%
  #dplyr::right_join(mdt_all_and_lane) %>%
  dplyr::right_join(mdt_all) %>%
  dplyr::filter(!is.na(mdt))

write.csv2(trp_mdt, file = "trp_mdt_2020-02.csv", row.names = F)
trp_mdt <- read.csv2("trp_mdt_2020-02.csv")

# Looking at the city index trps ####
city_trp_raw <-
  read_csv2("H:/Programmering/R/byindeks/data_points_raw/cities_points.csv",
            locale = readr::locale(encoding = "latin1"))

city_trp <- city_trp_raw %>%
  select(trp_id) %>%
  unique()

trp_distinct_all <- trp %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom))

city_trp_def <- city_trp_raw %>%
  left_join(trp_distinct_all, by = c("trp_id")) %>%
  select(city_area_name, agreement_start, legacyNortrafMpn,
         name, road_reference, validFrom, kommentar)

city_mdt <- trp_mdt %>%
  ungroup() %>%
  right_join(city_trp) %>%
  filter(!is.na(name)) %>%
  select(trs_id, name, county_name, deviceType, month, coverage, mdt, length_quality) %>%
  arrange(length_quality) %>%
  filter(deviceType == "LOOP_MONITOR")

write.csv2(city_mdt, file = "city_mdt_2020-02.csv", row.names = F)


# city
trp_distinct_city <- trp %>%
  #dplyr::filter(!is.na(validFrom)) %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  #dplyr::filter(!str_detect(road_reference, "KD")) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom))

city_trp_all <- city_trp_raw %>%
  left_join(trp_distinct_city) %>%
  select(city_area_name, name, road_reference, kommentar) %>%
  filter(kommentar != "virtuelt")

# TRPs with differing quality on its lanes
trp_mdt_spread <- trp_mdt %>%
  dplyr::filter(lane != "all") %>%
  dplyr::group_by(trp_id, deviceType) %>%
  dplyr::summarise(no_lanes = n(),
                   sd = sd(length_quality))



# trp trøndelag
trp_trondelag <- trp %>%
  dplyr::filter(county_no == 50) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::select(-validFrom, -validTo)

write.csv2(trp_trondelag, file = "trp_trondelag.csv")

# low aadt ####

trp_aadt <- trp_distinct %>%
  dplyr::filter(validFrom < "2019-07-01")

aadt <- get_aadt_for_trp_list(trp_aadt$trp_id)

trp_with_aadt <- aadt %>%
  dplyr::left_join(trp_distinct) %>%
  dplyr::select(trp_id, name, traffic_type, road_reference, county_name, municipality_name,
                validFrom, number_of_directions, year, adt, coverage, valid_length_volume,
                valid_speed_volume)

write.csv2(trp_with_aadt, file = "alle_aadt.csv",
           row.names = F)

low_aadt <- trp_with_aadt %>%
  dplyr::filter(year == 2019,
                coverage > 90,
                adt <= 500)


# SDT ----
sdt_2021 <-
  get_sdt_for_trp_list(
    trp_distinct$trp_id,
    "2021"
  )

saveRDS(
  sdt_2021,
  "spesialbestillinger/sdt_2021.rds"
)

trp_sdt_2021 <-
  sdt_2021 %>%
  dplyr::filter(
    season == "SUMMER",
    coverage > 50
  ) %>%
  dplyr::left_join(
    trp_distinct,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_name,
    municipality_name,
    season,
    year,
    coverage,
    sdt
  ) %>%
  dplyr::arrange(
    county_name,
    road_reference
  )

writexl::write_xlsx(
  trp_sdt_2021,
  path = "spesialbestillinger/sommerdogntrafikk_2021.xlsx"
)


# Average hour of day ----
trp_latest_data <-
  get_trps_latest_data()

trp_distinct <- trp %>%
  dplyr::filter(
    !is.na(validFrom),
    #validTo >= "2021-09-01" | is.na(validTo),
    traffic_type == "VEHICLE",
    !str_detect(road_reference, "KD"),
    registration_frequency == "CONTINUOUS",
    county_name == "Trøndelag"
  ) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(
    which.min(validFrom)
  ) %>%
  dplyr::left_join(
    trp_latest_data,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_name,
    municipality_name,
    latest_data_by_hour
  )


average_hourly <-
  get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(
    c(
      "01559V249583",
      "06438V2413672",
      "09480V72828",
      "06970V72811"
    ),
    2021
  ) %>%
  dplyr::filter(
    day_type == "ALL"
  ) %>%
  dplyr::inner_join(
    trp_distinct,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_name,
    municipality_name,
    year,
    coverage,
    start_of_hour,
    average_hour_of_day_traffic,
    average_hour_of_day_traffic_relative
  )


melhus_n <-
  dplyr::bind_rows(
    get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(
      c(
        "15862V72153"
      ),
      2017
    ),
    get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(
      c(
        "15862V72153"
      ),
      2018
    ),
    get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(
      c(
        "15862V72153"
      ),
      2019
    ),
    get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(
      c(
        "15862V72153"
      ),
      2020
    ),
    get_trp_average_hour_of_day_traffic_for_all_day_types_for_trp_list(
      c(
        "15862V72153"
      ),
      2021
    )
  ) %>%
  dplyr::filter(
    day_type == "ALL"
  ) %>%
  dplyr::inner_join(
    trp_distinct,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_name,
    municipality_name,
    year,
    coverage,
    start_of_hour,
    average_hour_of_day_traffic,
    average_hour_of_day_traffic_relative
  )

writexl::write_xlsx(
  melhus_n,
  path = "spesialbestillinger/melhus_n.xlsx"
)

melhus_n %>%
  dplyr::mutate(
    year = factor(year)
  ) %>%
  ggplot2::ggplot(
    aes(
      x = start_of_hour,
      y = average_hour_of_day_traffic_relative,
      color = year
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  labs(
    x = "Klokketimestart",
    y = "Relativ andel trafikk (%) \n",
    caption = "Data: Statens vegvesen, fylkeskommunene og kommunene") +
  ggtitle("Relativ fordeling av klokketimetrafikk per år",
          subtitle = "Ev 6 Melhus nord") +
  theme(legend.position = "bottom")
