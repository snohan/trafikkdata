# Parse csv files from Kibana
# Add trp name, roadref, county andt speed limit

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")


# Point metadata from Traffic Data API
points <- get_points()

trps <- points %>%
  dplyr::rowwise() %>%
  dplyr::mutate(lanes = toString(lane_numbers)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lane_numbers) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

trp_data_time_span <- get_trp_data_time_span()

trps_chosen <- readr::read_csv2("fart_okv/trp_okv.csv")

# One list with how it is like now
trp_okv <- trps %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice_max(validFrom, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::filter(trp_id %in% trps_chosen$trp_id) %>%
  dplyr::select(trp_id, name, road_reference, lanes, county_geono, county_name,
                municipality_name, lat, lon, road_link_position, operational_status) %>%
  dplyr::left_join(trp_data_time_span, by = "trp_id") %>%
  dplyr::mutate(speed_limit = mapply(get_speedlimit_by_roadlink, road_link_position)) %>%
  dplyr::arrange(speed_limit, road_reference)

readr::write_rds(trp_okv, file = "fart_okv/trp_okv.rds")
writexl::write_xlsx(trp_okv, "fart_okv/utvalgte_punkter.xlsx")
# Commission and lane history

# TODO: Speed limit history (LATER)
