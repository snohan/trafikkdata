# Fetch daily data and calculate aadt by using factor curves

library(writexl)
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# All points from Traffic Data API
points_metadata <- get_points() %>%
  dplyr::select(trp_id, name, road_reference, county_geono, county_name, municipality_name,
                road_link_position, lat, lon) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

# Periodic registrations with permanent sensors ####
# Fetched from file made in trs_trp.R
periodic_inductive <- read.csv2("periodiske_registreringer_faste_sensorer_2020.csv") %>%
  dplyr::select(trp_id, traffic_type = trafficType, status, registration_frequency, road_link_position,
                lat, lon, commission_interval, commission_length_in_days)

periodic_inductive_trp_ids <- periodic_inductive %>%
  dplyr::select(trp_id) %>%
  dplyr::distinct(trp_id, .keep_all = T)


# Fetch daily traffic from API
periodic_inductive_data <- get_dt_by_length_for_trp_list(
  periodic_inductive_trp_ids$trp_id,
  "2020-01-01T00:00:00+01:00",
  "2021-01-01T00:00:00+01:00")

periodic_inductive_data_stats <- periodic_inductive_data %>%
  dplyr::filter(length_range == "[5.6,..)",
                coverage > 99) %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarise(number_of_days = n()) %>%
  dplyr::rename(trp_id = point_id)

periodic_inductive_stats <- periodic_inductive %>%
  dplyr::left_join(points_metadata) %>%
  dplyr::left_join(periodic_inductive_data_stats) %>%
  dplyr::select(trp_id, name, county_name, municipality_name, commission_interval,
                commission_length_in_days, total_number_of_days_in_2020 = number_of_days)

periodic_inductive_data_ready <- periodic_inductive_data %>%
  dplyr::filter(length_range == "[5.6,..)",
                coverage > 99) %>%
  dplyr::mutate(weekno = lubridate::isoweek(from),
                dayno = lubridate::wday(from,
                                        week_start = getOption("lubridate.week.start", 1)))


estimated_aadt <- calculate_aadt_by_daily_traffic(periodic_inductive_data_ready)

points_inductive_estimated_aadt <- estimated_aadt %>%
  dplyr::left_join(periodic_inductive_data_stats) %>%
  dplyr::left_join(points_metadata) %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                number_of_days, curve, aadt, heavy_ratio, road_link_position, lat, lon)

write.csv2(points_inductive_estimated_aadt,
           file = "periodic_data/periodiske_punkt_faste_sensorer_estimert_aadt.csv",
           row.names = F)



# TOPO radars ####
# See atki_api.R

# Armadillo radars ####
# See tetryon_api


# Gather all in an Excel file for sharing ####
periodic_inductive <- read.csv2("periodic_data/periodiske_punkt_faste_sensorer_estimert_aadt.csv") %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                number_of_days, factor_curve = curve, aadt, heavy_ratio) %>%
  dplyr::mutate(equipment = "inductive loops")

# Need geo info
municipalities <- get_municipalities()
counties <- get_counties()
municipalities_and_counties <- dplyr::left_join(municipalities, counties)
municipality_info_chosen <- municipalities_and_counties %>%
  dplyr::select(municipality_number, municipality_name, county_name)

periodic_topo <- read.csv2("periodic_data/topo_estimates.csv") %>%
  dplyr::select(trp_id = site_id, road_reference, municipality_number = municipality,
                number_of_days = complete_days, factor_curve = curve, aadt, heavy_ratio) %>%
  dplyr::left_join(municipality_info_chosen) %>%
  dplyr::mutate(name = NA,
                equipment = "TOPO radar") %>%
  dplyr::select(-municipality_number)


#periodic_armadillo <- read.csv2("periodic_data/armadillo_estimates.csv")

periodic_aadt_estimates <- dplyr::bind_rows(
  periodic_inductive,
  periodic_topo
  #periodic_armadillo
  ) %>%
  split_road_system_reference() %>%
  dplyr::select(-(road:intersection_meter), -road_number, -road_category_and_number) %>%
  dplyr::relocate(road_category, .before = road_reference) %>%
  dplyr::arrange(county_name)

writexl::write_xlsx(periodic_aadt_estimates, path = "periodic_data/aadt_periodiske_registreringer_2020.xlsx")
