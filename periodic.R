# Fetch daily data and calculate aadt by using factor curves

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# All points from Traffic Data API
points_metadata <- get_points() %>%
  dplyr::select(trp_id, name, road_reference, county_geono, county_name, municipality_name,
                road_link_position, lat, lon) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

# Periodic registrations with permanent sensors ####

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
