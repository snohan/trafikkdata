# Fetch daily data and calculate aadt by using factor curves

library(writexl)
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# TRP info from Traffic Data API
# Dec. 21: the mobile trps are not here yet, must use TRP API
points_metadata <- get_points() %>%
  dplyr::select(trp_id, name, road_reference, county_geono, county_name, municipality_name,
                road_link_position, lat, lon) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

# Permanent sensors 2020 ----
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


#estimated_aadt <- calculate_aadt_by_daily_traffic(periodic_inductive_data_ready)

points_inductive_estimated_aadt <- estimated_aadt %>%
  dplyr::left_join(periodic_inductive_data_stats) %>%
  dplyr::left_join(points_metadata) %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                number_of_days, curve, aadt, heavy_ratio, road_link_position, lat, lon)

write.csv2(points_inductive_estimated_aadt,
           file = "periodic_data/periodiske_punkt_faste_sensorer_estimert_aadt.csv",
           row.names = F)


# Permanent sensors 2021 ----
# AADT
estimated_aadt_periodic_permanent <-
  read_csv2(
    "periodic_data/permanent_periodic_aadt_2021.csv"
  ) %>%
  dplyr::filter(
    days > 1
  )

# Heavy ratio
periodic_inductive_data <- get_dt_by_length_for_trp_list(
  estimated_aadt_periodic_permanent$trp_id,
  "2021-01-01T00:00:00+01:00",
  "2022-01-01T00:00:00+01:00")

periodic_permanent_n_days <-
  periodic_inductive_data %>%
  dplyr::filter(
    length_range == "[5.6,..)",
    coverage > 99,
    valid_length > 95
  ) %>%
  dplyr::group_by(point_id) %>%
  dplyr::summarise(number_of_days = n()) %>%
  dplyr::rename(trp_id = point_id)

heavy_ratio_periodic_permanent <-
  periodic_inductive_data %>%
  dplyr::filter(
    length_range == "[5.6,..)",
    coverage > 99,
    valid_length > 95
  ) %>%
  dplyr::mutate(
    daily_heavy_ratio = 100 * length_range_volume / total_volume
  ) %>%
  dplyr::group_by(
    point_id
  ) %>%
  dplyr::summarise(
    heavy_ratio = round(mean(daily_heavy_ratio, na.rm = T),
                        digits = 0)
  ) %>%
  dplyr::rename(trp_id = point_id) %>%
  dplyr::left_join(periodic_permanent_n_days, by = "trp_id")

# Combined trp info, adt and heavy ratio
periodic_permanent <-
  estimated_aadt_periodic_permanent %>%
  dplyr::left_join(
    heavy_ratio_periodic_permanent, by = "trp_id"
  ) %>%
  dplyr::left_join(
    points_metadata, by = "trp_id"
  ) %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    county_name,
    municipality_name,
    road_category,
    road_reference,
    road_link_position,
    year,
    factor_curve,
    days,
    adt,
    heavy_ratio
  ) %>%
  dplyr::filter(
    !is.na(name)
  ) %>%
  dplyr::mutate(
    installation_type = "permanent"
  )


# Mobile registrations 2021 ----
# 2020, see atki_api.R and  tetryon_api.R

# TRP info from TRP API
mobile_trps <- get_mobile_trps_with_commission() %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name = trp_name,
    county_name,
    municipality_name,
    road_category,
    road_reference,
    road_link_position,
    commission_from,
    commission_to,
    commission_interval,
    commission_length_in_days
  )

mobile_trps_distinct <- mobile_trps %>%
  dplyr::distinct(
    trp_id, .keep_all = T
  ) %>%
  dplyr::select(
    trp_id,
    name,
    county_name,
    municipality_name,
    road_category,
    road_reference,
    road_link_position
  )

# Use commissions to filter incomplete days, assuming they are
# the first and last days of commission
mobile_commission_stats <- mobile_trps %>%
  dplyr::group_by(
    commission_length_in_days
  ) %>%
  dplyr::summarise(
    n = n()
  )

mobile_trp_commission_stats <- mobile_trps %>%
  dplyr::mutate(
    year = lubridate::year(commission_from)
  ) %>%
  dplyr::group_by(
    trp_id,
    year
  ) %>%
  dplyr::summarise(
    days = sum(commission_length_in_days),
    n_commissions = n()
  ) %>%
  dplyr::filter(
    !is.na(days)#,
    #year == 2021
  )

mobile_trp_stats <- mobile_trps %>%
  dplyr::filter(
    commission_length_in_days > 0
  ) %>%
  dplyr::distinct(
    trp_id, .keep_all = T
  ) %>%
  dplyr::group_by(
    road_category
  ) %>%
  dplyr::summarise(
    n_trp = n()
  )

mobile_commissions_road_category <- mobile_trps %>%
  dplyr::group_by(
    road_category
  ) %>%
  dplyr::summarise(
    days = sum(commission_length_in_days, na.rm = T)
  )

mobile_commission_incomplete_days <- mobile_trps %>%
  dplyr::mutate(
    start_day = parse_and_floor_date(commission_from, "day"),
    end_day = parse_and_floor_date(commission_to, "day")
  ) %>%
  dplyr::select(
    trp_id, start_day, end_day
  ) %>%
  tidyr::pivot_longer(
    cols = c(start_day, end_day),
    names_to = "day_place",
    values_to = "date"
  ) %>%
  dplyr::select(
    -day_place
  ) %>%
  dplyr::distinct()

# Heavy ratio: aggregate dt from events in Kibana systest,
# filtered by mobile_commission_incomplete_days
heavy_ratio_periodic_mobile_raw <-
  dplyr::bind_rows(
    read_csv2("periodic_data/periodic_mobile_daily_traffic_2021-01-04.csv"),
    read_csv2("periodic_data/periodic_mobile_daily_traffic_2021-05-08.csv"),
    read_csv2("periodic_data/periodic_mobile_daily_traffic_2021-09-12.csv"),
    read_csv2("periodic_data/periodic_mobile_daily_traffic_2020-01-06.csv"),
    read_csv2("periodic_data/periodic_mobile_daily_traffic_2020-07-12.csv"),
    read_csv2("periodic_data/periodic_mobile_daily_traffic_2019-01-12.csv"),
    read_csv2("periodic_data/periodic_mobile_daily_traffic_2018-01-12.csv")
  )

heavy_ratio_periodic_mobile <-
  heavy_ratio_periodic_mobile_raw %>%
  dplyr::mutate(
    date = lubridate::dmy(date)
  ) %>%
  tidyr::pivot_wider(
    names_from = norsikt_2,
    values_from = volume
  ) %>%
  dplyr::anti_join(
    mobile_commission_incomplete_days,
    by = c("trp_id", "date")
  ) %>%
  dplyr::mutate(
    daily_heavy_ratio = 100 * HMV / (LMV + HMV)
  ) %>%
  dplyr::group_by(
    trp_id
  ) %>%
  dplyr::summarise(
    heavy_ratio = round(mean(daily_heavy_ratio, na.rm = T),
                        digits = 0)
  )

# aadt from ES
periodic_mobile_adt <-
  read_csv2(
  "periodic_data/mobile_periodic_aadt.csv"
  )

# Combined trp info, adt and heavy ratio
periodic_mobile <-
  periodic_mobile_adt %>%
  dplyr::left_join(
    mobile_trps_distinct,
    by = "trp_id"
  ) %>%
  dplyr::left_join(
    heavy_ratio_periodic_mobile,
    by = "trp_id"
  ) %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    county_name,
    municipality_name,
    road_category,
    road_reference,
    road_link_position,
    year,
    factor_curve,
    days,
    adt,
    heavy_ratio
  ) %>%
  dplyr::mutate(
    installation_type = "mobile"
  )

# All periodic adts ----
periodic_adt <-
  dplyr::bind_rows(
    #periodic_permanent,
    periodic_mobile
  ) %>%
  dplyr::filter(
    !is.na(road_category)
  )

# Triona CSV
periodic_adt_2021 %>%
  dplyr::filter(
    road_category %in% c("E", "R", "F", "K")
  ) %>%
  dplyr::select(
    trp_id,
    road_link_position,
    year,
    adt,
    heavy_ratio
  ) %>%
  write.csv2(
    "periodic_data/periodisk_adt_2021.csv",
    row.names = F
  )

# User's Excel
periodic_adt %>%
  writexl::write_xlsx(
    path = "periodic_data/aadt_mobile_registreringer.xlsx"
  )


# Excel file for sharing 2020 ----
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

# User supplied file ####
bergheim_vbv <- readr::read_delim("periodisk_RV22_Bergheim.txt") %>%
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime),
                vehicle_class = dplyr::if_else(length < 56, "light", "heavy"),
                point_id = "bergheim")

bergheim_hourly <- bergheim_vbv %>%
  dplyr::mutate(datetime = lubridate::floor_date(datetime, unit = "hour")) %>%
  dplyr::group_by(point_id, datetime, vehicle_class) %>%
  dplyr::summarise(volume = n()) %>%
  tidyr::pivot_wider(names_from = vehicle_class, values_from = volume, values_fill = 0) %>%
  dplyr::mutate(total_volume = sum(light, heavy, na.rm = TRUE),
                heavy_ratio = heavy / total_volume * 100) %>%
  dplyr::ungroup()

# Noe er rart her. Den første dagen er timetrafikken omtrent det dobbelte av hva den er den senere dagene.
# I tillegg mangler et helt døgn, og det mangler midnatt-midnatt UTC.

bergheim_daily_total <- bergheim_vbv %>%
  dplyr::group_by(point_id, weekno, dayno) %>%
  dplyr::summarise(total_volume = n()) %>%
  dplyr::mutate(vehicle_class = "total")

bergheim_daily_class <- bergheim_vbv %>%
  dplyr::group_by(point_id, weekno, dayno, vehicle_class) %>%
  dplyr::summarise(total_volume = n())

# Estimerer
bergheim_alle <- bergheim_hourly %>%
  calculate_aadt_by_hourly_traffic()

bergheim_uten_dag_1 <- bergheim_hourly %>%
  dplyr::slice(-(1:18)) %>%
  calculate_aadt_by_hourly_traffic()

# Factor curve method implementation test
diagonalen <-
  readr::read_csv2("factor_curve_method/94877V319535.csv",
                   locale = locale(tz = "Europe/Oslo")) %>%
  dplyr::mutate(
    point_id = "94877V319535",
    datetime = lubridate::ymd_hms(Dato, tz = "CET"),
    heavy = NA
    ) %>%
  dplyr::select(
    point_id, datetime, total_volume = Volum, heavy
    ) %>%
  calculate_aadt_by_hourly_traffic()

#hourly_traffic <-
okstadbakken <-
  readr::read_csv2("factor_curve_method/25419V72197.csv",
                   locale = locale(tz = "Europe/Oslo")) %>%
  dplyr::mutate(
    point_id = "25419V72197",
    datetime = lubridate::ymd_hms(Dato),
    heavy = NA
  ) %>%
  dplyr::select(
    point_id, datetime, total_volume = Volum, heavy
  ) %>%
  calculate_aadt_by_hourly_traffic()


