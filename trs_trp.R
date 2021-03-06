# TRS and TRP

library(geosphere)
library(writexl)

source("H:/Programmering/R/byindeks/get_from_trp_api.R")
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# Large distance between trs and trp ####
trs_trp <- get_stations_and_trps_with_coordinates_from_TRPAPI_httr()

trs_trp_distance <- trs_trp %>%
  dplyr::mutate(distance = round(
                  by(trs_trp, 1:nrow(trs_trp),
                     function(row) {distGeo(c(row$stasjon_lon, row$stasjon_lat),
                                            c(row$punkt_lon, row$punkt_lat))
                  }),
                  digits = 1)) %>%
  dplyr::filter(distance > 100)

write.csv2(trs_trp_distance, file = "stasjon_punkt_avstand.csv",
           row.names = F)

# Points ####
trp <- getPointsFromTRPAPI()
trp_with_commissions <- get_trp_with_commissions()
#trp_trs <- get_trs_trp()
trs_with_trp <- get_all_trs_with_trp_httr()

# trp_id and legacies
trs_with_trp %>%
  dplyr::filter(!is.na(legacyNortrafMpn)) %>%
  write.csv2(file = "trp_id_and_nortraf_mpn.csv",
             row.names = F)

# Stations with AADT in Nortraf in 2014, 2013 or 2012 ####
trs_nortraf <- read_csv2("trs_from_nortraf_with_adt_in_2014-2012.csv")

#trp_from_kristin <- read_csv2("trp_fra_kristin.csv")
#trp_til_kristin <- trp_trs %>%
#  dplyr::filter(trp_id %in% trp_from_kristin$trp_id)
#write.csv2(trp_til_kristin, file = "trp_med_trs.csv", row.names = F)

# Points that shall not have legacyNortrafMpn
trp_shant_legacy <- c("78233V444025", # Rud, added to TRS after Nortraf.
                      "05882V3188133", # Simo, moved to new road.
                      "21571V2394246", # Strindheimtunnelen, ble
                      "73951V2394243", # ikke splittet i Nortraf.
                      "40649V2411511", # Grillstadtunnelen, ble ikke
                      "44210V2411509", # splittet i Nortraf.
                      "74801V2676825", # Thallerkrysset, moved to new road.
                      "32362V1126287", # Buktamo sør, duplicate to be merged.
                      "81921V2407139", # Bergsøya, all data in Datainn.
                      "87500V2407154", # Bergsøya, all data in Datainn.
                      "12426B2798733" # Trikkestallen
)

# Points without legacyNortrafMpn ####
trp_no_legacy <- trp_with_commissions %>%
  dplyr::filter(is.na(legacyNortrafMpn)) %>%
  # Do all have a commission?
  dplyr::filter(!is.na(commission_valid_from)) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::mutate(commission_valid_from =
                  lubridate::with_tz(lubridate::ymd_hms(commission_valid_from)),
                trs_id = as.integer(trs_id)) %>%
  dplyr::slice(which.min(commission_valid_from)) %>%
  #dplyr::filter(commission_valid_from < "2019-01-01") %>%
  dplyr::filter(trs_id < 3000000) %>%
  #dplyr::filter(trs_id %in% trs_nortraf$trs) %>%
  dplyr::filter(incompatible_with_nortraf == FALSE) %>%
  dplyr::filter(!(trp_id %in% trp_shant_legacy)) %>%
  dplyr::arrange(trs_id)

# Points without stations ####
trp_without_trs <- dplyr::anti_join(trp, trs_with_trp)
# TODO: filter out stations without commissions, or why are they not part of
# station-query

# Bike trps, revised approve list ####
trs <- get_all_trs_with_trp()

# or just
bike_trps <- get_points() %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(validFrom)) %>%
  dplyr::filter(traffic_type == "BICYCLE") %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                first_commissioned = validFrom, operational_status) %>%
  dplyr::arrange(road_reference) %>%
  writexl::write_xlsx(path = "sykkelpunkter.xlsx")


# Periodic trps with commissions in 2020 ####
# For use in estimating AADT

interval_2020 <- lubridate::interval(ymd("2020-01-01"), ymd("2020-12-31"))

periodic_trps_with_commission <- get_periodic_trps_with_commission() %>%
  dplyr::filter(status != "PERMANENTLY_RETIRED",
                trafficType == "VEHICLE",
                commission_interval %within% interval_2020,
                commission_length_in_days > 0)

write.csv2(periodic_trps_with_commission,
           "periodiske_registreringer_faste_sensorer_2020.csv",
           row.names = F)

#
# TRPs and their registration frequency ####
# To check correctness of trp info
# TODO: clean code and use public API when possible?

trs_and_trp_id <- get_trs_and_trp_id() # 4656 / 4619

all_trps <- get_points_from_trp_api() # 3569

trps_and_their_trs <- all_trps %>%
  dplyr::left_join(trs_and_trp_id) %>%
  split_road_system_reference() %>%
  # Choose one filter:
  dplyr::filter(registration_frequency == "UNKNOWN") # 25
  #dplyr::filter(is.na(trs_id)) # 192



trs_commission <- get_trs_trp_commissions_httr()

# Periodic registrations from NorTraf ####
# periodic_trps <- get_trs_and_trp_id() %>%
#   dplyr::filter(trs_type == "PERIODIC")

# Find relevant info for each trs to join with outcome of nortraf-csvs
trs_info <- get_trs_info()

# periodic_trp <- periodic_trs_and_trp_id %>%
#   dplyr::filter(!is.na(trp_id))

# Parse NorTraf CSV ####
parse_nortrafweb_csv <- function(filename, year) {
  # Parsing hellish nortrafweb-csvs with aadts to a tibble
  nortraf_csv <- read_csv2(filename,
                           skip = 6,
                           col_names = FALSE) %>%
    dplyr::select(X1, X3, X7, X8, X10, X11, X12, X13) %>%
    head(n = -2)

  trs_ids <- nortraf_csv %>%
    filter(row_number() %% 3 == 1) %>%
    mutate(trs_id = stringr::str_extract(X1, "\\([:digit:]*\\)") %>%
             stringr::str_sub(2, -2)) %>%
    select(trs_id) %>%
    mutate(year = year)

  numbers <- nortraf_csv %>%
    filter(row_number() %% 3 == 0)

  nortrafdata <- bind_cols(trs_ids,
                           numbers) %>%
    rename(trs_id = 1,
           year = 2,
           aadt = 3,
           aadt_sd = 4,
           ydt = 5,
           ydt_sd = 6,
           hdt = 7,
           hdt_sd = 8,
           sdt = 9,
           sdt_sd = 10)

  return(nortrafdata)
}

#filename <- "periodisk_adt/periodiske_n2_2018_retning_lengde.csv"
#year <- "2018"
parse_nortrafweb_csv_with_direction_and_length <- function(filename, year) {
  # Parsing hellish nortrafweb-csvs with aadts to a tibble
  nortraf_csv <- read_csv2(filename,
                           skip = 4,
                           col_names = FALSE) %>%
    dplyr::select(direction = X1,
                  length_class = X5,
                  adt = X8,
                  adt_sd = X9,
                  ydt = X10,
                  ydt_sd = X11,
                  hdt = X12,
                  hdt_sd = X13) %>%
    head(n = -2) %>%
    dplyr::filter(direction != "Felt") %>%
    dplyr::mutate(new_trs = dplyr::if_else(is.na(length_class), "new", "")) %>%
    dplyr::group_by(trs_serial_number = cumsum(new_trs == "new")) %>%
    dplyr::ungroup()

  trs_id_alle <- nortraf_csv %>%
    dplyr::filter(is.na(length_class)) %>%
    mutate(trs_id = stringr::str_extract(direction, "\\([:digit:]*\\)") %>%
             stringr::str_sub(2, -2)) %>%
    select(trs_id) %>%
    mutate(year = year) %>%
    tibble::rowid_to_column("trs_serial_number")

  nortraf_csv_trs_id <- nortraf_csv %>%
    dplyr::left_join(trs_id_alle) %>%
    dplyr::select(-new_trs, -trs_serial_number) %>%
    dplyr::filter(!is.na(length_class))

  return(nortraf_csv_trs_id)
}


# Without length
periodic_aadt_2018 <- bind_rows(
  parse_nortrafweb_csv("periodisk_adt/periodiske_n2_2018.csv", 2018),
  parse_nortrafweb_csv("periodisk_adt/periodiske_n3_2018.csv", 2018)
)

periodic_aadt_2019 <- bind_rows(
  parse_nortrafweb_csv("periodisk_adt/periodiske_n2_2019.csv", 2019),
  parse_nortrafweb_csv("periodisk_adt/periodiske_n3_2019.csv", 2019)
)

periodic_trps_2018 <- left_join(periodic_trps,
                                periodic_aadt_2018)

periodic_trps_2019 <- left_join(periodic_trps,
                                periodic_aadt_2019)

write.csv2(periodic_trps_2018, "periodisk_adt/periodiske_punkt_2018.csv",
           row.names = F)

write.csv2(periodic_trps_2019, "periodisk_adt/periodiske_punkt_2019.csv",
           row.names = F)

# With length
periodic_aadt_length_2018 <- bind_rows(
  parse_nortrafweb_csv_with_direction_and_length("periodisk_adt/periodiske_n2_2018_retning_lengde.csv", 2018),
  parse_nortrafweb_csv_with_direction_and_length("periodisk_adt/periodiske_n3_2018_retning_lengde.csv", 2018)
)

periodic_aadt_length_2019 <- bind_rows(
  parse_nortrafweb_csv_with_direction_and_length("periodisk_adt/periodiske_n2_2019_retning_lengde.csv", 2019),
  parse_nortrafweb_csv_with_direction_and_length("periodisk_adt/periodiske_n3_2019_retning_lengde.csv", 2019)
)

# For NR
# periodic_trps_2018 <- dplyr::inner_join(trs_info,
#                                         periodic_aadt_length_2018)
#
# periodic_trps_2019 <- dplyr::inner_join(trs_info,
#                                         periodic_aadt_length_2019)
#
# write.csv2(periodic_trps_2018, "periodisk_adt/periodiske_punkt_2018_lengde_retning.csv",
#            row.names = F)
#
# write.csv2(periodic_trps_2019, "periodisk_adt/periodiske_punkt_2019_lengde_retning.csv",
#            row.names = F)

# For manual AADT inventory
periodic_trps_2018_2019 <-
  dplyr::bind_rows(
    periodic_aadt_length_2018,
    periodic_aadt_length_2019
  ) %>%
  dplyr::mutate(adt = as.numeric(adt)) %>%
  dplyr::group_by(trs_id, year, length_class) %>%
  dplyr::summarise(adt = round(sum(adt), digits = -1)) %>%
  dplyr::mutate(length_class = dplyr::case_when(
    length_class == "Alle" ~ "alle",
    length_class == "Mindre enn 5,6m" ~ "lette",
    length_class == "Større eller lik 5,6m" ~ "tunge"
  )) %>%
  tidyr::pivot_wider(names_from = length_class, values_from = adt) %>%
  dplyr::mutate(heavy_ratio = round(tunge / alle * 100, digits = 0)) %>%
  dplyr::select(trs_id, year, adt = alle, heavy_ratio) %>%
  dplyr::inner_join(trs_info) %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  split_road_system_reference() %>%
  dplyr::select(trs_id, name, road_category, road_reference, county_name,
                municipality_name, year, adt, heavy_ratio)

writexl::write_xlsx(periodic_trps_2018_2019,
                    path = "periodisk_adt/aadt_periodiske_punkt_faste_sensorer_2018_2019.xlsx")

# Manual TRPs ####
mtrps <- get_manual_points_from_trpapi_httr()

write.csv2(mtrps, file = "manuelle_punkter.csv",
           row.names = F)


# TRS and TRP with legacy ####
trs_trp_legacy <- get_trp_for_vti_httr()

writexl::write_xlsx(trs_trp_legacy, path = "trafikkregistreringspunkt_og_msnr.xlsx")

writexl::write_xlsx(trs_with_trp, path = "trafikkregistreringspunkt_og_nye_msnr.xlsx")





# TRS with more than one trp ####
trs_plural <- trs_with_trp %>%
  dplyr::group_by(trs_id, trs_name, traffic_type, station_type) %>%
  dplyr::summarise(no_trps = n()) %>%
  dplyr::filter(no_trps > 1)

write.csv2(trs_plural, file = "stasjoner_med_flere_punkt.csv",
           row.names = F)



# TRS with TRP and lanes ####
trs_trp_lanes <- get_trs_trp_lanes_httr()

write.csv2(trs_trp_lanes, file = "trs_trp_lanes.csv",
           row.names = F)


# TRS and commissions ####

trs_commissions <- get_trs_trp_commissions_httr()

trs_commissions_filtered <- trs_commissions %>%
  dplyr::filter(operational_status == "OPERATIONAL")

# Need to filter by last commission
last_commission <- trs_commissions %>%
  dplyr::group_by(trs_id) %>%
  dplyr::summarise(max(commission_from)) %>%
  dplyr::rename(last_commission = 2)

trs_commissions_filtered_2 <- trs_commissions_filtered %>%
  dplyr::left_join(last_commission) %>%
  dplyr::filter(last_commission < "2017-11-01",
                is.na(commission_to),
                is.na(device_to)) %>%
  dplyr::arrange(trs_id)

writexl::write_xlsx(trs_commissions_filtered_2,
                    path = "stasjoner_med_siste_igangsetting_pre_nov_2017.xlsx")

# Check all commissions

trs_all_commissions <- get_trs_commissions()

writexl::write_xlsx(trs_all_commissions,
                    path = "stasjoners_igangsettinger.xlsx")


# TRP with direction names ####
trp_with_direction <- get_points_with_direction() %>%
  split_road_system_reference() %>%
  dplyr::arrange(county_no) %>%
  dplyr::select(-road, -road_number) %>%
  dplyr::relocate(road_category_and_number, .before = section_number)

writexl::write_xlsx(trp_with_direction, path = "punkter_med_retningsnavn.xlsx")


# TRS' first commission ####

trs_commissions <- get_trs_trp_commissions_httr()

first_commission <- trs_commissions %>%
  dplyr::group_by(trs_id) %>%
  dplyr::slice_min(commission_from, with_ties = FALSE)

writexl::write_xlsx(first_commission, path = "stasjoners_forste_igangsetting.xlsx")



# All stations and their trps ####
trs_and_trp_id <- get_trs_and_trp_id()

trs_info <- get_trs_info() %>%
  dplyr::select(trs_id,
                trs_road_category = road_category,
                trs_road_reference = road_reference)

trp_info <- get_points_from_trp_api() %>%
  dplyr::select(trp_id,
                legacy_nortraf_mpn,
                trp_name = name,
                trp_status,
                registration_frequency,
                road_reference,
                municipality_name,
                county_name)

all_trs <- trs_and_trp_id %>%
  dplyr::left_join(trs_info) %>%
  dplyr::left_join(trp_info) %>%
  dplyr::relocate(trp_id, .before = trp_name) %>%
  dplyr::mutate(trs_id = as.numeric(trs_id)) %>%
  dplyr::arrange(trs_id)

writexl::write_xlsx(all_trs, path = "all_stations.xlsx")
