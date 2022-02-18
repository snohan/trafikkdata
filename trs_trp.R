# TRS and TRP

source("H:/Programmering/R/byindeks/get_from_trp_api.R")
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

library(geosphere)
library(readxl)
library(writexl)

# TRP stats ----
trp <- get_points()

distinct_trps <- trp %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    traffic_type,
    road_category,
    county_name,
    registration_frequency,
    operational_status
    ) %>%
  dplyr::distinct(trp_id, .keep_all = T)

trp_stats_road_category <- distinct_trps %>%
  dplyr::group_by(
    traffic_type,
    #road_category,
    registration_frequency
  ) %>%
  dplyr::summarise(
    n_trp = n()
  )

# Large distance from TRS to TRP ----
# Note that bike trps with LM should not be more than 10 m away
trs_trp <- get_trs_and_trps_with_coordinates_from_trp_api()

trs_trp_distance <- trs_trp %>%
  dplyr::mutate(distance = round(
                  by(trs_trp, 1:nrow(trs_trp),
                     function(row) {distGeo(c(row$stasjon_lon, row$stasjon_lat),
                                            c(row$punkt_lon, row$punkt_lat))
                  }),
                  digits = 1)) %>%
  dplyr::filter(distance > 100) %>%
  dplyr::arrange(desc(distance))

write.csv2(trs_trp_distance, file = "stasjon_punkt_avstand.csv",
           row.names = F)


# TRPs ----
trp <- get_points_from_trp_api()
trp_with_commissions <- get_trp_with_commissions()
#trp_trs <- get_trs_trp()
trs_with_trp <- get_all_trs_with_trp()
trs_with_trp_via_sensorconfig <- get_all_trs_with_trp_via_sensorconfig()


# TRPs and lanes ----
trp <- get_points()

trp_latest_data <- get_trps_latest_data()

trp_tidy <- trp %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::select(trp_id, name, traffic_type, registration_frequency,
                road_reference, county_name, municipality_name,
                operational_status, lane_numbers) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(lanes = toString(lane_numbers)) %>%
  dplyr::select(-lane_numbers) %>%
  dplyr::left_join(
    trp_latest_data,
    by = "trp_id"
  )

writexl::write_xlsx(trp_tidy, path = "trs_trp/punkter_med_feltnummer.xlsx")


# TRP and legacies ----
trs_with_trp %>%
  dplyr::filter(!is.na(legacyNortrafMpn)) %>%
  write.csv2(file = "trp_id_and_nortraf_mpn.csv",
             row.names = F)


## TRS with AADT in Nortraf in 2014, 2013 or 2012 ----
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


## TRPs without legacyNortrafMpn ----
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


# TRP without TRS ----
trp_without_trs <- dplyr::anti_join(trp, trs_with_trp) %>%
  dplyr::anti_join(trs_with_trp_via_sensorconfig) %>%
  dplyr::filter(!stringr::str_detect(trp_status, ".*RETIRED"))
# TODO: filter out stations without commissions, or why are they not part of
# station-query


# Bike TRPs, revised approve list ----
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


# Periodic trps with commissions in 2020 ----
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


# TRPs and their registration frequency ----
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


# Periodic registrations from NorTraf ----
# periodic_trps <- get_trs_and_trp_id() %>%
#   dplyr::filter(trs_type == "PERIODIC")

# Find relevant info for each trs to join with outcome of nortraf-csvs
trs_info <- get_trs_info()

# periodic_trp <- periodic_trs_and_trp_id %>%
#   dplyr::filter(!is.na(trp_id))


## Parse NorTraf CSV ----
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


# Periodic AADT ----
periodic_trps <-
  trp %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    traffic_type,
    registration_frequency,
    road_category,
    road_category_and_number,
    road_reference,
    road_link_position,
    county_name,
    municipality_name
  ) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "PERIODIC"
  )

# 1. Fetch AADT from API and calculate heavy_ratio
# 2. Read AADT by factor curve method currently only available in Kibana

periodic_aadt_raw <-
  get_aadt_by_length_for_trp_list(
    periodic_trps$trp_id
  ) %>%
  dplyr::select(
    trp_id,
    year,
    length_range,
    aadt_length_range,
    aadt_total
  )

periodic_aadt_tidy <-
  periodic_aadt_raw %>%
  dplyr::filter(
    length_range == "[5.6,..)"
  ) %>%
  dplyr::mutate(
    heavy_ratio = round(aadt_length_range / aadt_total * 100, digits = 0)
  ) %>%
  dplyr::left_join(
    periodic_trps,
    by = "trp_id"
  ) %>%
  dplyr::filter(
    year > 2017
  ) %>%
  dplyr::select(
    trp_id,
    name,
    traffic_type,
    registration_frequency,
    road_category,
    road_category_and_number,
    road_reference,
    road_link_position,
    county_name,
    municipality_name,
    year,
    adt_plain = aadt_total,
    heavy_ratio
  )

aadt_by_factor_curve <-
  readr::read_csv2(
    "periodic_data/periodic_aadt_by_factor_curve.csv"
  ) %>%
  dplyr::select(
    trp_id = traffic_registration_point_id,
    year,
    adt_factor_curve = adt,
    curve
  )

periodic_aadt_with_factor_curve <-
  periodic_aadt_tidy %>%
  dplyr::left_join(
    aadt_by_factor_curve,
    by = c("trp_id", "year")
  ) %>%
  dplyr::arrange(
    county_name,
    road_reference
  )

writexl::write_xlsx(
  periodic_aadt_with_factor_curve,
  path = "periodic_data/aadt_periodiske_punkt_faste_sensorer_2018-2020.xlsx")

# For Triona
periodic_aadt_with_factor_curve %>%
  dplyr::filter(
    year == 2021
  ) %>%
  dplyr::select(
    -adt_plain
  ) %>%
  write.csv2(
    file = "periodic_data/aadt_periodiske_punkt_faste_sensorer_2021.csv",
    row.names = FALSE
  )


# Add new road reference to TRS list from nortraf ----
nortraf_n2 <- readxl::read_xlsx("periodisk_adt_nortraf/nortraf_trs_n2.xlsx",
                                skip = 4) %>%
  dplyr::mutate(Nummer = as.character(Nummer))

trs_ny_vegref <- trs_info %>%
  dplyr::select(Nummer = trs_id, Vegreferanse_ny = road_reference,
                Fylke_ny = county_name, Kommune_ny = municipality_name)

nortraf_n2_added <- nortraf_n2 %>%
  dplyr::left_join(trs_ny_vegref) %>%
  dplyr::relocate(Vegreferanse_ny, .after = Vegreferanse) %>%
  dplyr::relocate(Fylke_ny, .after = Fylke) %>%
  dplyr::relocate(Kommune_ny, .after = Kommune)

writexl::write_xlsx(nortraf_n2_added,
                    path = "periodisk_adt_nortraf/nortraf_n2_added.xlsx")

nortraf_n3 <- readxl::read_xlsx("periodisk_adt_nortraf/nortraf_trs_n3.xlsx",
                                skip = 4) %>%
  dplyr::mutate(Nummer = as.character(Nummer))

nortraf_n3_added <- nortraf_n3 %>%
  dplyr::left_join(trs_ny_vegref) %>%
  dplyr::relocate(Vegreferanse_ny, .after = Vegreferanse) %>%
  dplyr::relocate(Fylke_ny, .after = Fylke) %>%
  dplyr::relocate(Kommune_ny, .after = Kommune)

writexl::write_xlsx(nortraf_n3_added,
                    path = "periodisk_adt_nortraf/nortraf_n3_added.xlsx")


# Manual TRPs ----
mtrps <- get_manual_points_from_trpapi_httr()

write.csv2(mtrps, file = "manuelle_punkter.csv",
           row.names = F)


# TRS and TRP with legacy ----
trs_trp_legacy <- get_trp_for_vti_httr()

writexl::write_xlsx(trs_trp_legacy, path = "trafikkregistreringspunkt_og_msnr.xlsx")

writexl::write_xlsx(trs_with_trp, path = "trafikkregistreringspunkt_og_nye_msnr.xlsx")


# TRS with more than one trp ----
trs_plural <- trs_with_trp %>%
  dplyr::group_by(trs_id, trs_name, traffic_type, station_type) %>%
  dplyr::summarise(no_trps = n()) %>%
  dplyr::filter(no_trps > 1)

write.csv2(trs_plural, file = "trs_trp/stasjoner_med_flere_punkt.csv",
           row.names = F)


# TRS with TRP and lanes ----
trs_trp_lanes <- get_trs_trp_lanes()

write.csv2(trs_trp_lanes, file = "trs_trp_lanes.csv",
           row.names = F)


# TRS and commissions ----

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


# TRP with direction names ----
trp_with_direction <- get_points_with_direction() %>%
  split_road_system_reference() %>%
  dplyr::arrange(county_no) %>%
  dplyr::select(-road, -road_number) %>%
  dplyr::relocate(road_category_and_number, .before = section_number)

writexl::write_xlsx(trp_with_direction, path = "punkter_med_retningsnavn.xlsx")


# TRS' first commission ----

trs_commissions <- get_trs_trp_commissions_httr()

first_commission <- trs_commissions %>%
  dplyr::group_by(trs_id) %>%
  dplyr::slice_min(commission_from, with_ties = FALSE)

writexl::write_xlsx(first_commission, path = "stasjoners_forste_igangsetting.xlsx")


# All TRSs and their TRPs ----
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

# All TRS info ----
trs_info_all <- get_trs_info()
writexl::write_xlsx(trs_info_all, path = "trs_trp/all_stations.xlsx")

trs_info_all <-
  readxl::read_xlsx(path = "trs_trp/all_stations.xlsx")

# TRS history ----
# History of commission, device:
trs_history <- get_trs_history()

# But not all at once...

# Firmware-history must be fetched from its own endpoint in Datamottak:
firmware_history <- get_firmware_history()

saveRDS(firmware_history, file = "trs_trp/firmware_history.rds")

types_of_fw_used <- firmware_history %>%
  dplyr::select(firmware_version, device_name) %>%
  dplyr::distinct()


# TRS device type history ----
trs_device_history <- get_trs_device()

# not interested in when
trs_device_types <-
  trs_device_history %>%
  dplyr::select(
    trs_id,
    device_type
  ) %>%
  dplyr::distinct()

# to find trp device history
trs_trp_id <- get_trs_and_trp_id()

trp_device_history <-
  trs_trp_id %>%
  dplyr::select(
    trp_id,
    trs_id
  ) %>%
  dplyr::left_join(
    trs_device_history,
    by = "trs_id"
  ) %>%
  dplyr::filter(
    !is.na(trp_id),
    !is.na(device_type)
  )

saveRDS(trp_device_history,
        file = "trs_trp/trp_device_types.rds")

trp_device_history <-
  readRDS(
    file = "trs_trp/trp_device_types.rds"
    )

# TRS with EMU ----
recalculated_trs <-
  read.csv2(
    "H:/Programmering/R/byindeks/recalculated_trs.csv"
  ) %>%
  dplyr::mutate(
    trs_id = as.character(trs_id)
  )

trs_with_emu <-
  trp_device_history %>%
  dplyr::filter(
    device_type == "EMU",
    # Some trps are not public
    trp_id %in% trp$trp_id
  ) %>%
  tidyr::replace_na(
    list(
      valid_to = lubridate::today()
    )
  ) %>%
  dplyr::mutate(
    valid_from =
      lubridate::floor_date(
        valid_from,
        unit = "day"
      ),
    valid_to =
      lubridate::floor_date(
        valid_to,
        unit = "day"
    ),
    start = as.character(valid_from),
    end = as.character(valid_to),
    first_day_in_2021 =
      dplyr::case_when(
        start < "2021-01-01" ~ "2021-01-01",
        start < "2022-01-01" ~ start,
        TRUE ~ NA_character_
      ),
    last_day_in_2021 =
      dplyr::case_when(
        end > "2021-12-31" ~ "2021-12-31",
        end > "2021-01-01" ~ end,
        TRUE ~ NA_character_
      ),
    first_day_in_2021 = as.Date(first_day_in_2021),
    last_day_in_2021 = as.Date(last_day_in_2021)
  ) %>%
  tidyr::replace_na(
    list(
      first_day_in_2021 = as.Date("2021-12-31"),
      last_day_in_2021 = as.Date("2021-01-01")
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    n_days =
      length(
        seq(
          valid_from,
          valid_to,
          by = "day"
        )
      ),
    n_days_2021 =
      length(
        seq(
          first_day_in_2021,
          last_day_in_2021,
          by = "day"
        )
      ),
    n_days_2021 =
      replace(
        n_days_2021,
        last_day_in_2021 == "2021-01-01",
        0
      ),
    n_days_2021 =
      replace(
        n_days_2021,
        first_day_in_2021 == "2021-12-31",
        0
      )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    trs_id,
    trp_id,
    n_days,
    n_days_2021
  ) %>%
  dplyr::group_by(
    trs_id,
    trp_id
  ) %>%
  dplyr::summarise(
    n_days = sum(n_days),
    n_days_2021 = sum(n_days_2021)
  ) %>%
  dplyr::group_by(
    trs_id,
    n_days,
    n_days_2021
  ) %>%
  dplyr::summarise(
    n_trp = n()
  ) %>%
  dplyr::left_join(
    trs_info_all,
    by = "trs_id"
  ) %>%
  dplyr::select(
    geo_no,
    county_name,
    municipality_name,
    trs_id,
    name,
    road_category,
    road_reference,
    status,
    traffic_type,
    registration_frequency,
    n_days,
    n_days_2021,
    n_trp
  ) %>%
  dplyr::filter(
    traffic_type == "VEHICLE"
  ) %>%
  dplyr::arrange(
    geo_no,
    desc(n_days_2021)
  ) %>%
  dplyr::left_join(
    recalculated_trs,
    by = "trs_id"
  )

writexl::write_xlsx(
  trs_with_emu,
  path = "O:/ToS/Utv/DKA40 Transportdata/05. Trafikkdata/Spesialuttak/stasjoner_med_emu3.xlsx"
)

recalculated_trs <- trs_with_emu %>%
  dplyr::filter(
    recalculated == TRUE
  )

to_be_recalculated_trs <- trs_with_emu %>%
  dplyr::filter(
    recalculated != TRUE | is.na(recalculated),
    n_days_2021 > 180
  )

nrow(recalculated_trs) /
  nrow(trs_with_emu) * 100


# TRP with EMU3 or LM ----
trp_info <- trp %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    traffic_type,
    road_category_and_number,
    county_name,
    municipality_name,
    registration_frequency,
    operational_status
  ) %>%
  dplyr::distinct(trp_id, .keep_all = T)

trp_with_lm <-
  trp_device_history %>%
  dplyr::filter(
    device_type == "LOOP_MONITOR",
    # Some trps are not public
    trp_id %in% trp$trp_id
  ) %>%
  tidyr::replace_na(
    list(
      valid_to = lubridate::today()
    )
  ) %>%
  dplyr::mutate(
    valid_from =
      lubridate::floor_date(
        valid_from,
        unit = "day"
      ),
    valid_to =
      lubridate::floor_date(
        valid_to,
        unit = "day"
      ),
    start = as.character(valid_from),
    end = as.character(valid_to),
    first_day_in_2022 =
      dplyr::case_when(
        start < "2022-01-01" ~ "2022-01-01",
        start < as.character(lubridate::today()) ~ start,
        TRUE ~ NA_character_
      ),
    last_day_in_2022 =
      dplyr::case_when(
        end > as.character(lubridate::today()) ~
          as.character(lubridate::today()),
        end > "2022-01-01" ~ end,
        TRUE ~ NA_character_
      ),
    first_day_in_2022 = as.Date(first_day_in_2022),
    last_day_in_2022 = as.Date(last_day_in_2022)
  ) %>%
  tidyr::replace_na(
    list(
      first_day_in_2022 = as.Date(lubridate::today()),
      last_day_in_2022 = as.Date("2022-01-01")
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    n_days =
      length(
        seq(
          valid_from,
          valid_to,
          by = "day"
        )
      ),
    n_days_2022 =
      length(
        seq(
          first_day_in_2022,
          last_day_in_2022,
          by = "day"
        )
      ),
    n_days_2022 =
      replace(
        n_days_2022,
        last_day_in_2022 == "2022-01-01",
        0
      ),
    n_days_2022 =
      replace(
        n_days_2022,
        first_day_in_2022 == as.character(lubridate::today()),
        0
      )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    trs_id,
    trp_id,
    n_days,
    n_days_2022
  ) %>%
  dplyr::group_by(
    trs_id,
    trp_id
  ) %>%
  dplyr::summarise(
    n_days = sum(n_days),
    n_days_2022 = sum(n_days_2022)
  ) %>%
  dplyr::left_join(
    trp_info,
    by = "trp_id"
  ) %>%
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    n_days_2022 > 10
  ) %>%
  dplyr::arrange(
    county_name
  )


dt <-
  get_dt_for_trp_list(
    trp_with_lm$trp_id,
    "2022-01-01T00:00:00+01:00",
    "2022-02-01T00:00:00+01:00"
  ) %>%
  dplyr::select(
    trp_id = point_id,
    date = from,
    volume = total_volume,
    coverage
  )

dt_lm <-
  dt %>%
  dplyr::left_join(
    trp_with_lm,
    by = "trp_id"
  ) %>%
  dplyr::select(
    county_name,
    municipality_name,
    trs_id,
    trp_id,
    name,
    road_category_and_number,
    date,
    volume,
    coverage
  )

dt_lm %>%
  dplyr::mutate(
    date = as.character(date)
  ) %>%
  writexl::write_xlsx(
    path =
    "O:/ToS/Utv/DKA40 Transportdata/05. Trafikkdata/Spesialuttak/trp_med_lm_dt_2022.xlsx"
  )


# TRS and sensorconfig errors ----
sensorconfig_errors <- get_all_trs_with_trp_via_sensorconfig() %>%
  dplyr::filter(purrr::map_lgl(errors, ~!rlang::is_empty(.x)))

# to filter out accepted errors (updated 2021-04-12)
accepted_errors <- c("3000090",
                     "200062",
                     "1200236",
                     "1200238",
                     "1200258",
                     "1900114",
                     "1200129",
                     "300171",
                     "300172",
                     "300143",
                     "300146",
                     "300221",
                     "300222",
                     "300200",
                     "300201",
                     "300226",
                     "300227",
                     "3000095",
                     "3000094",
                     "3000428",
                     "300228",
                     "300229",
                     "3000482",
                     "3000507",
                     "3000511",
                     "3000397",
                     "3000398",
                     "3000502")

sensorconfig_errors_filtered <- sensorconfig_errors %>%
  dplyr::filter(station_type == "CONTINUOUS",
                !(trs_id %in% accepted_errors),
                operational_status %in% c("OPERATIONAL",
                                          "NON-OPERATIONAL")) %>%
  dplyr::arrange(as.numeric(trs_id))


## Missing lanes ----
trs_with_missing_lanes <- sensorconfig_errors %>%
  dplyr::filter(str_detect(errors, "mangler"))

points <- get_points_from_trp_api() %>%
  dplyr::select(trp_id, road_reference, county_name, municipality_name)


## TRS with TRP per direction ----
trs_with_trp_per_direction <- trs_with_missing_lanes %>%
  dplyr::left_join(points) %>%
  dplyr::filter(trs_id %in% c("3000094",
                              "3000095",
                              "3000428",
                              "300143",
                              "300146",
                              "300171",
                              "300172",
                              "300200",
                              "300201",
                              "300507",
                              "300511",
                              "300226",
                              "300227",
                              "300228",
                              "300229",
                              "300221",
                              "300222",
                              "300502",
                              "3000090")) %>%
  # Manually group trps
  dplyr::arrange(road_reference) %>%
  dplyr::select(trp_id, trp_name, road_reference, county_name, municipality_name) %>%
  tibble::rowid_to_column("row_id") %>%
  dplyr::mutate(virtual_trp = (2 * row_id + 1 + (-1)^(row_id + 1)) / 4) %>%
  dplyr::select(-row_id)

trp_per_direction_aadt <- get_aadt_by_length_for_trp_list(trs_with_trp_per_direction$trp_id)

trp_per_direction_aadt_filtered <- trp_per_direction_aadt %>%
  dplyr::select(trp_id, year, length_range, aadt_length_range, aadt_valid_length,
                aadt_total, coverage) %>%
  dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)"),
                coverage > 50) %>%
  dplyr::mutate(length_quality = round(100 * aadt_valid_length / aadt_total, digits = 1)) %>%
  dplyr::select(-aadt_valid_length) %>%
  dplyr::mutate(length_range = dplyr::case_when(
    length_range == "[..,5.6)" ~ "lette",
    length_range == "[5.6,..)" ~ "tunge"
  ))

virtual_trps <- trs_with_trp_per_direction %>%
  dplyr::left_join(trp_per_direction_aadt_filtered) %>%
  dplyr::group_by(virtual_trp, year, length_range) %>%
  dplyr::summarise(aadt_length_range = sum(aadt_length_range),
                   virtual_aadt_total = sum(aadt_total),
                   virtual_coverage = round(mean(coverage), digits = 1),
                   virtual_length_quality = round(mean(length_quality), digits = 1)) %>%
  tidyr::pivot_wider(names_from = length_range, values_from = aadt_length_range,
                     names_prefix = "virtual_aadt_") %>%
  dplyr::mutate(virtual_andel_tunge = round(100 * virtual_aadt_tunge / virtual_aadt_total, digits = 0))

trp_with_aadt <- trp_per_direction_aadt_filtered %>%
  tidyr::pivot_wider(names_from = length_range, values_from = aadt_length_range,
                     names_prefix = "aadt_") %>%
  dplyr::mutate(andel_tunge = round(100 * aadt_tunge / aadt_total, digits = 0))

virtual_trps_with_aadt <- trs_with_trp_per_direction %>%
  dplyr::left_join(trp_with_aadt) %>%
  dplyr::left_join(virtual_trps, by = c("virtual_trp" = "virtual_trp", "year" = "year"))

writexl::write_xlsx(virtual_trps_with_aadt,
                    path = "adt_rapporter/virtuelle_punkter_fra_delretning.xlsx")


# TRP with lanes and fw ----
# OBS! lanes from last commission, so not always correct with former fw
trp_lanes_fw <- firmware_history %>%
  dplyr::left_join(trs_and_trp_id, by = c("trs_id")) %>%
  dplyr::left_join(trp_tidy, by = c("trp_id")) %>%
  dplyr::filter(trs_status == "OPERATIONAL",
                trs_type == "CONTINUOUS",
                traffic_type == "VEHICLE") %>%
  dplyr::select(trs_id, trs_name,
                trp_id, trp_name = name,
                road_reference,
                lanes,
                county_name, municipality_name,
                device_name,
                fw_version = firmware_version,
                fw_valid_from = valid_from
                )

writexl::write_xlsx(trp_lanes_fw, path = "trs_trp/trp_lanes_fw.xlsx")


# Periodic TRPs ----
all_trps <- get_points()

periodic_trps <- all_trps %>%
  dplyr::filter(registration_frequency == "PERIODIC",
                validFrom >= "2018-01-01") %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice_max(validFrom)

test_dt <- get_daily_traffic(
  "59044V971518",
  "2019-01-01T00:00:00+01:00",
  "2019-12-31T00:00:00+01:00"
  )

test_aadt <- test_dt %>%
  dplyr::filter(coverage >= 95) %>%
  dplyr::summarise(aadt = mean(total_volume))


sykkel_tromso <- all_trps %>%
  dplyr::filter(traffic_type == "BICYCLE",
                municipality_name == "Tromsø") %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice_min(validFrom)


