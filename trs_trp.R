# TRS and TRP

library(tidyverse)
library(jsonlite)
library(httr)
library(ghql)
library(geosphere)
library(writexl)

source("H:/Programmering/R/byindeks/get_from_trp_api.R")

# Large distance between trs and trp ####
trs_trp <- get_stations_and_trps_with_coordinates_from_TRPAPI_httr()

trs_trp_distance <- trs_trp %>%
  dplyr::mutate(distance = round(
                  by(trs_trp, 1:nrow(trs_trp),
                     function(row) {distGeo(c(row$stasjon_lon, row$stasjon_lat),
                                            c(row$punkt_lon, row$punkt_lat))
                  }),
                  digits = 1)) #%>%
  #dplyr::filter(distance > 100)

write.csv2(trs_trp_distance, file = "stasjon_punkt_avstand.csv",
           row.names = F)

# Points ####
trp <- getPointsFromTRPAPI()
trp_with_commissions <- get_trp_with_commissions()
#trp_trs <- get_trs_trp()
trs_with_trp <- get_all_trs_with_trp()

# trp_id and legacies
trs_with_trp %>%
  dplyr::filter(!is.na(legacyNortrafMpn)) %>%
  write.csv2(file = "trp_id_and_nortraf_mpn.csv",
             row.names = F)

# Stations with AADT in Nortraf in 2014, 2013 or 2012
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
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::filter(traffic_type == "BICYCLE") %>%
  dplyr::select(trp_id, name, road_reference, county_name, municipality_name,
                validFrom, validTo) %>%
  dplyr::arrange(road_reference) %>%
  writexl::write_xlsx(path = "sykkelpunkter.xlsx")

# Periodic trps ####
periodic_trps <- get_periodic_trps()

parse_nortrafweb_csv <- function(filename, year) {
  # Parses hellish nortrafweb-csvswith aadts to a tibble
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

