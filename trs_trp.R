# TRS and TRP

library(tidyverse)
library(jsonlite)
library(httr)
library(ghql)
library(geosphere)

#source("get_from_trp_api.R")

trs_trp <- get_stations_and_trps_with_coordinates_from_TRPAPI()

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

# Points without stations
trp <- getPointsFromTRPAPI()

trp_without_trs <- dplyr::anti_join(trp, trs_trp, by = c("trp_id" = "punktnr"))
# TODO: filter out stations without commissions, or why are they not part of
# station-query

