#
# FAKTROKURVER
# Beregne nye faktorkurver basert på nye data.
#

# Pakker ####
library(ghql)
library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)

# Angir APIets URL
cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query="
)

# Definerer spørringen med msnr som variabel
msnr <- '1601405'
query_del1 <- '{
api(apiKey: "0985BA1D-813A-4003-BBFA-580F3D5E9111") {
station(measurePointNumber: '
query_del2_dogn <- ') {
volumes(
from: "2017-01-01",
to: "2017-02-01",
interval: 1,
unit: DAY,
groupBy: STATION) {
intervalStart
traffic {
lane
directionIsReverse
totalVolume
}
}
}
}
}'
query_del2_time <- ') {
volumes(
from: "2017-01-01",
to: "2017-02-01",
interval: 1,
unit: HOUR,
groupBy: LANE) {
intervalStart
traffic {
lane
directionIsReverse
totalVolume
}
}
}
}
}'

hentDogntrafikk <- function(msnr) {

  # Lager spørringen
  sporringer <- Query$new()
  sporringer$query("dogntrafikk", paste0(query_del1, msnr, query_del2_dogn))

  # Utfører spørringen
  dogntrafikk <- cli$exec(sporringer$queries$dogntrafikk) %>%
    fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    unnest() %>%
    rename(intervallstart = data.api.station.volumes.intervalStart,
           felt = lane,
           retning = directionIsReverse,
           trafikkmengde = totalVolume) %>%
    mutate(intervallstart = with_tz(ymd_hm(intervallstart), "CET"))

  adt_g <- dogntrafikk %>%
    summarise(adt = round(mean(trafikkmengde), digits = 0),
              std = round(sd(trafikkmengde), digits = 0),
              kfi = round(1.96 * std / n(), digits = 0),
              antall = n(),
              dekningsgrad = round((antall / 365) * 100, digits = 0))

  return(adt_g)
}

hentTimetrafikk <- function(msnr) {

  # Lager spørringen
  sporringer <- Query$new()
  sporringer$query("timetrafikk", paste0(query_del1, msnr, query_del2_time))

  # Utfører spørringen
  timetrafikk <- cli$exec(sporringer$queries$timetrafikk) %>%
    fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    unnest() %>%
    rename(intervallstart = data.api.station.volumes.intervalStart,
           felt = lane,
           retning = directionIsReverse,
           trafikkmengde = totalVolume) %>%
    mutate(intervallstart = with_tz(ymd_hm(intervallstart), "CET"))

  return(timetrafikk)
}

#
# Slutt.
#
