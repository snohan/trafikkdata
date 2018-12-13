#
# FACTOR CURVES
# Calculate new factor curves based on our own data.
#

# Packages ####
library(ghql)
library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)
library(magrittr)
options(stringsAsFactors = F)

# Functions ####

# Our API's URL
cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query="
)

# Get all traffic registration points

getPoints <- function() {

  # The GraphQL Query
  query_points <-
  "{
  trafficRegistrationPoints {
    id
    name
    trafficRegistrationType
  }
  }"

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  points <- cli$exec(myqueries$queries$points) %>%
    fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    rename(point_id = data.trafficRegistrationPoints.id,
           point_name = data.trafficRegistrationPoints.name,
           traffic_type =
             data.trafficRegistrationPoints.trafficRegistrationType)

  return(points)
}

# TODO: oppdater resten av koden!
trpID <- "43623V704583"
from <- "2017-01-01T00:00:00+01:00"
to <-   "2017-01-15T00:00:00+01:00"

getHourlytraffic <- function(trpID, from, to) {
  # Default values
  hasNextPage <- TRUE
  cursor <- ""
  hourlyTraffic <- data.frame()

  build_query <- function() {
    query_hourlyTraffic <- paste0(
      '{
    trafficData(trafficRegistrationPointId: "',
      trpID,
      '"){
        trafficRegistrationPoint {
          id
          name
        }
        volume {
        byHour(
        from: "',
      from,
      '",
        to: "',
      to,
      '",
        after: "',
      cursor,
      '"
        ) {
        edges {
          node {
            from
            total {
              volume
            }
              }
              }
              pageInfo {
                hasNextPage
                endCursor
              }
              }
              }
            }
        }
      ')
  }

  while(hasNextPage == TRUE){

    myqueries <- Query$new()
    myqueries$query("hourlyTraffic", build_query())

    trafficData <- cli$exec(myqueries$queries$hourlyTraffic) %>%
      fromJSON(simplifyDataFrame = T, flatten = T) %>%
      as.data.frame()

    cursor <-
      trafficData$data.trafficData.volume.byHour.pageInfo.endCursor[1] %>%
      as.character()
    hasNextPage <-
      trafficData$data.trafficData.volume.byHour.pageInfo.hasNextPage[1]

    trafficData %<>% select(1:4)

    hourlyTraffic <- bind_rows(hourlyTraffic, trafficData)
  }

  colnames(hourlyTraffic) <- c("point_id", "point_name", "hour_from",
                               "total_volume")
  hourlyTraffic %<>% mutate(hour_from = with_tz(ymd_hms(hour_from), "CET"))

  return(hourlyTraffic)
}

# Testhenting av 1 side
getOnePage <- function() {

  # The GraphQL Query
myqueries <- Query$new()
myqueries$query("onepage", query_hourlyTraffic)

# Executing the query
onepage <- cli$exec(myqueries$queries$onepage) %>%
  fromJSON(simplifyDataFrame = T, flatten = T) %>%
  as.data.frame()

#return(onepage)
}

# OLD CODE ####
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

hentTimetrafikk <- function(trp_id) {

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


  return(timetrafikk)
}

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



# Utførende kode ####

# Alle punkter for bil
points_for_vehicle <- getPoints() %>%
  filter(traffic_type == "VEHICLE")

# Timeverdier for et punkt
hourlyTrafficVolume <- getHourlytraffic(
  "43623V704583",
  "2017-01-01T00:00:00+01:00",
  "2017-12-31T00:00:00+01:00")

#
# END.
#