#
# FACTOR CURVES
# Calculate new factor curves based on our own data.
#

# Packages ####
#.libPaths( c( .libPaths(), "C://Eget/R/library") )
#.libPaths()
library(ghql)
library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(tictoc)
library(plotly)
options(stringsAsFactors = F)

# Functions ####

cli <- GraphqlClient$new(
  url = "https://www.vegvesen.no/trafikkdata/api/?query="
)

getPoints <- function() {
  # Get all traffic registration points
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
      fromJSON(simplifyDataFrame = T, flatten = T)

    if(length(trafficData$data$trafficData$volume$byHour$edges) == 0)
      break;

    trafficData %<>% as.data.frame()

    cursor <-
      trafficData$data.trafficData.volume.byHour.pageInfo.endCursor[1] %>%
      as.character()
    hasNextPage <-
      trafficData$data.trafficData.volume.byHour.pageInfo.hasNextPage[1]

    trafficData %<>% select(1:4)

    hourlyTraffic <- bind_rows(hourlyTraffic, trafficData)
  }

  if(nrow(hourlyTraffic) == 0) {
    hourlyTraffic <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                              c("point_id", "point_name", "hour_from",
                                "total_volume"))
  }else{
    colnames(hourlyTraffic) <- c("point_id", "point_name", "hour_from",
                                 "total_volume")
    hourlyTraffic %<>% mutate(hour_from = with_tz(ymd_hms(hour_from), "CET"))
  }

  return(hourlyTraffic)
}

calculatefactorCurve <- function(hourlyValues) {
  # A factor curve for a whole year is calculated by
  # dividing yearly hour traffic through yearly traffic
  # TODO: yearly traffic per point and then join to hourly values
  yearlyValues <- hourlyValues %>%
    group_by(point_id) %>%
    summarise(yearly_traffic = sum(total_volume))

  hourlyValues %<>%
    mutate(hour_of_day = hour(hour_from))  %>%
    group_by(point_id, hour_of_day) %>%
    summarise(yearly_hour_traffic = sum(total_volume)) %>%
    left_join(yearlyValues, by = c("point_id")) %>%
    mutate(hourly_factor = yearly_hour_traffic / yearly_traffic)
}

nationalFactorCurve <- function(hourlyValues) {
  yearly_traffic = sum(hourlyValues$yearly_hour_traffic)

  hourlyValues %<>%
    group_by(hour_of_day) %>%
    summarise(yearly_hour_traffic = sum(yearly_hour_traffic)) %>%
    mutate(hourly_factor = yearly_hour_traffic / yearly_traffic)
}

getTrafficDataForpoints <- function(trp_list, start, end) {
  number_of_points <- length(trp_list)
  data_points <- data.frame()
  trp <- 1

  while (trp <= number_of_points) {
    data_points <- bind_rows(data_points,
                             getHourlytraffic(trp_list[trp], start, end))
    trp <- trp + 1
  }
  return(data_points)
}

#
# OLD CODE ####
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

points_for_vehicle <- getPoints() %>%
  filter(traffic_type == "VEHICLE")

# 2017 is in focus for this analysis
interval_start <- "2017-01-01T00:00:00+01:00"
interval_end   <- "2018-01-01T00:00:00+01:00"

# Taking a few points at a time
# 300 a time should be safe!
tic()
hourlyTrafficVolume <- getTrafficDataForpoints(points_for_vehicle[201:300,1],
                                               interval_start,
                                               interval_end)
toc()

numberOfhours_2017 <- hourlyTrafficVolume %>%
  group_by(point_id) %>%
  summarise(no_hours = n()) %>%
  filter(no_hours == 8760)

hourlyTrafficVolume_to_add <-
  hourlyTrafficVolume %>%
  filter(point_id %in% numberOfhours_2017$point_id)

#hourlyTrafficVolume_2017 <- data.frame()
hourlyTrafficVolume_2017 <-
  bind_rows(hourlyTrafficVolume_2017, hourlyTrafficVolume_to_add)

length(unique(hourlyTrafficVolume_2017$point_id))

# Calculate the factor curves
factorCurve <- calculatefactorCurve(hourlyTrafficVolume_2017)

factorPlot <- factorCurve %>%
  filter(!(point_id %in% deviatingPoints)) %>%
  ggplot(aes(hour_of_day, hourly_factor, color = point_id)) +
  geom_line()

ggplotly(factorPlot)


# Deviating curves seen in plot:
# 71241V2460301: Kong Håkons gate: Kø?
# 96529V885935: Skjellesvikskaret: lav ÅDT
# 88780V2282345: Ørbekk sørgående rampe: forsinket rushtime?
# 991115V249438: Årøsæterlia: ?
# 13278V121819: Aukland vegstasjon: lav ÅDT
# 50043V885183: Trollvann: lav ÅDT
# 85303V886129: Svolvær lufthavn: lav ÅDT, kun flyplasstrafikk
# 16467V249421: Spjelkavik sør: omkjøringsvei tunnelstenging?
# 77194V72348: Hitratunnelen: lav ÅDT

deviatingPoints <- c("96529V885935", "71241V2460301",
                     "85303V886129", "16467V249421",
                     "77194V72348")

write.csv2(factorCurve, file = "faktorkurver_1_300.csv",
           row.names = F)

nationalCurve <- factorCurve %>% nationalFactorCurve()
# Ligner mest på M3, men er mer spisset!

# TODO: Interactive plot
# TODO: Remove deviating curves
# TODO: One point's 365 curves in one plot
# TODO: One plot per weekday
# TODO: One factor curve based on all points with 8760 values in 2017.
# TODO: Filter out points based on operational status.

#
# END.
#