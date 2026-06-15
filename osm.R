# Get OSM data

{
  library("tidyverse")
  library("sf")
  library("osmdata")
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")  
}


# Match OSM id to TRP id ----

# Finn først OSM Way id for hver TRP
# Må finne INRIX sin segmenterte OSM Way id, segmentid, basert på geometri-matching, men filtrert på way_id for å spare regnetid

## TRPs ----
trps <- 
  get_points() |> 
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED"
  ) |> 
  dplyr::select(
    trp_id, lat, lon
  ) |> 
  dplyr::distinct()



## Get OSM id ----
get_osm_id <- function(lat, lon) {

  result <-
    osmdata::opq_around(
      lat = lat,
      lon = lon,
      key = "highway",
      radius = 5
    ) |> 
    osmdata::osmdata_sf()

  osm_id <- result$osm_lines$osm_id

  return(osm_id)

}


trp_osm <-
  trps |> 
  dplyr::slice(1:3) |> 
  dplyr::mutate(
    osm_id = purrr::map2(lat, lon, ~ get_osm_id(.x, .y))
  )
  




#
test_punkt <- 
  # c(63.3932674, 10.4143456)
  c(63.4055996, 10.4000711)

test_data_2 <- 
  osmdata::opq_around(
    lat = test_punkt[1],
    lon = test_punkt[2],
    key = "highway",
    # value = "residential",
    radius = 5
  ) |> 
  # osmdata::add_osm_feature(key = "highway") 
  # osmdata::osmdata_data_frame()
  osmdata::osmdata_sf()

result <- get_osm_id(test_punkt[1], test_punkt[2])


test_data_2 <- 
  osmdata::opq_around(
    lat = trps$lat[3],
    lon = trps$lon[3],
    key = "highway",
    # value = "residential",
    radius = 5
  ) |> 
  # osmdata::add_osm_feature(key = "highway") 
  # osmdata::osmdata_data_frame()
  osmdata::osmdata_sf()


# Look at OSM map: https://www.openstreetmap.org/?way=[YOUR_WAY_ID]