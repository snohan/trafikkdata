# Get OSM data

{
  library("tidyverse")
  library("sf")
  library("osmdata")
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")  
}


# Match OSM id to TRP id ----

## TRPs ----
trps <- 
  get_points() |> 
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS"
  ) |> 
  dplyr::select(
    trp_id, lat, lon
  ) |> 
  dplyr::distinct()

# But need only TRPs with recent data of good quality
trp_latest_date <- 
  get_trps_latest_data() |> 
  dplyr::filter(
    latest_data_by_hour > "2026-05-01"
  )


## Get OSM id ----
get_osm_id_for_trp <- function(trp_id, lat, lon) {

  result <-
    osmdata::opq_around(
      lat = lat,
      lon = lon,
      key = "highway",
      radius = 5
    ) |> 
    osmdata::osmdata_sf() |> 
    dplyr::mutate(trp_id = trp_id)

  return(result)

}





#
test_punkt <- 
  # c(63.3932674, 10.4143456)
  c(63.4055996, 10.4000711)

test_data <- 
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



q <- 
  opq("trondheim, norway") |>
  add_osm_feature(key = "highway", value = "cycleway") |> 
  osmdata_sf()


available_features()
