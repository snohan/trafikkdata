# Fetch vehicle data from Kjøretøyregisteret

library(tidyverse)
library(httr)
library(jsonlite)

# URL and headers ----
api_url <- "https://www.vegvesen.no/ws/no/vegvesen/kjoretoy/felles/datautlevering/enkeltoppslag/kjoretoydata"

source("vehicle_registry_api_key.R") # Cannot pe public - personal access key

# Queries ----

#vehicle_registration_number <- "DN11184"
get_vehicle_data <- function(vehicle_registration_number) {

  # vehicle_registration_number STRING

  api_query <-
    paste0(
      api_url,
      "?kjennemerke=",
      vehicle_registration_number
    )

  api_response <-
    httr::GET(
      api_query,
      httr::add_headers(.headers = api_headers)
    )

  parsed <-
    jsonlite::fromJSON(
      stringr::str_conv(
        api_response$content, encoding = "UTF-8"
      ),
      simplifyDataFrame = T,
      flatten = T
      ) %>%
    purrr::pluck("kjoretoydataListe") %>%
    tibble::as_tibble()

  if(nrow(parsed) == 0 | ncol(parsed) < 2) {
    vehicle_info = data.frame()
  } else {
    vehicle_info <- parsed %>%
      tidyr::unnest_wider(
        godkjenning.tekniskGodkjenning.tekniskeData.generelt.merke
      ) %>%
      dplyr::select(
        vehicle_id = kjoretoyId.kjennemerke,
        manufacturer = merke,
        model = godkjenning.tekniskGodkjenning.tekniskeData.generelt.handelsbetegnelse,
        class_name = godkjenning.tekniskGodkjenning.kjoretoyklassifisering.tekniskKode.kodeNavn,
        class_code = godkjenning.tekniskGodkjenning.kjoretoyklassifisering.tekniskKode.kodeVerdi,
        length = godkjenning.tekniskGodkjenning.tekniskeData.dimensjoner.lengde,
        axles = godkjenning.tekniskGodkjenning.tekniskeData.akslinger.antallAksler
      ) %>%
      dplyr::mutate(
        vehicle_id =
          stringr::str_replace_all(
            vehicle_id,
            " ",
            ""
          )
      )
  }
}


#vehicle_id_list <- c(
#  "VH 81912",
#  "HF16969"
#)
get_vehicle_data_for_list <- function(vehicle_id_list) {

  # vehicle_id_list STRING column vector

  number_in_list <- length(vehicle_id_list)
  data_points <- data.frame()
  list_count <- 1

  while (list_count <= number_in_list) {

    data_points <- dplyr::bind_rows(
      data_points,
      get_vehicle_data(
        vehicle_id_list[list_count]
      )
    )

    list_count <- list_count + 1

  }

  return(data_points)
}



