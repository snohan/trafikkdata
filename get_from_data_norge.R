# Get data from data.norge.no
# Kjøretøyopplysninger

data_norge_headers <- c(
  "X-Client" = "Statens vegvesen, trafikkdatagruppa",
  "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
  "Accept" = "application/json"
)


get_vehicle_groups <- function() {

  response <-
    readr::read_csv2("https://hotell.difi.no/api/csv/vegvesen/kjoretoygruppe?")
}


get_technical_codes <- function() {

  response <-
    readr::read_csv2("https://hotell.difi.no/api/csv/vegvesen/teknisk-kode?")
}


get_vehicle_info_fields <- function() {

  response <- httr::GET("https://hotell.difi.no/api/json/vegvesen/kjoretoy/fields",
                        httr::add_headers(.headers = data_norge_headers))

  parsed_response <- jsonlite::fromJSON(
    stringr::str_conv(
      response$content, encoding = "UTF-8"),
    simplifyDataFrame = T,
    flatten = T)

}

traffic_data_relevant_columns <- c("tekn_aksler",
                                   #"tekn_aksler_drift",
                                   "tekn_avreg_dato",
                                   "tekn_drivst",
                                   "tekn_egenvekt",
                                   #"tekn_euronorm_ny",
                                   #"tekn_hybrid",
                                   #"tekn_hybrid_kategori",
                                   #"tekn_kjtgrp",
                                   #"tekn_last_1",
                                   #"tekn_last_2",
                                   #"tekn_last_3",
                                   "tekn_lengde",
                                   #"tekn_luft_1",
                                   #"tekn_luft_2",
                                   #"tekn_luft_3",
                                   "tekn_merkenavn",
                                   #"tekn_minavst_ms1",
                                   #"tekn_minavst_ms2",
                                   "tekn_modell",
                                   "tekn_reg_aar",
                                   "tekn_reg_status",
                                   #"tekn_thv_m_brems",
                                   #"tekn_thv_u_brems",
                                   "tekn_tknavn",
                                   "tekn_totvekt",
                                   "tekn_vogntogvekt"
                                   )

#technical_code <- "O1"
get_vehicle_info <- function(technical_code) {

  api_base_url <- paste0("https://hotell.difi.no/api/json/vegvesen/kjoretoy?tekn_reg_status=Registrert&tekn_tknavn=",
                         technical_code,
                         "&page=")

  i = 1
  start_url <- paste0(api_base_url, i)

  # Need to do one initial query to determine number of pages
  response <- httr::GET(start_url,
                        httr::add_headers(.headers = data_norge_headers))

  number_of_pages <- response$headers$`x-datahotel-total-pages` %>%
    as.numeric()

  number_of_vehicles <- response$headers$`x-datahotel-total-posts` %>%
    as.numeric()

  vehicle_entries <- tibble::tibble()

  while (i <= number_of_pages) {

    new_url <- paste0(api_base_url, i)

    response <- httr::GET(new_url,
                          httr::add_headers(.headers = data_norge_headers))

    parsed_response <- jsonlite::fromJSON(
      stringr::str_conv(
        response$content, encoding = "UTF-8"),
      simplifyDataFrame = T,
      flatten = T)

    vehicle_entries_new <- parsed_response$entries %>%
      dplyr::select(tidyselect::all_of(traffic_data_relevant_columns))

    vehicle_entries <- dplyr::bind_rows(vehicle_entries,
                                        vehicle_entries_new)
    i = i + 1
  }

  return(vehicle_entries)
}

# TODO: a function that fetches number of vehicles per group, by querying for the first page per group

