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


get_vehicle_info <- function() {

  # TODO: technical code as input
  api_base_url <- "https://hotell.difi.no/api/json/vegvesen/kjoretoy?tekn_reg_status=Registrert&tekn_tknavn=N3&page="

  i = 1
  start_url <- paste0(api_base_url, i)

  # Need to do one initial query to determine number of pages
  response <- httr::GET(start_url,
                        httr::add_headers(.headers = data_norge_headers))

  number_of_pages <- response$headers$`x-datahotel-total-pages` %>%
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

    vehicle_entries_new <- parsed_response$entries

    vehicle_entries <- dplyr::bind_rows(vehicle_entries,
                                        vehicle_entries_new)
    i = i + 1
  }

  return(vehicle_entries)
}



