# List of TRPs in need of labels

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

library(writexl)

# TRP stats ----
trp <- get_points()

distinct_trps <- trp %>%
  split_road_system_reference() %>%
  dplyr::select(
    county_geono,
    county_name,
    municipality_name,
    road_category,
    trp_id,
    name,
    road_reference,
    traffic_type,
    registration_frequency
  ) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no")
  )


# Read CSVs from Kibana ----
read_a_file <- function(file_name) {

  readr::read_csv2(
    paste0("zero_dt/", file_name)
  ) #%>%
  # dplyr::mutate(
  #   periode_start = as.character(periode_start)
  # )

}

zero_dt <-
  list.files(
    "zero_dt"
  ) %>%
  purrr::map_df(
    ~ read_a_file(.)
  )

trp_need_label <-
  zero_dt %>%
  dplyr::left_join(
    distinct_trps,
    by = "trp_id"
  ) %>%
  # Romove invisible trps (bike)
  dplyr::filter(
    !is.na(name)
  ) %>%
  dplyr::select(
    county_geono,
    county_name,
    municipality_name,
    road_category,
    name,
    traffic_type,
    registration_frequency,
    road_reference,
    lane,
    month,
    number_of_days
  ) %>%
  dplyr::arrange(
    county_geono,
    municipality_name,
    road_category,
    name,
    month
  ) %>%
  dplyr::select(
    -county_geono
  )

writexl::write_xlsx(
  trp_need_label,
  path = "O:/ToS/Utv/DKA40 Transportdata/05. Trafikkdata/Opplæring - kompetanse/7. Databehandling/nulltrafikk.xlsx"
)
