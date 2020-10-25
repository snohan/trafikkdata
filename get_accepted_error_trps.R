# Extract trp_ids from AcceptedErrors-list

library(tidyverse)

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

accepted <- readr::read_lines("unntakslista.txt") %>%
  tibble::tibble() %>%
  dplyr::rename(strings = 1) %>%
  dplyr::mutate(has_trp_id = stringr::str_detect(strings, "TrpId.*")) %>%
  dplyr::filter(has_trp_id == TRUE) %>%
  dplyr::mutate(shortened = stringr::str_sub(strings, 32, -3)) %>%
  tidyr::separate(shortened, into = c("trp_id", "one", "two"), sep = "asList") %>%
  dplyr::select(trp_id) %>%
  dplyr::mutate(trp_id = gsub("[[:punct:][:blank:]]+", "", trp_id)) %>%
  dplyr::distinct()

points <- get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T)

trp_meta <- accepted %>%
  dplyr::left_join(points) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no")) %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name,
                road_category_and_number,
                county_name, municipality_name,
                #direction_with, direction_against,
                number_of_directions)
  #dplyr::mutate(lane_numbers = stringr::str_c(lane_numbers, collapse = ", "))

write.csv2(trp_meta, file = "punkter_uten_alle_felt.csv",
           row.names = F)
