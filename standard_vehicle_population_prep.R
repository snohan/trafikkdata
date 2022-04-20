# Use vbv data from Kibana CSV files to calculate standard vehicle populations

# 1. Setup ----
library(tidyverse)

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("trafficdata_functions.R")

folder_raw <- "standard_pop/"
folder_sum <- "standard_pop_sums/"

# TODO: add:
# Torsvika

read_a_file <- function(file_name, folder_name) {

  readr::read_csv2(
    paste0(folder_name, file_name)
  ) #%>%
  #dplyr::mutate(
  #  periode_start = as.character(periode_start)
  #)

}

read_vbv_and_save_summary <- function(a_file_name) {

  # Reads a vbv file and saves an RDS containing the class sums

  # Enriching each vehicle with classification
  vbv_data_enriched <-
    a_file_name %>%
    read_a_file(folder_raw) %>%
    dplyr::select(
      trp_id = traffic_registration_point_id,
      length,
      vehicle_type_raw,
      valid_length,
      valid_classification
    ) %>%
    make_norsikt_classes() %>%
    make_length_classes()


  # Summarising each classification level (this is by trp)
  n_norsikt_l2_trp <-
    vbv_data_enriched %>%
    dplyr::filter(
      norsikt_l2 != "OMV",
      valid_classification == TRUE
    ) %>%
    dplyr::group_by(
      trp_id,
      norsikt_l2
    ) %>%
    dplyr::summarise(
      n_vehicles = n()
    ) %>%
    dplyr::mutate(
      classification_scheme = "norsikt_l2"
    ) %>%
    dplyr::rename(
      class = norsikt_l2
    )

  n_norsikt_l3_trp <-
    vbv_data_enriched %>%
    dplyr::filter(
      norsikt_l2 != "OMV",
      valid_classification == TRUE
    ) %>%
    dplyr::group_by(
      trp_id,
      norsikt_l3
    ) %>%
    dplyr::summarise(
      n_vehicles = n()
    ) %>%
    dplyr::mutate(
      classification_scheme = "norsikt_l3"
    ) %>%
    dplyr::rename(
      class = norsikt_l3
    )

  n_norsikt_l4_trp <-
    vbv_data_enriched %>%
    dplyr::filter(
      norsikt_l2 != "OMV",
      valid_classification == TRUE
    ) %>%
    dplyr::group_by(
      trp_id,
      norsikt_l4h
    ) %>%
    dplyr::summarise(
      n_vehicles = n()
    ) %>%
    dplyr::mutate(
      classification_scheme = "norsikt_l4h"
    ) %>%
    dplyr::rename(
      class = norsikt_l4h
    )

  n_length_2_trp <-
    vbv_data_enriched %>%
    dplyr::filter(
      valid_length == TRUE
    ) %>%
    dplyr::group_by(
      trp_id,
      length_class_2
    ) %>%
    dplyr::summarise(
      n_vehicles = n()
    ) %>%
    dplyr::mutate(
      classification_scheme = "length_class_2"
    ) %>%
    dplyr::rename(
      class = length_class_2
    )

  n_length_2_7.6_trp <-
    vbv_data_enriched %>%
    dplyr::filter(
      valid_length == TRUE
    ) %>%
    dplyr::group_by(
      trp_id,
      length_class_2_7.6
    ) %>%
    dplyr::summarise(
      n_vehicles = n()
    ) %>%
    dplyr::mutate(
      classification_scheme = "length_class_2_7.6"
    ) %>%
    dplyr::rename(
      class = length_class_2_7.6
    )

  n_length_class_full <-
    vbv_data_enriched %>%
    dplyr::filter(
      valid_length == TRUE
    ) %>%
    dplyr::group_by(
      trp_id,
      length_class_full
    ) %>%
    dplyr::summarise(
      n_vehicles = n()
    ) %>%
    dplyr::mutate(
      classification_scheme = "length_class_full"
    ) %>%
    dplyr::rename(
      class = length_class_full
    )

  file_summarised_data <-
    dplyr::bind_rows(
      n_norsikt_l2_trp,
      n_norsikt_l3_trp,
      n_norsikt_l4_trp,
      n_length_2_trp,
      n_length_2_7.6_trp,
      n_length_class_full
    )

  output_file_name <-
    paste0(
      folder_sum,
      stringr::str_sub(a_file_name, 1, -4),
      "rds"
    )

  saveRDS(file_summarised_data,
          file = output_file_name)

}

# TRP metadata
trp <- get_points()

distinct_trps <- trp %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    road_category,
    road_category_and_number,
    road_reference,
    county_name,
    municipality_name,
    registration_frequency,
    operational_status,
    lane_numbers
  ) %>%
  dplyr::distinct(
    trp_id,
    .keep_all = T
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    lanes = sort(lane_numbers) %>%
      paste(collapse = ", ")
  ) %>%
  ungroup() %>%
  dplyr::select(
    -lane_numbers
  )


# 2. Read vbv data ----
# All files have a prefix beginning on 001 and increasing.
# To keep track of which files have been read and included.
# So as to avoid doing the same work multiple times.
# Output file has same name, but RDS.

vbv_data_files <-
  list.files(
    folder_raw
  ) %>%
  stringr::str_sort()

read_vbv_and_save_summary(vbv_data_files[13])


# 3. Reading saved RDS ----
rds_data <-
  list.files(
    folder_sum
  ) %>%
  purrr::map_df(
    ~ readRDS(paste0(folder_sum, .))
  ) %>%
  dplyr::group_by(
    trp_id,
    classification_scheme,
    class
  ) %>%
  dplyr::summarise(
    n_vehicles = sum(n_vehicles),
    .groups = "drop"
  )


# 4. TRP info and AADT ----
trp_ids_used <-
  unique(rds_data$trp_id)

trps_used <-
  distinct_trps %>%
  dplyr::filter(
    trp_id %in% trp_ids_used
  )

aadt <-
  get_aadt_for_trp_list(
    trps_used$trp_id
  ) %>%
  dplyr::filter(
    year == 2021
  ) %>%
  dplyr::select(
    trp_id,
    adt,
    coverage,
    year
  )

trp_aadt <-
  trps_used %>%
  dplyr::left_join(
    aadt,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    county_name,
    adt,
    coverage,
    year
  )

saveRDS(trp_aadt,
        file = "standard_pop_trp_aadt.rds")


# 4. Calculating standard populations ----

# Total number of vehicles for later calculating ratios
n_vehicles_classified_by_trp <-
  rds_data %>%
  dplyr::filter(
    classification_scheme == "norsikt_l2"
  ) %>%
  dplyr::group_by(
    trp_id
  ) %>%
  dplyr::summarise(
    n_classified = sum(n_vehicles),
    .groups = "drop"
  )

n_vehicles_classified_all <-
  rds_data %>%
  dplyr::filter(
    classification_scheme == "norsikt_l2"
  ) %>%
  dplyr::summarise(
    n_classified = sum(n_vehicles),
    .groups = "drop"
  )

n_vehicles_length_classified_by_trp <-
  rds_data %>%
  dplyr::filter(
    classification_scheme == "length_class_2"
  ) %>%
  dplyr::group_by(
    trp_id
  ) %>%
  dplyr::summarise(
    n_classified = sum(n_vehicles),
    .groups = "drop"
  )

n_vehicles_length_classified_all <-
  rds_data %>%
  dplyr::filter(
    classification_scheme == "length_class_2"
  ) %>%
  dplyr::summarise(
    n_classified = sum(n_vehicles),
    .groups = "drop"
  )


## a. Standard populations for vehicle classes ----
standard_pop_class_by_trp <-
  rds_data %>%
  dplyr::filter(
    stringr::str_detect(classification_scheme, "^norsikt")
  ) %>%
  dplyr::left_join(
    n_vehicles_classified_by_trp,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    ratio = n_vehicles / n_classified
  ) %>%
  dplyr::left_join(
    trps_used,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    trp_name_and_road =
      paste0(
        name,
        ", ",
        road_category_and_number
      )
  ) %>%
  dplyr::select(
    classification_scheme,
    class,
    ratio,
    trp_name_and_road
  )

standard_pop_class_total <-
  rds_data %>%
  dplyr::filter(
    stringr::str_detect(classification_scheme, "^norsikt")
  ) %>%
  dplyr::group_by(
    classification_scheme,
    class
  ) %>%
  dplyr::summarise(
    n_vehicles = sum(n_vehicles),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    ratio = n_vehicles / n_vehicles_classified_all$n_classified,
    trp_name_and_road = "Samlet"
  ) %>%
  dplyr::select(
    -n_vehicles
  )

standard_pop_class <-
  dplyr::bind_rows(
    standard_pop_class_by_trp,
    standard_pop_class_total
  )

saveRDS(standard_pop_class,
        file = "standard_pop_class.rds")


## b. Standard populations for length classes ----
standard_pop_length_class_by_trp <-
  rds_data %>%
  dplyr::filter(
    stringr::str_detect(classification_scheme, "^length")
  ) %>%
  dplyr::left_join(
    n_vehicles_length_classified_by_trp,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    ratio = n_vehicles / n_classified
  ) %>%
  dplyr::left_join(
    trps_used,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    trp_name_and_road =
      paste0(
        name,
        ", ",
        road_category_and_number
      )
  ) %>%
  dplyr::select(
    classification_scheme,
    class,
    ratio,
    trp_name_and_road
  )

standard_pop_length_class_total <-
  rds_data %>%
  dplyr::filter(
    stringr::str_detect(classification_scheme, "^length")
  ) %>%
  dplyr::group_by(
    classification_scheme,
    class
  ) %>%
  dplyr::summarise(
    n_vehicles = sum(n_vehicles),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    ratio = n_vehicles / n_vehicles_length_classified_all$n_classified,
    trp_name_and_road = "Samlet"
  ) %>%
  dplyr::select(
    -n_vehicles
  )

standard_pop_length_class <-
  dplyr::bind_rows(
    standard_pop_length_class_by_trp,
    standard_pop_length_class_total
  )

saveRDS(standard_pop_length_class,
        file = "standard_pop_length_class.rds")





## Two length classes around 5.6 m----
n_length_2_trp <-
  enriched_data %>%
  dplyr::filter(
    valid_length == TRUE
  ) %>%
  dplyr::group_by(
    trp_id,
    length_class_2
  ) %>%
  dplyr::summarise(
    n_length_class_2 = n()
  ) %>%
  dplyr::left_join(
    n_vehicles_length_classified,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    ratio = n_length_class_2 / n_length_classified
  ) %>%
  dplyr::left_join(
    trps_used,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    trp_name_and_road =
      paste0(
        name,
        ", ",
        road_category_and_number
      )
  )


## Two length classes around 7.6 m----
n_length_2_7.6_trp <-
  enriched_data %>%
  dplyr::filter(
    valid_length == TRUE
  ) %>%
  dplyr::group_by(
    trp_id,
    length_class_2_7.6
  ) %>%
  dplyr::summarise(
    n_length_class_2_7.6 = n()
  ) %>%
  dplyr::left_join(
    n_vehicles_length_classified,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    ratio = n_length_class_2_7.6 / n_length_classified
  ) %>%
  dplyr::left_join(
    trps_used,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    trp_name_and_road =
      paste0(
        name,
        ", ",
        road_category_and_number
      )
  )

## All length classes ----
n_length_class_full <-
  enriched_data %>%
  dplyr::filter(
    valid_length == TRUE
  ) %>%
  dplyr::group_by(
    trp_id,
    length_class_full
  ) %>%
  dplyr::summarise(
    n_length_class_full = n()
  ) %>%
  dplyr::left_join(
    n_vehicles_length_classified,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    ratio = n_length_class_full / n_length_classified
  ) %>%
  dplyr::left_join(
    trps_used,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    trp_name_and_road =
      paste0(
        name,
        ", ",
        road_category_and_number
      )
  )


# 5. Write to file ----
