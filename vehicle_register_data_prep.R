# Read and filter vehicles from complete dump from vehicle registry

# Packages
library(tidyverse)
library(jsonlite)
library(flextable)
library(httr)
library(writexl)

source("get_from_data_norge.R")


# Four data sets from API ----

# Avgiftskoder, antakelig irrelevant
#vehicle_groups <- get_vehicle_groups()

# Klassene brukt i Kjøretøyforskriften
vehicle_technical_codes <- get_technical_codes()
# TODO: map to NorSiKT classes

# The available parameters
vehicle_info_fields <- get_vehicle_info_fields()

# The vehicle data, light goods vehicles
#vehicle_info_N1 <- get_vehicle_info("N1")
# Spørringa tar alt for langt tid! Bruk CSV-dump i stedet.


# Complete set in CSV dump ----
# OBS! 2 GB i csv-fila!
#komplett_liste <- readr::read_csv2("kjoretoyregisteret/kjoretoy_komplett.csv",
#                           col_select = traffic_data_relevant_columns)

# Ser kun på korte
shorter_vehicles <- komplett_liste %>%
  dplyr::filter(tekn_reg_status == "REGISTRERT",
                tekn_tknavn %in% c("M1", "M2", "N1", "N2"))

shorter_vehicles %>%
  write.csv2("kjoretoyregisteret/kjoretoy_M1_M2_N1_N2.csv",
             row.names = FALSE)
shorter_vehicles <- read_csv2("kjoretoyregisteret/kjoretoy_M1_M2_N1_N2.csv")

# Må inkludere m1g og n1g da f.eks. Land Cruiser er med her.
terrain_vehicles <- komplett_liste %>%
  dplyr::filter(tekn_reg_status == "REGISTRERT",
                tekn_tknavn %in% c("M1G", "M2G", "N1G", "N2G"))

terrain_vehicles %>%
  write.csv2("kjoretoyregisteret/kjoretoy_M1G_M2G_N1G_N2G.csv",
             row.names = FALSE)
terrain_vehicles <- read_csv2("kjoretoyregisteret/kjoretoy_M1G_M2G_N1G_N2G.csv")


# Transform ----
categorize_vehicles <- function(length_limit) {

  all_small_vehicles <-
    dplyr::bind_rows(
      shorter_vehicles,
      terrain_vehicles
      ) %>%
    dplyr::select(
      -tekn_avreg_dato,
      -tekn_reg_status
      ) %>% # nothing interesting
    dplyr::filter(
      tekn_reg_aar > 2e7, # removing old vehicles
      tekn_lengde < 16e3, # removing obviously mistyped lengths
      tekn_lengde > 2500 # no real cars shorter than this, removing obviously mistyped lengths
    ) %>%
    dplyr::mutate(
      tekn_tknavn = stringr::str_sub(tekn_tknavn, 1, 2),
      tekn_lengde = tekn_lengde / 1e3,
      tekn_totvekt = tekn_totvekt / 1e3,
      lengdegrense = length_limit,
      kategori = dplyr::case_when(
        tekn_lengde < length_limit & tekn_totvekt <= 3.5 ~ "kort_og_lett",
        tekn_lengde >= length_limit & tekn_totvekt <= 3.5 ~ "lang_og_lett",
        tekn_lengde < length_limit & tekn_totvekt > 3.5 ~ "kort_og_tung",
        tekn_lengde >= length_limit & tekn_totvekt > 3.5 ~ "lang_og_tung",
        TRUE ~ ""
      ))
}

group_categorized_vehicles <- function(categorized_vehicle_df) {

  categorized_vehicle_df %>%
    dplyr::group_by(tekn_tknavn, kategori) %>%
    dplyr::summarise(antall = n())

}

vehicles_5.6 <- categorize_vehicles(5.6)
vehicles_6 <- categorize_vehicles(6)

total_number_of_vehicles <- nrow(vehicles_5.6)
total_number_of_vehicles_per_group <- vehicles_5.6 %>%
  dplyr::group_by(tekn_tknavn) %>%
  dplyr::summarise(totalantall = n())


vehicles_5.6_grouped <- vehicles_5.6 %>%
  group_categorized_vehicles()

vehicles_5.6_grouped %>%
  write.csv2("kjoretoyregisteret/vehicles_5.6_grouped.csv",
             row.names = FALSE)

vehicles_5.4_grouped <- categorize_vehicles(5.4) %>%
  group_categorized_vehicles()

vehicles_5.4_grouped %>%
  write.csv2("kjoretoyregisteret/vehicles_5.4_grouped.csv",
             row.names = FALSE)

vehicles_5.8_grouped <- categorize_vehicles(5.8) %>%
  group_categorized_vehicles()

vehicles_5.8_grouped %>%
  write.csv2("kjoretoyregisteret/vehicles_5.8_grouped.csv",
             row.names = FALSE)

vehicles_6.0_grouped <- categorize_vehicles(6) %>%
  group_categorized_vehicles()

vehicles_6.0_grouped %>%
  write.csv2("kjoretoyregisteret/vehicles_6.0_grouped.csv",
             row.names = FALSE)

# Find relative distribution for different length values ----
vehicles_5.6_grouped_relative <- vehicles_5.6_grouped %>%
  dplyr::left_join(total_number_of_vehicles_per_group,
                   by = "tekn_tknavn") %>%
  dplyr::mutate(prosentandel = round(100 * (antall / totalantall), digits = 1))

calculate_relative_distribution_of_grouped_categorized_vehicles <-
  function(categorized_vehicle_df) {

  categorized_vehicle_df %>%
    dplyr::group_by(kategori, lengdegrense) %>%
    dplyr::summarise(antall = n()) %>%
    dplyr::mutate(
      prosentandel = round(100 * (antall / total_number_of_vehicles),
                           digits = 1),
      lengdegrense = as.character(lengdegrense)
      )

  }

length_values <- seq(5.4, 8.9, 0.1)

vehicles_relative <- dplyr::bind_rows(
  categorize_vehicles(5.4) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(5.5) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(5.6) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(5.7) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(5.8) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(5.9) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.0) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.1) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.2) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.3) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.4) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.5) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.6) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.7) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.8) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(6.9) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.0) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.1) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.2) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.3) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.4) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.5) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.6) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.7) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.8) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(7.9) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(8.0) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(8.1) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(8.2) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(8.3) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(8.4) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(8.5) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles,
  categorize_vehicles(8.6) %>%
    calculate_relative_distribution_of_grouped_categorized_vehicles
)


vehicles_relative %>%
  write.csv2("kjoretoyregisteret/vehicles_relative.csv",
             row.names = FALSE)

# Looking at specific vehicles ----
short_lengths <- vehicles_5.6 %>%
  dplyr::filter(tekn_lengde < 3)

# Campingbiler
extreme_weights <- vehicles_5.6 %>%
  dplyr::filter(tekn_totvekt > 3.5,
                tekn_tknavn == "M1")

too_heavy <- vehicles_5.6 %>%
  dplyr::filter(kategori == "kort_og_tung")

too_heavy_models <- too_heavy %>%
  dplyr::mutate(
    merke_og_modell = paste0(tekn_merkenavn, " ", tekn_modell)) %>%
  dplyr::group_by(merke_og_modell) %>%
  dplyr::summarise(antall = n()) %>%
  dplyr::arrange(desc(antall))

too_heavy <- vehicles_6 %>%
  dplyr::filter(kategori == "kort_og_tung")

too_heavy_models <- too_heavy %>%
  dplyr::mutate(
    merke_og_modell = paste0(tekn_merkenavn, " ", tekn_modell)) %>%
  dplyr::group_by(merke_og_modell) %>%
  dplyr::summarise(antall = n()) %>%
  dplyr::arrange(desc(antall))


too_long <- vehicles_5.6 %>%
  dplyr::filter(kategori == "lang_og_lett")

too_long_models <- too_long %>%
  dplyr::mutate(
    merke_og_modell = paste0(tekn_merkenavn, " ", tekn_modell)) %>%
  dplyr::group_by(merke_og_modell) %>%
  dplyr::summarise(antall = n()) %>%
  dplyr::arrange(desc(antall))


mitsubishi_outlander <- vehicles_5.6 %>%
  dplyr::filter(tekn_merkenavn == "MITSUBISHI",
                tekn_modell == "OUTLANDER")

# TODO: finne optimalt lengdeskille
# Målet er å ha færrest andel i mellomkategoriene
# Simulere med lengder mellom 5,4 - 6,2
# TODO: motorsykler
# TODO: hvilke kjøretøy havner i mellomkategoriene?
# TODO: hengere...?



