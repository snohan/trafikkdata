# Read and filter vehicles from complete dump from vehicle registry

# Packages
library(tidyverse)
library(jsonlite)
library(flextable)
library(httr)
library(writexl)

source("get_from_data_norge.R")


# Must have four data sets ----

# Avgiftskoder, antakelig irrelevant
#vehicle_groups <- get_vehicle_groups()

# Klassene brukt i Kjøreøtyforskriften
vehicle_technical_codes <- get_technical_codes()
# TODO: map to NorSiKT classes

# The available parameters
vehicle_info_fields <- get_vehicle_info_fields()

# The vehicle data, light goods vehicles
#vehicle_info_N1 <- get_vehicle_info("N1")
# Spørringa tar alt for langt tid! Bruk CSV-dump i stedet.

# OBS! 2 GB i csv-fila!
#test_1 <- readr::read_csv2("kjoretoyregisteret/kjoretoy_komplett.csv",
#                           col_select = traffic_data_relevant_columns)

# Ser kun på korte
shorter_vehicles <- test_1 %>%
  dplyr::filter(tekn_reg_status == "REGISTRERT",
                tekn_tknavn %in% c("M1", "M2", "N1", "N2"))

# Må inkludere m1g og n1g da f.eks. Land Cruiser er med her.
terrain_vehicles <- test_1 %>%
  dplyr::filter(tekn_reg_status == "REGISTRERT",
                tekn_tknavn %in% c("M1G", "M2G", "N1G", "N2G"))


# TODO: fjerne veteranbiler og andre gamle som sikkert ikke kjører mye nå, dvs. regaar før 2000.
# TODO: fjern eventuelle uten lengde (hvor mange er det?)
# TODO: sjekk om noen lengder er upålitelige
# (har sett en lastebil fra 1978 på 26,5 m...tilfeldigivs samme tall som tillatt totalvekt!)

shorter_vehicles %>%
  write.csv2("kjoretoyregisteret/kjoretoy_M1_M2_N1_N2.csv",
             row.names = FALSE)
shorter_vehicles <- read_csv2("kjoretoyregisteret/kjoretoy_M1_M2_N1_N2.csv")


terrain_vehicles %>%
  write.csv2("kjoretoyregisteret/kjoretoy_M1G_M2G_N1G_N2G.csv",
             row.names = FALSE)
terrain_vehicles <- read_csv2("kjoretoyregisteret/kjoretoy_M1G_M2G_N1G_N2G.csv")

all_small_vehicles <- dplyr::bind_rows(shorter_vehicles, terrain_vehicles) %>%
  dplyr::select(-tekn_avreg_dato, -tekn_reg_status) %>% # nothing interesting
  dplyr::filter(tekn_reg_aar > 2e7, # removing old vehicles
                tekn_lengde < 16e3, # removing obviously mistyped lengths
                tekn_lengde > 2500 # no real cars shorter than this, removing obviously mistyped lengths
                ) %>%
  dplyr::mutate(tekn_tknavn = stringr::str_sub(tekn_tknavn, 1, 2),
                tekn_lengde = tekn_lengde / 1e3,
                tekn_totvekt = tekn_totvekt / 1e3,
                kategori = dplyr::case_when(
                  tekn_lengde < 5.6 & tekn_totvekt < 3.5 ~ "kort_og_lett",
                  tekn_lengde >= 5.6 & tekn_totvekt < 3.5 ~ "lang_og_lett",
                  tekn_lengde < 5.6 & tekn_totvekt >= 3.5 ~ "kort_og_tung",
                  tekn_lengde >= 5.6 & tekn_totvekt >= 3.5 ~ "lang_og_tung",
                  TRUE ~ ""
                ))

small_vehicles_grouped <- all_small_vehicles %>%
  dplyr::group_by(tekn_tknavn, kategori) %>%
  dplyr::summarise(antall = n())

table(all_small_vehicles$tekn_tknavn)

extreme_lengths <- all_small_vehicles %>%
  dplyr::filter(tekn_lengde > 16000)

short_lengths <- all_small_vehicles %>%
  dplyr::filter(tekn_lengde < 3000)

extreme_weights <- all_small_vehicles %>%
  dplyr::filter(tekn_totvekt > 7000,
                tekn_tknavn == "M1")

mitsubishi_outlander <- all_small_vehicles %>%
  dplyr::filter(tekn_merkenavn == "MITSUBISHI",
                tekn_modell == "OUTLANDER")

# TODO: finne optimalt lengdeskille
# Målet er å ha færrest andel i mellomkategoriene
# Simulere med lengder mellom 5,4 - 6,2