# Read and filter vehicles from complete dump of vehicle registry

# Packages
{
  library(tidyverse)
  library(jsonlite)
  library(flextable)
  library(httr)
  library(writexl)
}


# Complete set in CSV dump ----
# Huge file!
vehicles <-
  readr::read_csv(
    #"kjoretoyregisteret/kjoretoy_komplett.csv",
    "H:/Trafikkdata/Autosys/7877_kjoretoydata_5/7877_kjoretoydata_5.csv"
    #col_select = traffic_data_relevant_columns,
    #n_max = 10
  ) |>
  dplyr::filter(
    stringr::str_detect(TEKN_TKNAVN, "^T", negate = TRUE),
    stringr::str_detect(TEKN_TKNAVN, "^C", negate = TRUE),
    stringr::str_detect(TEKN_TKNAVN, "^R", negate = TRUE),
    stringr::str_detect(TEKN_TKNAVN, "^S", negate = TRUE),
    stringr::str_detect(TEKN_TKNAVN, "^B", negate = TRUE),
    stringr::str_detect(TEKN_TKNAVN, "L2e", negate = TRUE),
    stringr::str_detect(TEKN_TKNAVN, "L4e", negate = TRUE),
    stringr::str_detect(TEKN_TKNAVN, "L5e", negate = TRUE),
    TEKN_REG_F_G > 20050000 # Removing old vehicles
  ) |>
  dplyr::mutate(
    length_m = TEKN_LENGDE / 1e3,
    date_registered = lubridate::ymd(TEKN_REG_F_G),
    # date_registered_norway = lubridate::ymd(TEKN_REG_F_G_N),
    date_last_pkk = lubridate::ymd(TEKN_SISTE_PKK),
    axle_dist_m_1 = TEKN_MINAVST_MS1 / 1e3,
    axle_dist_m_2 = TEKN_MINAVST_MS2 / 1e3
  ) |> 
  dplyr::select(
    date_registered,
    # date_registered_norway,
    class_code = TEKN_TKNAVN,
    status = TEKN_REG_STATUS,
    brand = TEKN_MERKENAVN,
    model = TEKN_MODELL,
    length_m,
    weight_allowed = TEKN_TOTVEKT,
    coupling_weight_allowed = TEKN_BEL_H_FESTE,
    axles_n = TEKN_AKSLER,
    axle_dist_m_1,
    axle_dist_m_2,
    date_last_pkk,
    km = KILOMETERSTAND
  ) 

# diff_dates <-
#   vehicles |> 
#   dplyr::select(
#     date_registered,
#     date_registered_norway,
#   ) |> 
#   dplyr::mutate(
#     diff_date = date_registered_norway - date_registered
#   ) |> 
#   dplyr::filter(
#     diff_date > 0
#   )

vehicles$class_code |> unique() |> sort()
vehicles$status |> unique() |> sort()

# top_brands <-
#   vehicles |>
#   dplyr::filter(
#     stringr::str_detect(TEKN_TKNAVN, "^M3[:alnum:]*")
#   ) |>
#   dplyr::summarise(
#     n_vehicles = n(),
#     .by = TEKN_MERKENAVN
#   ) |>
#   dplyr::arrange(dplyr::desc(n_vehicles))

classes <- 
  vehicles |> 
  dplyr::summarise(
    n_vehicles = n(),
    .by = class_code
  ) |> 
  dplyr::arrange(class_code)


# L1e: moped
# L2e: moped med tre hjul
# L3e: motorsykkel
# L4e: motorsykkel med sidevogn
# L5e: motorsykkel med tre hjul
# L6e: moped med fire hjul
# L7e: motorsykkel med fire hjul

class_examples <- 
  vehicles |> 
  dplyr::filter(class_code == "L5e") |> 
  dplyr::select(
    class_code,
    brand,
    model,
    length_m,
    axles_n,
    date_last_pkk,
    km
  )


# HERE!!!






classified_vehicles <- registered_vehicles %>%
  dplyr::mutate(
    class = dplyr::case_when(
      tekn_tknavn %in% c("L1e", "L2e", "L3e", "L4e", "L5e", "L6e", "L7e",
                         "MCL", "MCM", "MCT") ~ "L",
      tekn_tknavn %in% c("M1", "M1G") ~ "M1",
      tekn_tknavn %in% c("M2", "M2G") ~ "M2",
      tekn_tknavn %in% c("M3", "M3G") ~ "M3",
      tekn_tknavn %in% c("N1", "N1G") ~ "N1",
      tekn_tknavn %in% c("N2", "N2G") ~ "N2",
      tekn_tknavn %in% c("N3", "N3G") ~ "N3",
      tekn_tknavn == "O1" ~ "O1",
      tekn_tknavn == "O2" ~ "O2",
      tekn_tknavn == "O3" ~ "O3",
      tekn_tknavn == "O4" ~ "O4",
      TRUE ~ "no_class"
    ),
   lengde_m = tekn_lengde / 1e3,
   vekt_t = tekn_totvekt / 1e3
  ) %>%
  dplyr::filter(
    class != "no_class",
    !is.na(lengde_m)
  ) %>%
  dplyr::mutate(
    valid_length = dplyr::case_when(
      class == "L" & lengde_m < 1 ~ FALSE,
      class == "L" & lengde_m > 4 ~ FALSE,
      class == "M1" & lengde_m < 2.5 ~ FALSE,
      class == "M1" & lengde_m > 12 ~ FALSE,
      # M2 og M3 har alle troverdige lengder
      class == "N1" & lengde_m < 4 ~ FALSE,
      class == "N1" & lengde_m > 10 ~ FALSE,
      class == "N2" & lengde_m < 3 ~ FALSE,
      class == "N2" & lengde_m > 12 ~ FALSE,
      class == "N3" & lengde_m < 3 ~ FALSE,
      class == "N3" & lengde_m > 20 ~ FALSE,
      class == "O1" & lengde_m < 1 ~ FALSE,
      class == "O1" & lengde_m > 10 ~ FALSE,
      class == "O2" & lengde_m < 2 ~ FALSE,
      class == "O2" & lengde_m > 15 ~ FALSE,
      class == "O3" & lengde_m < 4 ~ FALSE,
      class == "O3" & lengde_m > 15 ~ FALSE,
      class == "O4" & lengde_m < 2 ~ FALSE,
      class == "O4" & lengde_m > 25 ~ FALSE,
      TRUE ~ TRUE
    ),
    valid_weight = dplyr::case_when(
      class == "L" & vekt_t > 2 ~ FALSE,
      TRUE ~ TRUE
    )
  ) %>%
  dplyr::select(tekn_merkenavn, tekn_modell, class, lengde_m, valid_length,
                vekt_t, valid_weight, tekn_vogntogvekt)

classified_vehicles %>%
  write.csv2("kjoretoyregisteret/kjoretoy_filtrert.csv",
             row.names = FALSE)
#classified_vehicles <- read_csv2("kjoretoyregisteret/kjoretoy_filtrert.csv")

# length_check <- classified_vehicles %>%
#   dplyr::filter(class == "M3",
#                 lengde_m < 10
#                 ) %>%
#   dplyr::arrange(lengde_m)

# weight_check <- classified_vehicles %>%
#   dplyr::filter(class == "N2",
#                 vekt_t < 3.5
#                 ) %>%
#   dplyr::arrange(vekt_t)


# Shorter vehicles
shorter_vehicles <- classified_vehicles %>%
  dplyr::filter(class %in% c("M1", "M2", "N1", "N2"))

shorter_vehicles %>%
  write.csv2("kjoretoyregisteret/kjoretoy_M1_M2_N1_N2.csv",
             row.names = FALSE)
#shorter_vehicles <- read_csv2("kjoretoyregisteret/kjoretoy_M1_M2_N1_N2.csv")

categorize_vehicles <- function(length_limit) {

  shorter_vehicles_categorized <- shorter_vehicles %>%
    dplyr::mutate(
      lengdegrense = length_limit,
      kategori = dplyr::case_when(
        lengde_m < length_limit & vekt_t <= 3.5 ~ "kort_og_lett",
        lengde_m >= length_limit & vekt_t <= 3.5 ~ "lang_og_lett",
        lengde_m < length_limit & vekt_t > 3.5 ~ "kort_og_tung",
        lengde_m >= length_limit & vekt_t > 3.5 ~ "lang_og_tung",
        TRUE ~ ""
      ))
}

group_categorized_vehicles <- function(categorized_vehicle_df) {

  categorized_vehicle_df %>%
    dplyr::group_by(class, kategori) %>%
    dplyr::summarise(antall = n())

}

vehicles_5.6_grouped <- categorize_vehicles(5.6) %>%
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
total_number_of_vehicles <- nrow(shorter_vehicles)

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

#length_values <- seq(5.4, 8.9, 0.1)

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
vehicles_5.6 <- categorize_vehicles(5.6)

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



# TODO: vehicles with trailers



