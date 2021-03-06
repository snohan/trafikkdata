# Get AADTs for specific trps
library(writexl)
library(DataExplorer)

points <- get_trp_for_adt()

geodata_1 <- c(54, 18, 50, 15)
geodata_2 <- c(11, 38, 42, 46)
geodata_3 <- c(3, 30, 34)

points_chosen <- points %>%
  dplyr::filter(county_number %in% geodata_3)

aadt_chosen_raw <- get_aadt_by_length_for_trp_list(points_chosen$trp_id)

aadt_chosen <- aadt_chosen_raw %>%
  dplyr::select(-sd_length_range) %>%
  dplyr::filter(length_range %in% c("[..,5.6)", "[5.6,..)")) %>%
  dplyr::mutate(length_range =
                  case_when(length_range == "[..,5.6)" ~ "lette",
                            length_range == "[5.6,..)" ~ "tunge"),
                klasse1 = length_range,
                klasse2 = length_range,
                godkjent_lengde = round(aadt_valid_length / aadt_total * 100,
                                        digits = 1),
                coverage = round(coverage, digits = 1)) %>%
  tidyr::pivot_wider(names_from = c(klasse1, klasse2, length_range),
                     values_from = c(aadt_length_range,
                                     aadt_ci_lowerbound_length_range,
                                     aadt_ci_upperbound_length_range)) %>%
  dplyr::rename(dekningsgrad = coverage,
                aadt_ki_start_total = aadt_ci_lowerbound_total,
                aadt_ki_slutt_total = aadt_ci_upperbound_total,
                aadt_lette = aadt_length_range_lette_lette_lette,
                aadt_ki_start_lette = aadt_ci_lowerbound_length_range_lette_lette_lette,
                aadt_ki_slutt_lette = aadt_ci_upperbound_length_range_lette_lette_lette,
                aadt_tunge = aadt_length_range_tunge_tunge_tunge,
                aadt_ki_start_tunge = aadt_ci_lowerbound_length_range_tunge_tunge_tunge,
                aadt_ki_slutt_tunge = aadt_ci_upperbound_length_range_tunge_tunge_tunge) %>%
  dplyr::select(-aadt_valid_length, -total.volume.confidenceInterval) %>%
  dplyr::select(trp_id:aadt_lette, aadt_ki_start_lette, aadt_ki_slutt_lette,
                aadt_tunge, aadt_ki_start_tunge, aadt_ki_slutt_tunge)

points_chosen_aadt <- points_chosen %>%
  dplyr::left_join(aadt_chosen)

geodata_3_aadt <- points_chosen_aadt


geodata_x_aadt <- bind_rows(geodata_1_aadt,
                            geodata_2_aadt,
                            geodata_3_aadt)

# Sjekker
DataExplorer::introduce(geodata_x_aadt)
DataExplorer::plot_missing(geodata_x_aadt)
DataExplorer::plot_bar(geodata_x_aadt)
DataExplorer::plot_histogram(geodata_x_aadt$aadt_ki_start_total)
summary(geodata_x_aadt$aadt_ki_start_total)

alle_adt <- geodata_x_aadt %>%
  dplyr::mutate(first_commission =
                  as.character(first_commission_datainn)) %>%
  dplyr::select(Fylkenr = county_number,
                Fylkenavn = county_name,
                Punktid = trp_id,
                #Nortrafnr = legacyNortrafMpn,
                Punktnavn = name,
                Vegkat = road_category,
                Vegsystemreferanse = road_system_reference,
                #Vegreferanse = road_reference,
                Trafikkdatasystemet_fra = first_commission,
                Ar = year,
                Dekningsgrad = dekningsgrad,
                Lengdekvalitetsgrad = godkjent_lengde,
                ADT_total = aadt_total,
                #ADT_total_ki_start = aadt_ki_start_total,
                #ADT_total_ki_slutt = aadt_ki_slutt_total,
                ADT_lette = aadt_lette,
                #ADT_lette_ki_start = aadt_ki_start_lette,
                #ADT_lette_ki_slutt = aadt_ki_slutt_lette,
                ADT_tunge = aadt_tunge#,
                #ADT_tunge_ki_start = aadt_ki_start_tunge,
                #ADT_tunge_ki_slutt = aadt_ki_slutt_tunge
                )

write_xlsx(alle_adt, path = "aadt_trafikkdataportalen_2021-01-13.xlsx")










