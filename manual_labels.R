# Get all manual labels from trp-api and write to csv

library(writexl)

source("H:/Programmering/R/byindeks/get_from_trp_api.R")

manual_labels <- get_manual_labels_by_county("50", "F")

writexl::write_xlsx(manual_labels, path = "manuelle_merkinger.xlsx")
