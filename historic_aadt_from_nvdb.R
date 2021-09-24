# Historisk ÅDT-belegging

source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")

# Tosentunnelen
tosentunnelen_aadt <- get_historic_aadt_by_roadlinkposition("0.1@885997")
writexl::write_xlsx(tosentunnelen_aadt,
                    path = "spesialbestillinger/tosentunnelen_historisk_trafikkmengde.xlsx")
