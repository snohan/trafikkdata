# --------------------------------------------------------------------------- #
#                                                                             #
#    Legger til lag der lenkeretning er lik trafikkretningen                  #
#                                                                             #
#                                                                             #
#    Lag:  'trafikklenker_rettet_trafikk'                                     #
#                                                                             #
# --------------------------------------------------------------------------- #

library(tidyverse)
library(sf)
library(tmap)

# les inn trafikklenkene ------------------------------------------------------

path_gpkg <- 'data/nr/drive-download-20210421T081315Z-001/trafikklenker_undir_maincomp_20210420.gpkg'

trafikklenker_rettet_metrering <- st_read(path_gpkg, layer = 'trafikklenker_rettet_metrering')

# START_NODE_OID --> END_NODE_OID følger metreringsretningen ved datauttaksdato

# variabelen DIRECTION angir trafikkretning relativt til metreringsretningen
# 1 = MED - lenken har enveistrafikk og trafikkretningen er MED metreringsretningen
# 2 = MOT - lenken har enveistrafikk og trafikkretningen er MOT metreringsretningen
# 3 = BEGGE - lenken har toveistrafikk

# snu enveislenker med DIRECTION = 2 ------------------------------------------

# enveislenker med kjøreretning mot lenkeretning (= metreringsretning)
enveis <- filter(trafikklenker_rettet_metrering, DIRECTION == 2)

# resten av lenkene
trafikklenker <- filter(trafikklenker_rettet_metrering, DIRECTION != 2)

# snu retning
enveis <- enveis %>%
  mutate(
    # snu start- og sluttnode
    START_NODE_OID = enveis$END_NODE_OID,
    END_NODE_OID = enveis$START_NODE_OID,
    # angi at lenkeretning er mot metreringsretning ved å sette dummy = 0
    med_metrering = 0,
    # snu vegreferansen
    ROADREF_START = enveis$ROADREF_END, 
    ROADREF_END  = enveis$ROADREF_START,
    # snu from og to
    from = enveis$to,
    to = enveis$from
  )

# slå sammen lenker
trafikklenker_rettet_trafikk <- bind_rows(trafikklenker, enveis)

# legg til løpenummer ---------------------------------------------------------

# sorter på ID
trafikklenker_rettet_trafikk <- trafikklenker_rettet_trafikk %>% 
  arrange(ID)

# legg til et løpenummer for hver rettede lenke
trafikklenker_rettet_trafikk <- rownames_to_column(trafikklenker_rettet_trafikk, var = "lenke_nr")


# lagrer til datasettet -------------------------------------------------------

st_write(trafikklenker_rettet_trafikk, dsn = path_gpkg, layer = 'trafikklenker_rettet_trafikk')
