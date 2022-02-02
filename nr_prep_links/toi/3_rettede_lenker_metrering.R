# --------------------------------------------------------------------------- #
#                                                                             #
#    Legger til lag der trafikklenker med toveistrafikk ligger dobbelt        #
#                                                                             #
#    Ny variabel med_metrering angir                                          #
#                                                                             #
#              1 = lenkeretning MED metreringsretning                         #
#              0 = lenkeretning MOT metreringsretning                         #
#                                                                             #
#    Lag: 'trafikklenker_rettet_metrering'                                    #
#                                                                             #
# --------------------------------------------------------------------------- #

library(tidyverse)
library(sf)
library(tmap)

# les inn trafikklenkene ------------------------------------------------------

path_gpkg <- '/nr_prep_links/trafikklenker_undir_maincomp.gpkg'

trafikklenker <- st_read(path_gpkg, layer = 'edges_main')

# START_NODE_OID --> END_NODE_OID følger metreringsretningen ved datauttaksdato

# variabelen DIRECTION angir trafikkretning relativt til metreringsretningen
# 1 = MED - lenken har enveistrafikk og trafikkretningen er MED metreringsretningen
# 2 = MOT - lenken har enveistrafikk og trafikkretningen er MOT metreringsretningen
# 3 = BEGGE - lenken har toveistrafikk

trafikklenker %>%
  st_set_geometry(NULL) %>%
  count(DIRECTION)
#   DIRECTION     n
# 1         1   828
# 2         2   142
# 3         3 53041

# legg til metreringsretning dummy --------------------------------------------

trafikklenker <- mutate(trafikklenker, med_metrering = 1)

# dupliserer leker med toveistrafikk ------------------------------------------

trafikklenker_enveis <- filter(trafikklenker, DIRECTION < 3)

trafikklenker_toveis <- filter(trafikklenker, DIRECTION == 3)

trafikklenker_toveis_mot <- trafikklenker_toveis # lag en kopi

identical(trafikklenker_toveis_mot, trafikklenker_toveis)
# TRUE

# snu retning
trafikklenker_toveis_mot <- trafikklenker_toveis_mot %>%
  mutate(
    # snu start- og sluttnode
    START_NODE_OID = trafikklenker_toveis$END_NODE_OID,
    END_NODE_OID = trafikklenker_toveis$START_NODE_OID,
    # angi at lenkeretning er mot metreringsretning ved å sette dummy = 0
    med_metrering = 0,
    # snu vegreferansen
    ROADREF_START = trafikklenker_toveis$ROADREF_END,
    ROADREF_END  = trafikklenker_toveis$ROADREF_START,
    # snu from og to
    from = trafikklenker_toveis$to,
    to = trafikklenker_toveis$from
  )

identical(trafikklenker_toveis_mot, trafikklenker_toveis)
# FALSE

trafikklenker <- bind_rows(trafikklenker_enveis, trafikklenker_toveis, trafikklenker_toveis_mot)

rm(trafikklenker_enveis, trafikklenker_toveis, trafikklenker_toveis_mot)

table(trafikklenker$med_metrering)

# sorter etter ID
trafikklenker <- trafikklenker %>%
  arrange(ID)

# nå ligger trafikklenkene med toveistrafikk dobbelt
trafikklenker %>%
  filter(DIRECTION == 3) %>%
  head() %>%
  View()

# lagrer til datasettet -------------------------------------------------------

st_write(trafikklenker, dsn = path_gpkg, layer = 'trafikklenker_rettet_metrering')
