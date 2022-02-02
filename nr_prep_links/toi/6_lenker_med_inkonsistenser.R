# Sjekk lenker med inkonsistenser

library(sf)
library(tmap)
library(tidyverse)
library(stringi)

# Les inn trafikklenker -------------------------------------------------------

path_gpkg <- 'data/nr/drive-download-20210421T081315Z-001/trafikklenker_undir_maincomp_20210420.gpkg'

st_layers(path_gpkg)

trafikklenker <- st_read(path_gpkg, layer = 'trafikklenker_rettet_trafikk')

# Les inn tabell med vegreferanser --------------------------------------------

vegref <- read_delim(file = "data/region-vest/2021_04/trafikklenker_vegsystemreferanser.csv", delim = ";")

vegref %>% 
  count(KATEGORI)

# # A tibble: 4 x 2
#   KATEGORI                       n
#   <chr>                      <int>
# 1 Samme kryssdel               644
# 2 Samme veg, ulik strekning   2645
# 3 Samme vegstrekning        103757
# 4 Ulik veg- eller kryssdel       6

# Veger med START+END på samme vegstrekning eller kryssdel er enkelt å koble
# "Samme veg, ulik strekning" og "Ulik veg- eller kryssdel" krever litt undersøking

vegref %>% 
  count(med_metrering, SIGN_METRERING)
# # A tibble: 8 x 3
#   med_metrering SIGN_METRERING     n
#           <dbl>          <dbl> <int>
# 1             0             -1 51828
# 2             0              0    18
# 3             0              1    14
# 4             0             NA  1323
# 5             1             -1    15
# 6             1              0    16
# 7             1              1 52510
# 8             1             NA  1328

# De fleste lenkene er som forventet: 
# med_metrering = 1 & SIGN_METRERING = 1 : 52510
# med_metrering = 0 & SIGN_METRERING = -1 : 51828

# NA: 1323 + 1328 = 2651 = 2645 + 6 - dette er "Samme veg, ulik strekning" og "Ulik veg- eller kryssdel"

# Hva er det med disse 63 lenkene?

# # A tibble: 8 x 3
#   med_metrering SIGN_METRERING     n
#           <dbl>          <dbl> <int>
# 2             0              0    18
# 3             0              1    14
# 5             1             -1    15
# 6             1              0    16

sjekk <- vegref %>% 
  filter(!is.na(SIGN_METRERING) & 
        !((med_metrering == 1 & SIGN_METRERING == 1) | (med_metrering == 0 & SIGN_METRERING == -1)))

n_distinct(sjekk$ID)
# 34 forskjellige lenker

# Samme START & END 

id_lik_ref <- trafikklenker %>% 
  filter(ROADREF_START == ROADREF_END) %>% 
  distinct(ID) %>% 
  pull(ID)

# 18 lenker med samme start- og sluttreferanse

id_feil_retning <- sjekk %>% 
  filter(!ID %in% id_lik_ref) %>% 
  distinct(ID) %>% 
  pull(ID)

# Les inn urettede lenker -----------------------------------------------------

st_layers(path_gpkg)

edges <- st_read(path_gpkg, layer = 'edges_main')


edges_lik_ref <- edges %>% 
  filter(ID %in% id_lik_ref)

edges_sjekk_retning <- edges %>% 
  filter(ID %in% id_feil_retning)



st_write(edges_lik_ref, dsn = 'data/region-vest/2021_04/sjekk_lenker.gpkg', layer = 'lenker_lik_vegref')


st_write(edges_sjekk_retning, dsn = 'data/region-vest/2021_04/sjekk_lenker.gpkg', layer = 'lenker_retning')


edges_lik_ref <- edges_lik_ref %>% 
  mutate(kommentar = "ROADREF_START er lik ROADREF_END")

edges_sjekk_retning <- edges_sjekk_retning %>% 
  mutate(kommentar = "Stemmer retningen?")



tmp <- bind_rows(edges_lik_ref, edges_sjekk_retning)


write_delim(tmp, file = "data/region-vest/2021_04/sjekk_lenker.csv", delim = ";")

