# --------------------------------------------------------------------------- #
#                                                                             #
#    Filtrerer utviklingsområdet til Region Vest (Vestland+Rogaland)          #
#                                                                             #
#    Trafikklenker er generert for hele Norge av Triona for SVV.              #
#    Prosjkektet skal estimere ÅDT for Region Vest                            #
#    I dette scriptet filtreres datasettene på geografi                       #
#                                                                             #
#    Nytt datasett lagres i en ny gpkg-fil:                                   #
#                                                                             #
#    'trafikklenker_region_vest_2021_03_17.gpkg'                              #
#                                                                             #
# --------------------------------------------------------------------------- #

library(tidyverse)
library(sf)
library(tmap)

# lagre hit:
path_rv_gpkg <- 'data/region-vest/2021_03/trafikklenker_region_vest_2021_03_17.gpkg'

# les inn trafikklenkene ------------------------------------------------------

path_gpkg <- 'data/triona/Trafikklenker_20210317104831.gpkg'

st_layers(path_gpkg)
# Available layers:
#                  layer_name  geometry_type features fields
# 1                      test 3D Line String        0      0
# 2                     Kryss       3D Point   193827      2
# 3             TrafikkLenker 3D Line String   227318     28
# 4 Trafikklenker_stedfesting             NA   752267      5

trafikklenker <- st_read(path_gpkg, layer = 'TrafikkLenker')

kryss <- st_read(path_gpkg, layer = 'Kryss')

trafikklenker_stedfesting <- st_read(path_gpkg, layer = 'Trafikklenker_stedfesting')

# les inn fylkesdata ----------------------------------------------------------

# Data om Vestland og Rogaland fylker er hentet herfra:
# https://kartkatalog.geonorge.no/metadata/administrative-enheter-kommuner/041f1e6e-bdbc-4091-b48f-8a5990f3cc5b

path_vestland <- 'data/fylker/Basisdata_46_Vestland_25833_Kommuner_GEOJSON.geojson'

path_rogaland <-'data/fylker/Basisdata_11_Rogaland_25833_Kommuner_GEOJSON.geojson'

st_layers(path_vestland)

vestland <- st_read(path_vestland, layer = 'administrative_enheter.kommune')

rogaland <- st_read(path_rogaland, layer = 'administrative_enheter.kommune')

region_vest <- bind_rows(vestland, rogaland)

region_vest <- region_vest %>% 
  st_union()

# behold trafikklenker i region-vest ------------------------------------------

# lenker som overlapper med region-vest
ind <- st_intersects(trafikklenker, region_vest)

ind_rv <- lengths(ind) == 1

trafikklenker <- trafikklenker[ind_rv, ]

nrow(trafikklenker)
#51764

# # fjerner vegklasse 7-9 -------------------------------------------------------
# 
# # max og min vegklasse er ikke lik for alle lenker
# all(trafikklenker$MIN_VEGKL_9338 == trafikklenker$MAX_VEGKL_9338, na.rm = T)
# 
# # vi velger å bruke MAX_VEGKL_9338 som filtreringskriterium og fjerner dermed
# # 7 (private veger): 1275
# # 8 (skogsbilveger): 6
# # 9 (ingen i første omgang): 46
# 
# # fjerner vegklasse 7, 8 og 9, men beholder NA(=2 lenker)
# trafikklenker <- trafikklenker %>%
#   filter(MAX_VEGKL_9338 <= 6 | is.na(MAX_VEGKL_9338))
# 
# nrow(trafikklenker)
# # 50437
# n_distinct(trafikklenker$ID) == nrow(trafikklenker)
# # TRUE

# filtrerer kryss og stedfestingslenker ---------------------------------------

# filtrerer stedfestingslenker
trafikklenker_stedfesting <- trafikklenker_stedfesting %>% 
  filter(FEATURE_OID %in% trafikklenker$ID)

all(trafikklenker_stedfesting$FEATURE_OID %in% trafikklenker$ID)
# TRUE

ind_kryss <- st_covered_by(kryss, region_vest)

ind_kryss_1 <- which(lengths(ind_kryss) == 1)

noder <- unique(c(trafikklenker$START_NODE_OID,
                  trafikklenker$END_NODE_OID))

ind_kryss_2 <- which(kryss$FEATURE_OID %in% noder)

length(ind_kryss_1) # 49479
length(ind_kryss_2) # 49502

setdiff(ind_kryss_2, ind_kryss_1)

# De to metodene gir samme kryss, bortsett fra 23 langs grensen mot nord og øst
# Kryss
tmap_mode("view")
tm_shape(kryss[setdiff(ind_kryss_2, ind_kryss_1), ]) +
  tm_dots()

# Tar med alle
kryss <- kryss[ind_kryss_2, ]

# dropper z-dimensjonen for å gjøre filen mindre
trafikklenker <- st_zm(trafikklenker)
kryss <- st_zm(kryss)

# lagrer datasett -----------------------------------------------------------------------

st_write(trafikklenker, dsn = path_rv_gpkg, layer = 'TrafikkLenker')

st_write(trafikklenker_stedfesting, dsn = path_rv_gpkg, layer = 'Trafikklenker_stedfesting')

st_write(kryss, dsn = path_rv_gpkg, layer = 'Kryss')




