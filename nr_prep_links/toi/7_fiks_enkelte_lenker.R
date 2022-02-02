# Legger til info om feil i vegreferanser og lagrer vegnettsgrafen til prosjektet

library(sf)
library(tmap)
library(tidyverse)
library(stringi)

tmap_mode("view")

# Les inn trafikklenker -------------------------------------------------------

path_gpkg <- 'data/nr/drive-download-20210421T081315Z-001/trafikklenker_undir_maincomp_20210420.gpkg'

st_layers(path_gpkg)

trafikklenker <- st_read(path_gpkg, layer = 'trafikklenker_rettet_trafikk')

kryss <- st_read(path_gpkg, layer = 'nodes_main')

# Les inn tabell med vegreferanser --------------------------------------------

lenker_vegref <- read_delim(file = "data/region-vest/2021_04/trafikklenker_vegsystemreferanser.csv", delim = ";")

# Lenker med problemer --------------------------------------------------------

lenker_med_feil <- read_delim(file = "data/region-vest/2021_04/sjekk_lenker.csv", delim = ";")

id <- lenker_med_feil$ID

# Sletter ikke likevel
# # 1) Sletter lenke som har ROADREF_START == ROADREF_END og er en rasteplass: "49142c06-aaed-4371-a098-f469b5d2cd21"
# 
# tm_shape(filter(trafikklenker, ID == "49142c06-aaed-4371-a098-f469b5d2cd21")) +
#   tm_lines()
# 
# trafikklenker <- filter(trafikklenker, ID != "49142c06-aaed-4371-a098-f469b5d2cd21")
# 
# lenker_vegref <- filter(lenker_vegref, ID != "49142c06-aaed-4371-a098-f469b5d2cd21")
# 
# id <- id[id != "49142c06-aaed-4371-a098-f469b5d2cd21"]
# 
# # sletter også tilhørende endekryss
# 
# kryss <- filter(kryss, FEATURE_OID != 1252316)


# Velger å ikke rette opp de 33 lenkene med feil med vegreferansene, da disse er enklere å koble manuelt
# enn å rette opp vegreferansene
# Har sjekket at noder og lenker er ok, så disse vil ikke påvirke grafen. 
# Det er kun koblingen som må sjekkes nærmere

# Legg til info om feil med vegreferanser -------------------------------------

trafikklenker <- trafikklenker %>% 
  mutate(KOMMENTAR_TOI = if_else(ID %in% id, "Noe rart med ROADREF_START/ROADREF_END", ""))

lenker_vegref <- lenker_vegref %>% 
  mutate(KOMMENTAR_TOI = if_else(ID %in% id, "Noe rart med ROADREF_START/ROADREF_END", ""))

# Lagre rettet veggraf --------------------------------------------------------

path_data <- "data/region-vest/2021_04/trafikklenker_rettet_20210502.gpkg"

st_write(trafikklenker, path_data, layer = "lenker")

st_write(kryss, path_data, layer = "kryss")

# Lagre referanser

write_delim(lenker_vegref, file = "data/region-vest/2021_04/trafikklenker_vegsystemreferanser.csv", delim = ";")
