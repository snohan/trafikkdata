#
# Tilrettelegger trafikklenker for kobling basert på vegsystemreferanser
#

library(sf)
library(tmap)
library(tidyverse)
library(stringi)

path_gpkg <- 'data/nr/drive-download-20210421T081315Z-001/trafikklenker_undir_maincomp_20210420.gpkg'

st_layers(path_gpkg)

trafikklenker <- st_read(path_gpkg, layer = 'trafikklenker_rettet_trafikk')

edges_main <- st_read(path_gpkg, layer = 'edges_main')

# Undersøk om vegsystemreferanser er unike ------------------------------------

# bruker de urettede lenkene til å undersøke kombinasjonen av kommunenummer og vegsystemreferanse

# nye referanser: kommunenummer + vegsystemreferanse
edges_main <- edges_main %>% 
  mutate(ref_start = paste(MUNICIPALITY, ROADREF_START), 
         ref_end = paste(MUNICIPALITY, ROADREF_END)) 

# Er referansene unike?
edges_main %>% 
  st_drop_geometry() %>% 
  group_by(ROADREF_CATEGORY) %>% 
  summarise(n_start = n_distinct(ROADREF_START), 
            n_start_new = n_distinct(ref_start),
            n_end = n_distinct(ROADREF_END), 
            n_end_new = n_distinct(ref_end)) %>% 
  ungroup() %>% 
  mutate(diff_start = n_start_new - n_start,
         diff_end = n_end_new - n_end)

#   ROADREF_CATEGORY n_start n_start_new n_end n_end_new diff_start diff_end
#   <chr>              <int>       <int> <int>     <int>      <int>    <int>
# 1 E                   1310        1310  1310      1310          0        0
# 2 F                   8889        8889  8889      8889          0        0
# 3 K                  31005       43035 43044     43051      12030        7
# 4 R                    757         757   759       759          0        0

# ERF-veger har unike referanser, men for de kommunale vegene er ikke vegreferansene 
# unike uten å også benytte kommunenummer

# Er det også flere veier innen samme kommune med samme vegnummer?

ikke_unike_start <- edges_main %>% 
  st_drop_geometry() %>% 
  filter(ROADREF_CATEGORY == "K") %>% 
  count(ref_start) %>% 
  filter(n > 1) %>% 
  pull(ref_start)

ikke_unike_end <- edges_main %>% 
  st_drop_geometry() %>% 
  filter(ROADREF_CATEGORY == "K") %>% 
  count(ref_end) %>% 
  filter(n > 1) %>% 
  pull(ref_end)

edges_main %>% 
  st_drop_geometry() %>% 
  count(MUNICIPALITY, ROADREF_END) %>% 
  filter(n>1)

# Det er kun 16 startreferanser som ikke er unike også med kommunenummer, 
# lagre ID for disse

id_ikke_unike_kommunale_veier <- edges_main %>% 
  filter(ref_start %in% ikke_unike_start) %>% 
  pull(ID)

tmp <- edges_main %>% filter(ID %in% id_ikke_unike_kommunale_veier)

tm_shape(filter(tmp, ref_start == "1108 K4003 S1D1 m0")) +
  tm_lines(col = "red")

# Det finnes veger i samme kommune som har samme referanse med er ulike steder, 
# ikke så mye å gjøre med dette

# Undersøk sammenhengen mellom VEGKLASSE OG ROADREF_CATEGORY

table(edges_main$MAX_VEGKL_9338)

edges_main %>% 
  st_drop_geometry() %>% 
  filter(MIN_VEGKL_9338 > 6 & ROADREF_CATEGORY != "K") 

# Det finnes veier der MIN og MAX vegklasse ikke stemmer med referansene


# Klargjør vegsystemreferanser for matching -----------------------------------

## Fra ----

fra <- trafikklenker$ROADREF_START

fra <- str_split(fra, " ")

table(lengths(fra))
#      3      5 
# 106402    650 
# vegreferanser med 5 elementer er del av et kryss-system

# liste --> df
fra <- plyr::ldply(fra, rbind)

### Vegkategori & nummer ----

# EV6 - vegkategori = E, vegfase = V, vegnummer = 6

VK_VN <- fra$`1`

VK <- str_sub(VK_VN, 1, 1)

VN <- as.numeric(str_extract(VK_VN, "[[:digit:]]+"))

### Strekning & Delstrekning ----

S_D <- fra$`2`

S <- str_split_fixed(S_D, "D", n = 2)

D <- as.numeric(S[, 2])

S <- as.numeric(str_extract(S[, 1], "[[:digit:]]+"))

### Meterverdi ----

# desimal punktum
M <- str_replace(fra$`3`, ",", ".")

M <- round(as.numeric(str_extract(M, "[[:digit:]]+\\.*[[:digit:]]*")))

### Kryss ----

KD <- fra$`4`

# desimal punktum
KD_M <- str_replace(fra$`5`, ",", ".")

KD_M <- round(as.numeric(str_extract(KD_M, "[[:digit:]]+\\.*[[:digit:]]*")))

### Fra koblingsnøkkel ----

df_fra <- tibble(
  KOMMUNE = trafikklenker$MUNICIPALITY,
  REF = trafikklenker$ROADREF_START, 
  VK,
  VN,
  S, 
  D,
  M,
  KD,
  KD_M,
  VK_VN,
  S_D)

df_fra <- df_fra %>% 
  unite("VK_VN_SD", c(VK_VN, S_D), sep = " ", na.rm = TRUE, remove = FALSE) %>% 
  mutate(KOMM_REF = if_else(VK == "K", paste(KOMMUNE, VK_VN), VK_VN), 
         KOMM_VEGSYSREF = if_else(VK == "K", paste(KOMMUNE, VK_VN, S_D), paste(VK_VN, S_D)))

# legg til fra i kolonnenavnene
df_fra <- df_fra %>% rename_all(function(x) paste0("FRA_", x))

# rydd opp
rm(fra, VK, VN, VK_VN, S, D, S_D, M, KD, KD_M)

## Til ----

til <- trafikklenker$ROADREF_END

til <- str_split(til, " ")

table(lengths(til))
#      3      5 
# 106402    650 
# vegreferanser med 5 elementer er del av et kryss-system

# liste --> df
til <- plyr::ldply(til, rbind)

### Vegkategori & nummer ----

# EV6 - vegkategori = E, vegfase = V, vegnummer = 6

VK_VN <- til$`1`

VK <- str_sub(VK_VN, 1, 1)

VN <- as.numeric(str_extract(VK_VN, "[[:digit:]]+"))

### Strekning & Delstrekning ----

S_D <- til$`2`

S <- str_split_fixed(S_D, "D", n = 2)

D <- as.numeric(S[, 2])

S <- as.numeric(str_extract(S[, 1], "[[:digit:]]+"))

### Meterverdi ----

# desimal punktum
M <- str_replace(til$`3`, ",", ".")

M <- round(as.numeric(str_extract(M, "[[:digit:]]+\\.*[[:digit:]]*")))

### Kryss ----

KD <- til$`4`

# desimal punktum
KD_M <- str_replace(til$`5`, ",", ".")

KD_M <- round(as.numeric(str_extract(KD_M, "[[:digit:]]+\\.*[[:digit:]]*")))

### til koblingsnøkkel ----

df_til <- tibble(
  KOMMUNE = trafikklenker$MUNICIPALITY,
  REF = trafikklenker$ROADREF_END, 
  VK,
  VN,
  S, 
  D,
  M,
  KD,
  KD_M,
  VK_VN,
  S_D)

df_til <- df_til %>% 
  unite("VK_VN_SD", c(VK_VN, S_D), sep = " ", na.rm = TRUE, remove = FALSE) %>% 
  mutate(KOMM_REF = if_else(VK == "K", paste(KOMMUNE, VK_VN), VK_VN),
         KOMM_VEGSYSREF = if_else(VK == "K", paste(KOMMUNE, VK_VN, S_D), paste(VK_VN, S_D)))

# legg til til i kolonnenavnene
df_til <- df_til %>% rename_all(function(x) paste0("TIL_", x))

# rydd opp
rm(til, VK, VN, VK_VN, S, D, S_D, M, KD, KD_M)

# Koblingstabell --------------------------------------------------------------

df <- bind_cols(df_fra, df_til)

df <- df %>% 
  select(-c(FRA_KOMMUNE, TIL_KOMMUNE)) %>% # kommunenummer er like, så sletter
  mutate(lenke_nr = trafikklenker$lenke_nr,
         ID = trafikklenker$ID, 
         RETNING = trafikklenker$DIRECTION,
         med_metrering = trafikklenker$med_metrering,
         .before = 1)

n_distinct(df$FRA_VK_VN)
# 6038 unike referanse uten kommunenummer

n_distinct(df$FRA_KOMM_REF)
# 14459 unike referanser med kommunenummer på de kommunale vegene


# Start- og endepunkt på samme vegstrekning ----

samme_vegstrekning <- df %>% 
  filter(is.na(FRA_KD) & is.na(TIL_KD)) %>% # ikke på kryssdel
  filter(FRA_KOMM_VEGSYSREF == TIL_KOMM_VEGSYSREF) %>% # samme kommune nr VK+VN+S+D
  mutate(KATEGORI = "Samme vegstrekning",
         VEGSYSTEMREFERANSE = paste0(FRA_VK_VN_SD, " m", FRA_M, "-", TIL_M))

nrow(samme_vegstrekning) # 103757

# Start- og endepunkt på samme kryssdel ----

samme_kryssdel <- df %>% 
  filter(!is.na(FRA_KD) & !is.na(TIL_KD), # på kryssdel
         FRA_KOMM_VEGSYSREF == TIL_KOMM_VEGSYSREF, # samme kommune nr VK+VN+S+D
         FRA_KD == TIL_KD) %>% # samme kryssdel
  mutate(KATEGORI = "Samme kryssdel",
         VEGSYSTEMREFERANSE = paste0(FRA_VK_VN_SD, " m", FRA_M, " ", FRA_KD, " m", FRA_KD_M, "-", TIL_KD_M))

nrow(samme_kryssdel) # 644

tmap_mode("view")
tm_shape(filter(trafikklenker, ID %in% samme_kryssdel$ID)) +
  tm_lines()
# Dette er ramper o.l.

# ingen overlapp for lenkene
intersect(samme_vegstrekning$ID, samme_kryssdel$ID)

# Samme veg, ulik strekning ----

samme_veg <- df %>% 
  filter(is.na(FRA_KD) & is.na(TIL_KD)) %>% # ikke på kryssdel
  filter(FRA_KOMM_REF == TIL_KOMM_REF) %>%  # samme kommune nr VK+VN
  filter(FRA_S != TIL_S, FRA_D == TIL_D) %>%  # ulik S, samme D
  mutate(KATEGORI = "Samme veg, ulik strekning",
         VEGSYSTEMREFERANSE = NA_character_)

nrow(samme_veg) # 2645

# ingen overlapp for lenkene
intersect(samme_vegstrekning$ID, samme_veg$ID)

# slå sammen de som har lik veg/vegstrekning/kryssdel for start og sluttpunkt
kobling <- bind_rows(samme_vegstrekning, samme_veg, samme_kryssdel)

# Ulik veg- eller kryssdel ----

nrow(df) - nrow(kobling) # 6

# hent ut de som er ulike på en eller annen måte
ulike <- df %>% 
  filter(!ID %in% kobling$ID) %>% 
  mutate(KATEGORI = "Ulik veg- eller kryssdel", 
         VEGSYSTEMREFERANSE = NA_character_)

nrow(ulike) # 6

# Lagre koblingsnøkkel ----

# legg til de siste lenkene
kobling <- bind_rows(kobling, ulike)

# sorter
kobling <- kobling %>% arrange(ID)

# sjekk at alle er med

nrow(df) == nrow(kobling)
all(df$ID %in% kobling$ID)

kobling %>% 
  count(KATEGORI) %>% 
  mutate(p = n/nrow(kobling)*100) %>% 
  arrange(desc(n))

# # A tibble: 4 x 3
# KATEGORI                       n        p
# <chr>                      <int>    <dbl>
# 1 Samme vegstrekning        103757 96.9    
# 2 Samme veg, ulik strekning   2645  2.47   
# 3 Samme kryssdel               644  0.602  
# 4 Ulik veg- eller kryssdel       6  0.00560

# legg til variabler for metreringsretning og kommunale referanser som ikke er unike

kobling <- kobling %>%
  mutate(
    DIFF_METRERING = case_when(
      KATEGORI == "Samme vegstrekning" ~ TIL_M - FRA_M,
      KATEGORI == "Samme kryssdel" ~ TIL_KD_M - FRA_KD_M,
      TRUE ~ NA_real_
    ),
    SIGN_METRERING = sign(DIFF_METRERING), 
    IKKE_UNIK_REF = if_else(ID %in% id_ikke_unike_kommunale_veier, TRUE, FALSE)
  )

write_delim(kobling, file = "data/region-vest/2021_04/trafikklenker_vegsystemreferanser.csv", delim = ";")

