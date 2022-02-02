#
# Forbereder påkobling av nettutlagt ÅDT basert på vegsystemreferanser:
#
# 1) Undersøker lenker som ikke har fått ny vegreferanse
# 2) Oppdaterer kommunenummer til 2020
# 3) Splitter opp vegreferanser og forbereder kobling
# 4) Kategoriserer type lenke, f.eks. er start og slutt på samme strekning
# 5) Lagrer data.frame med ÅDT og vegsystemreferanser for kobling:
#    "data/region-vest/2021_04/cube_vegsystemreferanser.csv"
#

library(sf)
library(tmap)
library(tidyverse)
library(stringi)

# Les inn Cube-lenker med ÅDT og vegsystemreferanser fra 2020 -----------------

path_vegref_cube <- "I:/ØL-AVD/4876 NY ÅDT/Transportmodell/Ny vegreferanse/NVDB-oppslag/Merged_Cube_totalt.xlsx"

cube <- readxl::read_excel(path_vegref_cube)

# Lenker uten nye referanser --------------------------------------------------

# Endre "-99999" til NA
cube <- na_if(cube, "-99999")

sum(is.na(cube$HP_ID))
# 7 missing HP_ID

cube %>% filter(is.na(FROM_Vegreferanse2020) | is.na(TO_Vegreferanse2020)) %>% nrow()
# 9169 missing ref på minst ett punkt

cube %>% filter(is.na(FROM_Vegreferanse2020) & is.na(TO_Vegreferanse2020)) %>% nrow()
# 7293 missing ref på begge punkt

# Hva slags veger mangler referanser?
cube %>% filter(is.na(FROM_Vegreferanse2020) | is.na(TO_Vegreferanse2020)) %>% count(VK)
# Det er flest kommunale veger, men også ERF-veger
# VK        n
#  E       252
#  F       490
#  K      7848
#  P       478
#  R        94
#  NA        7

# Har disse vegene ÅDT?
cube %>% 
  filter(is.na(FROM_Vegreferanse2020) | is.na(TO_Vegreferanse2020)) %>% 
  pull(AADT_ALLE) %>% 
  hist("Scott")

# Ser ut som mange av dem har AADT = 0, sjekker:
cube %>% 
  filter(is.na(FROM_Vegreferanse2020) | is.na(TO_Vegreferanse2020) & AADT_ALLE == 0) %>% 
  nrow()
# 8925, dvs. 97 prosent


# Oppdater kommunenummer til 2020 ---------------------------------------------

kommnr <- readxl::read_excel("data/fylker/fylker-kommuner-2019-2020-alle.xlsx")

# fiks variabelnavn
kommnr <- rename_with(kommnr, ~ tolower(gsub(" ", "_", .x, fixed = TRUE)))
kommnr <- rename_with(kommnr, ~ tolower(gsub(".", "", .x, fixed = TRUE)))

# De fire første sifrene i HP_ID er kommunenummer
cube <- cube %>% 
  mutate(kommune_2018 = as.numeric(str_sub(HP_ID, 1, 4)))

cube <- left_join(cube, kommnr, by = c("kommune_2018" = "kommunenr_2019"))

# Hvilke 2018-kommuner har ikke fått nytt kommunenummer?
cube %>% 
  filter(is.na(kommunenr_2020)) %>% 
  count(kommune_2018)
#   kommune_2018     n
#          <dbl> <int>
# 1          500  3306
# 2          600  1492
# 3          800  1423
# 4          900  1957
# 5         1000 10413
# 6         1100 18984
# 7         1200 23298
# 8         1400  9531
# 9         1500  8844
# 10           NA     7

# Dette er ERF-veger, som har fått kommunenummer 00
cube %>% 
  filter(is.na(kommunenr_2020)) %>% 
  count(VK)
#   VK        n
#   <chr> <int>
# 1 E      9460
# 2 F     61546
# 3 R      6928

# Mulig det blir nødvendig å bruke fylkesnummer for rett kobling, se an dette til senere

# Klargjør vegsystemreferanser for matching -----------------------------------

## Fra ----

fra <- cube$FROM_Vegreferanse2020

fra <- str_split(fra, " ")

table(lengths(fra))
#    1      3      5 
# 8239 212219   4783  
# vegreferanser med 5 elementer er del av et kryss-system eller sideanlegg
# vegreferansene med 1 element har kun vegnummer
# "PV4928"  "KV2060"  "KV1958"

View(cube[which(lengths(fra) == 1), ])

# liste --> df
fra <- plyr::ldply(fra, rbind)

### Vegkategori & nummer ----

# EV6 - vegkategori = E, vegfase = V, vegnummer = 6

VK_VN <- fra$`1`

# triona lenkene har ikke med status som disse
table(str_sub(VK_VN, 2, 2))
# A      V 
# 3 217007 
# tre i anleggsfasen

# fjerner V og A
VK_VN <- str_replace(VK_VN, "V", "")
VK_VN <- str_replace(VK_VN, "A", "")

VK <- str_sub(VK_VN, 1, 1)

VN <- as.numeric(str_extract(VK_VN, "[[:digit:]]+"))

### Strekning & Delstrekning ----

S_D <- fra$`2`

S <- str_split_fixed(S_D, "D", n = 2)

D <- as.numeric(S[, 2])

S <- as.numeric(str_extract(S[, 1], "[[:digit:]]+"))

### Meterverdi ----

M <- fra$`3`

M <- as.numeric(str_extract(M, "[[:digit:]]+"))

### Kryss ----

KD <- fra$`4`

KD_M <- fra$`5`

KD_M <- as.numeric(str_extract(KD_M, "[[:digit:]]+"))

### Fra koblingsnøkkel ----

df_fra <- tibble(
  KOMMUNE = replace_na(cube$kommunenr_2020, ""),
  REF = cube$FROM_Vegreferanse2020, 
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

til <- cube$TO_Vegreferanse2020

til <- str_split(til, " ")

table(lengths(til))
#    1      3      5 
# 8239 212219   4783 

# liste --> df
til <- plyr::ldply(til, rbind)

### Vegkategori & nummer ----

# EV6 - vegkategori = E, vegfase = V, vegnummer = 6

VK_VN <- til$`1`

# triona lenkene har ikke med status som disse
table(str_sub(VK_VN, 2, 2))
# A      V 
# 4 217006 
# fire i anleggsfasen

# fjerner V og A
VK_VN <- str_replace(VK_VN, "V", "")
VK_VN <- str_replace(VK_VN, "A", "")

VK <- str_sub(VK_VN, 1, 1)

VN <- as.numeric(str_extract(VK_VN, "[[:digit:]]+"))

### Strekning & Delstrekning ----

S_D <- til$`2`

S <- str_split_fixed(S_D, "D", n = 2)

D <- as.numeric(S[, 2])

S <- as.numeric(str_extract(S[, 1], "[[:digit:]]+"))

### Meterverdi ----

M <- til$`3`

M <- as.numeric(str_extract(M, "[[:digit:]]+"))

### Kryss ----

KD <- til$`4`

KD_M <- til$`5`

KD_M <- as.numeric(str_extract(KD_M, "[[:digit:]]+"))

### til koblingsnøkkel ----

df_til <- tibble(
  KOMMUNE = replace_na(cube$kommunenr_2020, ""),
  REF = cube$TO_Vegreferanse2020, 
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
  mutate(ID_CUBE = cube$id, 
         HP_ID = cube$HP_ID,
         AADT_ALLE = cube$AADT_ALLE,
         AADT_TUNGE = cube$AADT_TUNGE,
         AADT_LETTE = cube$AADT_LETTE,
         KOMMUNE_2018 = cube$kommune_2018,
         KOMMUNE_2020 = cube$kommunenr_2020,
         .before = 1)

n_distinct(df$FRA_VK_VN)
# 8955 unike referanse uten kommunenummer

n_distinct(df$FRA_KOMM_REF)
# 21666 unike referanser med kommunenummer på de kommunale vegene

# Missing vegreferanse på minst en node

mangler <- df %>% 
  filter(is.na(FRA_REF) | is.na(TIL_REF)) %>% 
  mutate(KATEGORI = "Mangler vegreferanse")

# Start- og endepunkt på samme vegstrekning ----

samme_vegstrekning <- df %>% 
  filter(!is.na(FRA_REF) & !is.na(TIL_REF)) %>% # har vegreferanse på begge
  filter(is.na(FRA_KD) & is.na(TIL_KD)) %>% # ikke på kryssdel
  filter(FRA_KOMM_VEGSYSREF == TIL_KOMM_VEGSYSREF) %>% # samme kommune nr VK+VN+S+D
  mutate(KATEGORI = "Samme vegstrekning", 
         VEGSYSTEMREFERANSE = paste0(FRA_REF, "-", TIL_M))

nrow(samme_vegstrekning) # 204434

# Start- og endepunkt på samme kryssdel ----

samme_kryssdel <- df %>% 
  filter(!is.na(FRA_REF) & !is.na(TIL_REF)) %>% # har vegreferanse på begge
  filter(!is.na(FRA_KD) & !is.na(TIL_KD), # på kryssdel
         FRA_KOMM_VEGSYSREF == TIL_KOMM_VEGSYSREF, # samme kommune nr VK+VN+S+D
         FRA_KD == TIL_KD) %>% # samme kryssdel
  mutate(KATEGORI = "Samme kryssdel",
         VEGSYSTEMREFERANSE = paste0(FRA_REF, "-", TIL_KD_M))

nrow(samme_kryssdel) # 4712

# ingen overlapp for lenkene
intersect(samme_vegstrekning$ID_CUBE, samme_kryssdel$ID_CUBE)

# Samme veg, ulik strekning ----

samme_veg <- df %>% 
  filter(!is.na(FRA_REF) & !is.na(TIL_REF)) %>% # har vegreferanse på begge
  filter(is.na(FRA_KD) & is.na(TIL_KD)) %>% # ikke på kryssdel
  filter(FRA_KOMM_REF == TIL_KOMM_REF) %>%  # samme kommune nr VK+VN
  filter(FRA_S != TIL_S, FRA_D == TIL_D) %>%  # ulik S, samme D
  mutate(KATEGORI = "Samme veg, ulik strekning", 
         VEGSYSTEMREFERANSE = NA_character_)

nrow(samme_veg) # 2388

# ingen overlapp for lenkene
intersect(samme_vegstrekning$ID_CUBE, samme_veg$ID_CUBE)

# slå sammen de som har lik veg/vegstrekning/kryssdel for start og sluttpunkt
kobling <- bind_rows(mangler, samme_vegstrekning, samme_veg, samme_kryssdel)

# Ulik veg- eller kryssdel ----

nrow(df) - nrow(kobling) # 4538

# hent ut de som er ulike på en eller annen måte
ulike <- df %>% 
  filter(!ID_CUBE %in% kobling$ID_CUBE) %>% 
  mutate(KATEGORI = "Ulik veg- eller kryssdel",
         VEGSYSTEMREFERANSE = NA_character_)

nrow(ulike) # 4538

# Lagre koblingsnøkkel ----

# legg til de siste lenkene
kobling <- bind_rows(kobling, ulike)

# sorter
kobling <- kobling %>% arrange(ID_CUBE)

# sjekk at alle er med

nrow(df) == nrow(kobling)
all(df$ID_CUBE %in% kobling$ID_CUBE)

kobling %>% 
  count(KATEGORI) %>% 
  mutate(p = n/nrow(kobling)*100) %>% 
  arrange(desc(n))

# # A tibble: 5 x 3
# KATEGORI                       n     p
# <chr>                      <int> <dbl>
# 1 Samme vegstrekning        204434 90.8 
# 2 Mangler vegreferanse        9169  4.07
# 3 Samme kryssdel              4712  2.09
# 4 Ulik veg- eller kryssdel    4538  2.01
# 5 Samme veg, ulik strekning   2388  1.06

kobling <- kobling %>%
  mutate(
    DIFF_METRERING = case_when(
      KATEGORI == "Samme vegstrekning" ~ TIL_M - FRA_M,
      KATEGORI == "Samme kryssdel" ~ TIL_KD_M - FRA_KD_M,
      TRUE ~ NA_real_
    ),
    SIGN_METRERING = sign(DIFF_METRERING)
  )

write_delim(kobling, file = "data/region-vest/2021_04/cube_vegsystemreferanser.csv", delim = ";")



