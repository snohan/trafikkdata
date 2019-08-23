#
# Hente data fra NVDBs API.
#

# Pakker ####
library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)

# Erfaringer:
# Overlapp mellom strekningselementer: sett den mest finoppdelte først, så
# synkende finoppdeling. Dette for å unngå dobbeltelling.

# Definer URI og sti ####

nvdb_url <- "https://www.vegvesen.no/nvdb/api/v2"

sti_vegobjekter <- "/vegobjekter"

# Hent målestasjoner med gitt fartsgrense ####
# (Greit å dele opp spørringen, for det er tydeligvis grense på 1000 svar.)
trafikkreg_med_egenskap <- "/482?egenskap=("
kilde_trafikkreg <- "egenskap(9292)=12989"
and_op <- "%20AND%20"
nivaa_1 <- "egenskap(3910)=4892"
trgruppe_motor <- "egenskap(9293)=12992"
or_op <- "%20OR%20"
status_op <- "egenskap(5201)=7081"
status_mud <- "egenskap(5201)=12987"

# Bestemmer hvilke data vi vil ha med
inkluder <- "&inkluder=egenskaper"

# Lager overlapp med gitt fartsgrense
fartsgrense <- "&overlapp=105(2021="

fart_30 <- "2726)"
fart_40 <- "2728)"
fart_50 <- "2730)"
fart_60 <- "2732)"
fart_70 <- "2735)"
fart_80 <- "2738)"
fart_90 <- "2741)"
fart_100 <- "5087)"
fart_110 <- "9721)"

# Setter sammen
api_query <- paste(nvdb_url,
                   sti_vegobjekter,
                   trafikkreg_med_egenskap,
                   kilde_trafikkreg,
                   and_op,
                   nivaa_1,
                   and_op,
                   trgruppe_motor,
                   and_op,
                   "(",
                   status_op,
                   or_op,
                   status_mud,
                   ")",
                   ")",
                   inkluder,
                   fartsgrense,
                   sep = "")

hent_for_fartsgrense <- function(fart) {
  # Tar inn koden for gitt fartsgrense i NVDB
  # Returnerer tabell med målestasjoner med gitt fartsgrense

  api_query_fart <- paste0(api_query, fart)

  respons <- GET(api_query_fart)

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  ms <- bind_rows(uthenta$objekter$egenskaper, .id = "stasjon")

  stasjoner <- ms %>%
    filter(id %in% c(5201, 4626, 4627)) %>%
    select(stasjon, navn, verdi) %>%
    spread(navn, verdi) %>%
    select(-1) %>%
    select(1, 3, 2)

  colnames(stasjoner) <- c("msnr", "navn", "status")

  return(stasjoner)
}

stasjoner_30 <- hent_for_fartsgrense(fart_30) %>%
  mutate(fartsgrense = "30")
stasjoner_40 <- hent_for_fartsgrense(fart_40) %>%
  mutate(fartsgrense = "40")
stasjoner_50 <- hent_for_fartsgrense(fart_50) %>%
  mutate(fartsgrense = "50")
stasjoner_60 <- hent_for_fartsgrense(fart_60) %>%
  mutate(fartsgrense = "60")
stasjoner_70 <- hent_for_fartsgrense(fart_70) %>%
  mutate(fartsgrense = "70")
stasjoner_80 <- hent_for_fartsgrense(fart_80) %>%
  mutate(fartsgrense = "80")
stasjoner_90 <- hent_for_fartsgrense(fart_90) %>%
  mutate(fartsgrense = "90")
stasjoner_100 <- hent_for_fartsgrense(fart_100) %>%
  mutate(fartsgrense = "100")
stasjoner_110 <- hent_for_fartsgrense(fart_110) %>%
  mutate(fartsgrense = "110")

stasjoner_med_fartsgrense <- bind_rows(stasjoner_30,
                                       stasjoner_40,
                                       stasjoner_50,
                                       stasjoner_60,
                                       stasjoner_70,
                                       stasjoner_80,
                                       stasjoner_90,
                                       stasjoner_100,
                                       stasjoner_110)

write.csv2(stasjoner_110,
           file = "stasjoner_med_fartsgrense_110_20190507.csv",
           row.names = F)

# AADT-belegging ####

hent_trafikkmengde_for_fylke_aar <- function(fylkenr, aarstall) {
  # Input: fylkenr og årstall.
  # Ut: tabell med ÅDT-tall per "ÅDT-lenke".

  api_query_aadt <- paste0(nvdb_url,
                           sti_vegobjekter,
                           "/540?",
                           #egenskap=(4621=","2016", #aarstall,
                           "fylke=",
                           "3", #fylkenr,
                           "&vegreferanse=E,R,F",
                           "&inkluder=egenskaper,lokasjon")

  respons <- GET(api_query_aadt,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  aadt <- bind_rows(uthenta$objekter$egenskaper, .id = "trafikkmengde")
  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  aadt_lenker <- aadt %>%
    filter(id == 4623) %>%
    select(navn, verdi)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:6)

  aadt_tabell <- bind_cols(aadt_lenker, vegref_unike, lenkelengde) %>%
    select(2:7)

  colnames(aadt_tabell) <- c("aadt", "fylke", "vegkategori", "vegstatus",
                             "vegnummer", "lenkelengde")

  aadt_tabell$aadt <- as.integer(aadt_tabell$aadt)

  return(aadt_tabell)
}

aadt_sum <- aadt_tabell %>%
  mutate(trafikkarbeid = (aadt * lenkelengde * 365) / 1e9) %>%
  group_by(vegkategori) %>%
  summarise(trafikkarbeid = round(sum(trafikkarbeid), digits = 0),
            veglengde = sum(lenkelengde) / 1e3)

str(aadt_tabell)


# Veglengder per fartsgrense ####
hent_motorveglengde_per_fartsgrense <- function(fart) {

  api_query_motorveglengde <- paste0(nvdb_url,
                                     sti_vegobjekter,
                                     "/595?",
                                     "&inkluder=egenskaper,lokasjon",
                                     "&overlapp=105(2021=",
                                     fart)

  respons <- GET(api_query_motorveglengde,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  motorveger <- bind_rows(uthenta$objekter$egenskaper, .id = "motorveg")
  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  motorveg_lenker <- motorveger %>%
    filter(id == 5378) %>%
    select(navn, verdi)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:6)

  motorveg_tabell <- bind_cols(motorveg_lenker, vegref_unike, lenkelengde) %>%
    select(2:7)

  colnames(motorveg_tabell) <- c("motorvegtype", "fylke", "vegkategori",
                             "vegstatus",
                             "vegnummer", "lenkelengde")
  return(motorveg_tabell)

}

df <- hent_motorveglengde_per_fartsgrense(fart_90)

motorveg_40 <- hent_motorveglengde_per_fartsgrense(fart_40) %>%
  mutate(fartsgrense = "40")
motorveg_50 <- hent_motorveglengde_per_fartsgrense(fart_50) %>%
  mutate(fartsgrense = "50")
motorveg_60 <- hent_motorveglengde_per_fartsgrense(fart_60) %>%
  mutate(fartsgrense = "60")
motorveg_70 <- hent_motorveglengde_per_fartsgrense(fart_70) %>%
  mutate(fartsgrense = "70")
motorveg_80 <- hent_motorveglengde_per_fartsgrense(fart_80) %>%
  mutate(fartsgrense = "80")
motorveg_90 <- hent_motorveglengde_per_fartsgrense(fart_90) %>%
  mutate(fartsgrense = "90")
motorveg_100 <- hent_motorveglengde_per_fartsgrense(fart_100) %>%
  mutate(fartsgrense = "100")
motorveg_110 <- hent_motorveglengde_per_fartsgrense(fart_110) %>%
  mutate(fartsgrense = "110")

motorveg_med_fartsgrense <- bind_rows(motorveg_40,
                                       motorveg_50,
                                       motorveg_60,
                                       motorveg_70,
                                       motorveg_80,
                                       motorveg_90,
                                       motorveg_100,
                                       motorveg_110) %>%
  filter(vegkategori != "F")


sum(motorveg_med_fartsgrense$lenkelengde)

# Variabel fartsgrense ####
hent_motorveglengde_variabel_fartsgrense <- function() {

  api_query_motorveglengde <- paste0(nvdb_url,
                                     sti_vegobjekter,
                                     "/595?",
                                     "&inkluder=egenskaper,lokasjon",
                                     "&overlapp=721")

  respons <- GET(api_query_motorveglengde,
                 add_headers("X-Client" = "trafikkdatagruppa",
                             "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                             "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  motorveger <- bind_rows(uthenta$objekter$egenskaper, .id = "motorveg")
  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  motorveg_lenker <- motorveger %>%
    filter(id == 5378) %>%
    select(navn, verdi)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:6)

  motorveg_tabell <- bind_cols(motorveg_lenker, vegref_unike, lenkelengde) %>%
    select(2:7)

  colnames(motorveg_tabell) <- c("motorvegtype", "fylke", "vegkategori",
                                 "vegstatus",
                                 "vegnummer", "lenkelengde")
  return(motorveg_tabell)

}

motorveg_med_variabel_fartsgrense <- hent_motorveglengde_variabel_fartsgrense()
sum(motorveg_med_variabel_fartsgrense$lenkelengde)


# Vegreferanse som hovedobjekt, og overlapp med motorveg og fartsgrense ####
vegtype <- "7356"
fart <- "2730)"

hent_vegref_motorveg_fartsgrense <- function(vegtype, fart) {
  # Motorvegtype: 7355 Motorveg, 7356 Motortrafikkveg

  api_query_motorveglengde <-
    paste0(nvdb_url,
           sti_vegobjekter,
           "/532?egenskap=(egenskap(4567)=5499", # eksisterende
           "%20AND%20egenskap(4574)!=9725", # ikke fiktiv
           "%20AND%20egenskap(4707)!=5777)", # ikke enveg mot
           "&inkluder=egenskaper,lokasjon",
           "&overlapp=595(5378=",
           vegtype,
           ")",
           "&overlapp=105(2021=",
           #"2730)"
           fart
    )


api_query_motorveglengde <- "https://www.vegvesen.no/nvdb/api/v2/vegobjekter/105?egenskap=(egenskap(2021)=2730)&inkluder=egenskaper,lokasjon&kommune=5001&vegreferanse=Rv706&overlapp=532(egenskap(4567)=5499%20AND%20egenskap(4574)!=9725%20AND%20egenskap(4707)!=5777)"

  respons <-
    GET(api_query_motorveglengde,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  #motorveger <- bind_rows(uthenta$objekter$egenskaper, .id = "motorveg")
  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:10)

  motorveg_tabell <- bind_cols(vegref_unike, lenkelengde)
  # %>%
  #   mutate(fra_til = til_meter - fra_meter) %>%
  #   mutate(lengdeforskjell = lenkelengde - fra_til)

  colnames(motorveg_tabell) <- c("fylke", "vegkategori",
                                 "vegstatus",
                                 "vegnummer", "parsell",
                                 "fra_meter", "til_meter",
                                 "vegref_kortform",
                                 "lenkelengde"#, "fra_til-lengde"
                                 )

  return(motorveg_tabell)
}

motorveg_tabell_60 <- motorveg_tabell
sum(motorveg_tabell$lenkelengde)

motorveg_40 <- hent_vegref_motorveg_fartsgrense("7355", fart_40) %>%
  mutate(fartsgrense = "40")
motorveg_50 <- hent_vegref_motorveg_fartsgrense("7355", fart_50) %>%
  mutate(fartsgrense = "50")
motorveg_60 <- hent_vegref_motorveg_fartsgrense("7355", fart_60) %>%
  mutate(fartsgrense = "60")
motorveg_70 <- hent_vegref_motorveg_fartsgrense("7355", fart_70) %>%
  mutate(fartsgrense = "70")
motorveg_80 <- hent_vegref_motorveg_fartsgrense("7355", fart_80) %>%
  mutate(fartsgrense = "80")
motorveg_90 <- hent_vegref_motorveg_fartsgrense("7355", fart_90) %>%
  mutate(fartsgrense = "90")
motorveg_100 <- hent_vegref_motorveg_fartsgrense("7355", fart_100) %>%
  mutate(fartsgrense = "100")
motorveg_110 <- hent_vegref_motorveg_fartsgrense("7355", fart_110) %>%
  mutate(fartsgrense = "110")

motorveg_med_fartsgrense <- bind_rows(motorveg_40,
                                      motorveg_50,
                                      motorveg_60,
                                      motorveg_70,
                                      motorveg_80,
                                      motorveg_90,
                                      motorveg_100,
                                      motorveg_110) %>%
  filter(vegkategori != "F") %>%
  mutate(vegtype = "Motorveg")

motortrafikkveg_40 <- hent_vegref_motorveg_fartsgrense("7356", fart_40) %>%
  mutate(fartsgrense = "40")
motortrafikkveg_50 <- hent_vegref_motorveg_fartsgrense("7356", fart_50) %>%
  mutate(fartsgrense = "50")
motortrafikkveg_60 <- hent_vegref_motorveg_fartsgrense("7356", fart_60) %>%
  mutate(fartsgrense = "60")
motortrafikkveg_70 <- hent_vegref_motorveg_fartsgrense("7356", fart_70) %>%
  mutate(fartsgrense = "70")
motortrafikkveg_80 <- hent_vegref_motorveg_fartsgrense("7356", fart_80) %>%
  mutate(fartsgrense = "80")
motortrafikkveg_90 <- hent_vegref_motorveg_fartsgrense("7356", fart_90) %>%
  mutate(fartsgrense = "90")
motortrafikkveg_100 <- hent_vegref_motorveg_fartsgrense("7356", fart_100) %>%
  mutate(fartsgrense = "100")
motortrafikkveg_110 <- hent_vegref_motorveg_fartsgrense("7356", fart_110) %>%
  mutate(fartsgrense = "110")

motortrafikkveg_med_fartsgrense <- bind_rows(motortrafikkveg_50,
                                             motortrafikkveg_60,
                                             motortrafikkveg_70,
                                             motortrafikkveg_80,
                                             motortrafikkveg_90,
                                             motortrafikkveg_100,
                                             motortrafikkveg_110) %>%
  filter(vegkategori != "F") %>%
  mutate(vegtype = "Motortrafikkveg")

motorveger <- rbind(motorveg_med_fartsgrense, motortrafikkveg_med_fartsgrense)

motorveg_summert <- motorveger %>%
  group_by(
    vegtype
    #,
    #fartsgrense
    ) %>%
  summarise(veglengde = sum(lenkelengde)/1000)

sum(motorveg_med_fartsgrense$lenkelengde)

# Motorveg med variabel fartsgrense ####

hent_vegref_motorveg_varfartsgrense <- function(vegtype) {
  # Motorvegtype: 7355 Motorveg, 7356 Motortrafikkveg

  api_query_motorveglengde <-
    paste0(nvdb_url,
           sti_vegobjekter,
           "/532?egenskap=(egenskap(4567)=5499", # eksisterende
           "%20AND%20egenskap(4574)!=9725", # ikke fiktiv
           "%20AND%20egenskap(4707)!=5777)", # ikke enveg mot
           "&inkluder=egenskaper,lokasjon",
           "&overlapp=595(5378=",
           vegtype,
           ")",
           "&overlapp=721"
    )

  respons <-
    GET(api_query_motorveglengde,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:6, 10)

  motorveg_tabell <- bind_cols(vegref_unike, lenkelengde)

  colnames(motorveg_tabell) <- c("fylke", "vegkategori",
                                 "vegstatus",
                                 "vegnummer",
                                 "vegref_kortform",
                                 "lenkelengde"
  )

  return(motorveg_tabell)
}

motorveg_varfart <- hent_vegref_motorveg_varfartsgrense("7355")
#motortrafikkveg_varfart <- hent_vegref_motorveg_varfartsgrense("7356")
sum(motorveg_varfart$lenkelengde)

# Veg med variabel fartsgrense ####

hent_vegref_varfartsgrense <- function() {

  api_query_motorveglengde <-
    paste0(nvdb_url,
           sti_vegobjekter,
           "/532?egenskap=(egenskap(4567)=5499", # eksisterende
           "%20AND%20egenskap(4574)!=9725", # ikke fiktiv
           "%20AND%20egenskap(4707)!=5777)", # ikke enveg mot
           "&inkluder=egenskaper,lokasjon",
           "&overlapp=721"
    )

  respons <-
    GET(api_query_motorveglengde,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

    vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:6, 10)

  motorveg_tabell <- bind_cols(vegref_unike, lenkelengde)

  colnames(motorveg_tabell) <- c("fylke", "vegkategori",
                                 "vegstatus",
                                 "vegnummer",
                                 "vegref_kortform",
                                 "lenkelengde"
  )

  return(motorveg_tabell)
}

veg_varfart <- hent_vegref_varfartsgrense()

veg_varfart_sum -> veg_varfart %>%
  filter(vegkategori != "F") %>%
  summarise(veglengde = sum(lenkelengde))


# Hent fart, vegref ####

# Definerer spørrestrengen:
api_query <- #_fart_vegref <-
  paste0(nvdb_url,
         sti_vegobjekter,
         "/105?egenskap=(egenskap(2021)=2728)&", # fartsgrense
         "inkluder=egenskaper,lokasjon",
         "&overlapp=532",
         "(egenskap(4567)=5499", # eksisterende
         "%20AND%20egenskap(4574)!=9725", # ikke fiktiv
         "%20AND%20(egenskap(4566)=5492", # E
         "%20OR%20egenskap(4566)=5493)", # R
         "%20AND%20egenskap(4707)!=5777)" # ikke enveg mot
  )

hent_fart_vegref <- function(api_query) {

  respons <-
    GET(api_query,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  returnert <- uthenta$metadata$returnert
  neste <- uthenta$metadata$neste$href

  #motorveger <- bind_rows(uthenta$objekter$egenskaper, .id = "motorveg")
  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:10)

  motorveg_tabell <- bind_cols(vegref_unike, lenkelengde)
  # %>%
  #   mutate(fra_til = til_meter - fra_meter) %>%
  #   mutate(lengdeforskjell = lenkelengde - fra_til)

  colnames(motorveg_tabell) <- c("fylke", "vegkategori",
                                 "vegstatus",
                                 "vegnummer", "parsell",
                                 "fra_meter", "til_meter",
                                 "vegref_kortform",
                                 "lenkelengde"#, "fra_til-lengde"
  )

  return(motorveg_tabell)
}

# Hent vegref, fart, motorveg ####
hent_vegref_fart_motorveg <- function(fg, mv) {

  api_query_veg_fart_motorveg <-
    paste0(nvdb_url,
           sti_vegobjekter,
           "/532?egenskap=(egenskap(4567)=5499", # eksisterende
           "%20AND%20egenskap(4574)!=9725", # ikke fiktiv
           "%20AND%20(egenskap(4566)=5492", # E
           "%20OR%20egenskap(4566)=5493)", # R
           "%20AND%20egenskap(4707)!=5777)", # ikke enveg mot
           "&inkluder=egenskaper,lokasjon",
           "&overlapp=105(egenskap(2021)=",
           fg, # fartsgrense:
           #"2730)",
           "&overlapp=595(5378=",
           mv, # motorvegtype: 7355 eller 7356 (motorv og motortrfv)
           ")"
    )

  respons <-
    GET(api_query_veg_fart_motorveg,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")
  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  vegref_unike <- vegref %>%
    filter(!duplicated(vegreferanse)) %>%
    select(2, 4:6, 10)

  motorveg_tabell <- bind_cols(vegref_unike, lenkelengde)

  colnames(motorveg_tabell) <- c("fylke", "vegkategori",
                                 "vegstatus",
                                 "vegnummer",
                                 "vegref_kortform",
                                 "lenkelengde"
  )

  return(motorveg_tabell)
}

# motorveg_30 <- hent_vegref_fart_motorveg(fart_30, "7355") # fins ikke
# motorveg_40 <- hent_vegref_fart_motorveg(fart_40, "7355") # fins ikke
motorveg_50 <- hent_vegref_fart_motorveg(fart_50, "7355") %>%
  mutate(fartsgrense = 50)
motorveg_60 <- hent_vegref_fart_motorveg(fart_60, "7355") %>%
  mutate(fartsgrense = 60)
motorveg_70 <- hent_vegref_fart_motorveg(fart_70, "7355") %>%
  mutate(fartsgrense = 70)
motorveg_80 <- hent_vegref_fart_motorveg(fart_80, "7355") %>%
  mutate(fartsgrense = 80)
motorveg_90 <- hent_vegref_fart_motorveg(fart_90, "7355") %>%
  mutate(fartsgrense = 90)
motorveg_100 <- hent_vegref_fart_motorveg(fart_100, "7355") %>%
  mutate(fartsgrense = 100)
motorveg_110 <- hent_vegref_fart_motorveg(fart_110, "7355") %>%
  mutate(fartsgrense = 110)

motorveg_med_fartsgrense <- bind_rows(motorveg_50,
                                      motorveg_60,
                                      motorveg_70,
                                      motorveg_80,
                                      motorveg_90,
                                      motorveg_100,
                                      motorveg_110) %>%
  group_by(fartsgrense) %>%
  summarise(lengde = sum(lenkelengde))

motortrafikkveg_50 <- hent_vegref_fart_motorveg(fart_50, "7356") %>%
  mutate(fartsgrense = 50)
motortrafikkveg_60 <- hent_vegref_fart_motorveg(fart_60, "7356") %>%
  mutate(fartsgrense = 60)
motortrafikkveg_70 <- hent_vegref_fart_motorveg(fart_70, "7356") %>%
  mutate(fartsgrense = 70)
motortrafikkveg_80 <- hent_vegref_fart_motorveg(fart_80, "7356") %>%
  mutate(fartsgrense = 80)
motortrafikkveg_90 <- hent_vegref_fart_motorveg(fart_90, "7356") %>%
  mutate(fartsgrense = 90)
motortrafikkveg_100 <- hent_vegref_fart_motorveg(fart_100, "7356") %>%
  mutate(fartsgrense = 100)
motortrafikkveg_110 <- hent_vegref_fart_motorveg(fart_110, "7356") %>%
  mutate(fartsgrense = 110)

motortrafikkveg_med_fartsgrense <- bind_rows(motortrafikkveg_50,
                                             motortrafikkveg_60,
                                             motortrafikkveg_70,
                                             motortrafikkveg_80,
                                             motortrafikkveg_90,
                                             motortrafikkveg_100,
                                             motortrafikkveg_110) %>%
  group_by(fartsgrense) %>%
  summarise(lengde = sum(lenkelengde))

# Hent fart, motorveg ####

mv <- "7355"
hent_fart_motorveg <- function(mv) {

  api_query_fart_motorveg <-
    paste0(nvdb_url,
           sti_vegobjekter,
           "/105?", #"egenskap=(egenskap(2021)=2728)&", # fartsgrense
           "inkluder=egenskaper,lokasjon",
           #"&overlapp=595",
           "&overlapp=595(5378=",
           mv, # motorvegtype: 7355 eller 7356 (motorv og motortrfv)
           ")",
           "&vegreferanse=Ev,Rv"

    )

  respons <-
    GET(api_query_fart_motorveg,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  fartsgrense <- rbindlist(uthenta$objekter$egenskaper,
                             idcol = "id_fartsgrense") %>%
    filter(navn == "Fartsgrense")

  lenkelengde <- as.data.frame(uthenta$objekter$lokasjon.strekningslengde)

  motorveg_tabell <- bind_cols(select(fartsgrense, verdi),
                               lenkelengde)

  colnames(motorveg_tabell) <- c("fartsgrense",
                                 "lenkelengde"
  )

  return(motorveg_tabell)
}

fart_motorveg <- hent_fart_motorveg("7355") %>%
  mutate(motorveg = "Motorveg")

fart_motortrafikkveg <- hent_fart_motorveg("7356") %>%
  mutate(motorveg = "Motortrafikkveg")

fart_motorveg_summert <- rbind(fart_motorveg, fart_motortrafikkveg) %>%
  mutate(fartsgrense = as.integer(fartsgrense)) %>%
  group_by(fartsgrense, motorveg) %>%
  summarise(veglengde = sum(lenkelengde)/1000) %>%
  arrange(motorveg, fartsgrense)

sum(fart_motorveg_summert$veglengde)

#
# Slutt. ####
#