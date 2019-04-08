#
# Hent fart på en trafikkregstasjon.
#

# Pakker ####
library(httr)
library(stringr)
library(dplyr)
library(jsonlite)
library(magrittr)

vegr <- "RV23HP1m3262"

# Spørring ####
hent_fart_punkt <- function(vegr) {

  api_query <-
    paste0(nvdb_url,
           sti_vegobjekter,
           "/105?vegreferanse=",
           vegr,
           "&segmentering=FALSE"
    )

  respons <-
    GET(api_query,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta <- fromJSON(str_conv(respons$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  respons2 <-
    GET(uthenta$objekter$href,
        add_headers("X-Client" = "trafikkdatagruppa",
                    "X-Kontaktperson" = "snorre.hansen@vegvesen.no",
                    "Accept" = "application/vnd.vegvesen.nvdb-v2+json"))

  uthenta2 <- fromJSON(str_conv(respons2$content, encoding = "UTF-8"),
                      simplifyDataFrame = T,
                      flatten = T)

  vegref <- bind_rows(uthenta$objekter$lokasjon.vegreferanser,
                      .id = "vegreferanse")

  verdier <- uthenta2$egenskaper$verdi

  return(verdier)
}

fartslenkeverdier <- hent_fart_punkt(fartsindekspunkter$vegref[53])
fartslenkeverdier2 <- hent_fart_punkt(fartsindekspunkter$vegref[2])

fartslenkeverdier[3]

# Leser inn punktfila ####
fartsindekspunkter <- read.csv2("fartsindekspunkter_riksveg.csv")

for (i in 1:nrow(fartsindekspunkter)) {

}

#
# End.
#