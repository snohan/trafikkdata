# Read CSVs exported from Kibana
# Write to Excel

library(tidyverse)
library(lubridate)
library(openxlsx)
library(ghql)

setwd("H:/Programmering/R/trafikkdata/kibanadata")

my_files <- list.files(path = ".", pattern = "*csv$")

file_1 <- read.csv(my_files[14], sep = ";") %>%
  tidyr::separate(from, into = c("date", "hour"), sep = ",") %>%
  dplyr::mutate(date = with_tz(mdy(date)),
                valid_events = as.numeric(gsub(",", "", valid_events)),
                valid_speed = as.numeric(gsub(",", "", valid_speed)),
                valid_length = as.numeric(gsub(",", "", valid_length))
                )

write.xlsx(file_1, file = "Stringvei_Nydalsbrua.xlsx")


# Rv3 ####
rv3_valid_speed <- read_delim("rv3data/rv3.csv", delim = ";") %>%
  tidyr::separate(from, into = c("date", "hour"), sep = ",") %>%
  dplyr::mutate(date = with_tz(mdy(date)),
                aar = year(date),
                maaned = month(date),
                lengde = case_when(
                  length_range == "[..,5.6)" ~ "Under 5,6 m",
                  length_range == "[5.6,..)" ~ "Minst 5,6 m"
                ),
                antall_fart = valid_speed * average_speed) %>%
  dplyr::select(-hour, -length_range) %>%
  dplyr::rename(dag = date,
                punkt = traffic_registration_point_id,
                antall_godkjent_fart = valid_speed,
                fart = average_speed)

trp <- getPoints()

rv3_maaned <- rv3_valid_speed %>%
  dplyr::group_by(punkt, aar, maaned, lengde) %>%
  dplyr::summarise(fart = round(sum(antall_fart, na.rm = T) /
                                  sum(antall_godkjent_fart, na.rm = T),
                                digits = 1),
                   trafikkmengde_godkjent_fart = sum(antall_godkjent_fart)) %>%
  dplyr::left_join(trp, by = c("punkt" = "trp_id")) %>%
  dplyr::ungroup() %>%
  dplyr::select(punkt = name,
                vegreferanse = road_reference,
                aar, maaned, lengde, trafikkmengde_godkjent_fart, fart)

write.xlsx(rv3_maaned, file = "rv3_fart.xlsx")

rv3_total <- read_delim("rv3data/rv3_total.csv", delim = ";") %>%
  tidyr::separate(from, into = c("date", "hour"), sep = ",") %>%
  dplyr::mutate(date = with_tz(mdy(date)),
                aar = year(date),
                maaned = month(date)) %>%
  dplyr::select(-hour, -length_range) %>%
  dplyr::rename(dag = date,
                punkt = traffic_registration_point_id,
                trafikkmengde = valid_events) %>%
  dplyr::group_by(punkt, aar, maaned) %>%
  dplyr::summarise(trafikkmengde = sum(trafikkmengde, na.rm = T))
# scrap, do not need this anyway!
