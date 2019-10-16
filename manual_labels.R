# Get all manual labels from trp-api and write to csv

library(tidyverse)
library(httr)
library(ghql)
library(lubridate)

manual_labels <- get_manual_labels()

write.csv2(manual_labels, file = "manuelle_merkinger.csv",
           row.names = F)
