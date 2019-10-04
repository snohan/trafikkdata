# Get all manual labels from trp-api and write to csv

library(tidyverse)
library(httr)
library(ghql)
library(devtools)
devtools::install_github("tidyverse/tidyr")


manual_labels <- get_manual_labels() %>%
#  dplyr::select(-labelLanes) %>%
  as_tibble()

test <- unlist(manual_labels$labelLanes)

test1 <- manual_labels$labelLanes[1]
str(test1)
test2 <- test1[1]

test3 <- unlist(points_trp$labelLanes, recursive = F)

test4 <- points_trp %>%
  mutate(states = map_chr(labelLanes, "states"))

write.csv2(manual_labels, file = "manuelle_markeringer.csv",
           row.names = F)
