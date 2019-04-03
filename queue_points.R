# From data exported through Kibana, rank points with high level of
# valid_speed == FALSE and large portion of speeds under 15 km/h.

# Packages ####
library(tidyverse)
library(jsonlite)

# Read in files exported from Kibana ####
# Latest export: 20190324-20190328 (monday-thursday)
# Exported as raw csv.
valid_speed <- readr::read_csv("valid_speed_per_point.csv")
low_speed <- readr::read_csv("low_speed_per_point.csv")

# TODO: mapping trp to trs from trp-api
trs_trp <- jsonlite::fromJSON("trp_trs.json",
                              simplifyDataFrame = T, flatten = T) %>%
  as.data.frame()
