# From data exported through Kibana, rank points with high level of
# valid_speed == FALSE and large portion of speeds under 15 km/h.

# Packages ####
library(tidyverse)
library(jsonlite)
library(plotly)

# Read in files exported from Kibana ####
# Latest export: 20190324-20190328 (monday-thursday)
# Exported as raw csv.
valid_speed <- readr::read_csv("valid_speed_per_point.csv") %>%
  dplyr::mutate(valid_speed = case_when(valid_speed == 0 ~ FALSE,
                                 valid_speed == 1 ~ TRUE)) %>%
  tidyr::spread(valid_speed, Count, drop = FALSE, sep = "_") %>%
  dplyr::mutate(valid_speed_ratio =
                  round(100 * valid_speed_FALSE /
                          (valid_speed_FALSE + valid_speed_TRUE),
                        digits = 0)) %>%
  dplyr::select(trp, valid_speed_ratio)

low_speed <- readr::read_csv("low_speed_per_point.csv") %>%
  dplyr::rename(speed = speed_range) %>%
  dplyr::mutate(speed = case_when(
    speed == "from:0,to:15" ~ "under_15",
    speed == "from:15,to:300" ~ "over_15")) %>%
  tidyr::spread(speed, Count, drop = FALSE, sep = "_") %>%
  dplyr::mutate(low_speed_ratio = round(100 * speed_under_15 /
                                          (speed_under_15 + speed_over_15),
                                        digits = 0)) %>%
  dplyr::select(trp, low_speed_ratio)

# Mapping trp to trs from trp-api
trs_trp <- jsonlite::fromJSON("trp_trs.json",
                              simplifyDataFrame = T, flatten = T) %>%
  as.data.frame() %>%
  dplyr::rename(trp = id,
                county = location.currentRoadReference.county,
                category = location.currentRoadReference.category,
                status = location.currentRoadReference.status,
                number = location.currentRoadReference.number,
                hp = location.currentRoadReference.hp,
                meter = location.currentRoadReference.meter)

head(trs_trp)

queue_points <- valid_speed %>%
  dplyr::left_join(low_speed) %>%
  dplyr::left_join(trs_trp) %>%
  # removing ramps
  dplyr::filter(hp < 70)

head(queue_points)

queue_points %>%
  ggplot2::ggplot(aes(valid_speed_ratio, low_speed_ratio)) +
  geom_jitter() +
  geom_hline(yintercept = 10) +
  geom_vline(xintercept = 50)

queue_points_for_exclusion <- queue_points %>%
  #dplyr::filter(low_speed_ratio > 10) %>%
  dplyr::filter(valid_speed_ratio > 50 | low_speed_ratio > 10)

write.csv2(queue_points_for_exclusion, file = "punkter_med_kø.csv",
           row.names = F)
#