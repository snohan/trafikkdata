# List of TRPs in need of labels

{
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  library(writexl)

}

# TRP stats ----
trp <- get_points()

distinct_trps <-
  trp |>
  split_road_system_reference() |>
  dplyr::select(
    county_geono,
    county_name,
    municipality_name,
    road_category,
    trp_id,
    name,
    road_reference,
    traffic_type,
    registration_frequency
  ) |>
  dplyr::distinct(trp_id, .keep_all = T)


# Zero dt ----
read_a_file <- function(file_name) {

  readr::read_csv(paste0("zero_dt/zero_days/", file_name))

}

zero_dt <-
  base::list.files("zero_dt/zero_days") |>
  purrr::map_df(
    ~ read_a_file(.)
  ) |>
  dplyr::select(-number_of_days) |>
  # Remove invisible trps
  dplyr::filter(trp_id %in% distinct_trps$trp_id) |>
  dplyr::mutate(
    #day = lubridate::dmy(day)
    lane = lane / 100 # 1,00 tolkes visst til 100!
  )

present_trp_ids <-
  zero_dt |>
  dplyr::select(trp_id) |>
  dplyr::distinct()

labels <-
  get_labels_for_trp_list(present_trp_ids$trp_id) |>
  dplyr::select(
    trp_id,
    lane,
    label_start,
    label_end,
    date_interval
  )

labelled_zero_days_lane <-
  zero_dt |>
  dplyr::inner_join(
    labels,
    by = dplyr::join_by(trp_id, lane, dplyr::between(day, label_start, label_end))
  ) |>
  dplyr::select(
   trp_id,
   lane,
   day
  )

labels_all_lanes <-
  labels |>
  dplyr::filter(is.na(lane)) |>
  dplyr::select(-lane)

labelled_zero_days_all_lanes <-
  zero_dt |>
  dplyr::inner_join(
    labels_all_lanes,
    by = dplyr::join_by(trp_id, dplyr::between(day, label_start, label_end))
  ) |>
  dplyr::select(
    trp_id,
    lane,
    day
  )

labelled_zero_days <-
  dplyr::bind_rows(
    labelled_zero_days_lane,
    labelled_zero_days_all_lanes
  ) |>
  dplyr::distinct()


# Real zero days as informed by owners
real_zero_days <-
  readr::read_csv2("zero_dt/real_zero_days.csv") %>%
  dplyr::mutate(
    day = lubridate::dmy(day)
  )

# TODO: add filter for lonely days in winter for bike

zero_dt_filtered <-
  zero_dt |>
  dplyr::anti_join(
    labelled_zero_days,
    by = c("trp_id", "lane", "day")
  ) |>
  dplyr::anti_join(
    real_zero_days,
    by = c("trp_id", "day")
  ) |>
  dplyr::mutate(
    month_number = lubridate::month(day)
  ) |>
  dplyr::filter(
    # Innherredsvegen ved Fjæregata
    !(trp_id == "04300V72813" & lane == 2 & day > "2017-07-01"),
    !(trp_id == "04300V72813" & lane == 4 & day > "2017-07-01"),
    # Gamle Nygårdsbru foratu og bilveg
    !(trp_id %in% c("17729B2483952", "17981B2483952")),
    # Langvatnet
    !(trp_id =="19588V1060150" & month_number %in% c(11, 12, 1, 2, 3, 4)),
    # Videseter
    !(trp_id =="90465V384130" & month_number %in% c(10, 11, 12, 1, 2, 3, 4, 5)),
    # Aurlandsfjellet
    !(trp_id =="12252V384272" & month_number %in% c(10, 11, 12, 1, 2, 3, 4, 5)),
    # Hogga sluser
    !(trp_id =="86134B493596" & month_number %in% c(10, 11, 12, 1, 2, 3)),
    # Rinnelva sykkel
    !(trp_id =="97043B1797561" & month_number %in% c(11, 12, 1, 2, 3)),
    # Løsberga sykkel
    !(trp_id =="28518B1828478" & month_number %in% c(10, 11, 12, 1, 2, 3)),
    # Værnestunnelen sykkel
    !(trp_id =="85036B1824272" & month_number %in% c(10, 11, 12, 1, 2, 3)),
    # Pirbrua sykkel sør
    !(trp_id =="50284B2426199" & month_number %in% c(11, 12, 1, 2, 3)),
    # Stavne sykkel
    !(trp_id =="82732B2353931" & month_number %in% c(11, 12, 1, 2, 3)),
    # Svingbrua sykkel
    !(trp_id =="03908B2426198" & month_number %in% c(11, 12, 1, 2, 3)),
    # Gamle Okstadbakken - Sykkel
    !(trp_id =="10272B1698646" & month_number %in% c(11, 12, 1, 2, 3)),
    # Mortavika ferjekai, felt 4
    !(trp_id == "74885V319528" & lane == 4 & day > "2018-02-01"),
    # Arna sykkel gs
    !(trp_id == "83708B802716"),
    # Sandviken sykkel R1
    !(trp_id =="23565B805152" & month_number %in% c(11, 12, 1, 2, 3)),
    # Sandviken sykkel R2
    !(trp_id =="24316B805152" & month_number %in% c(11, 12, 1, 2, 3)),
    # Liljevatnet sykkel
    !(trp_id =="07530B1807128"),
    # Folkestad
    !(trp_id =="33345B2562216" & month_number %in% c(11, 12, 1, 2, 3)),
    # Løken
    !(trp_id =="33211B2483702" & month_number %in% c(11, 12, 1, 2, 3)),
    # Bruusgaardsvei
    !(trp_id =="73890B1953494" & month_number %in% c(12, 1, 2)),
    # Elvadalen nord
    !(trp_id =="53385B249032"),
    # Gullbringvegen
    !(trp_id =="84491B2258803" & month_number %in% c(12, 1, 2)),
    # Rotevatn
    !(trp_id =="41209B1819765" & month_number %in% c(12, 1, 2)),
  )

n_before_2022 <-
  zero_dt_filtered |>
  dplyr::filter(
    day < "2022-01-01"
  )

# 2022-10-03: 49 843
# 2022-10-17: 51 420
# 2022-10-31: 48 368
# 2023-02-08: 42 887
# 2023-03-09: 48 780
# 2023-03-22: 38 901
# 2023-04-25: 33 847
# 2023-05-29: 38 412
# 2023-06-22: 38 142
# 2023-07-27: 38 125
# 2023-09-11: 38 537
# 2023-10-16: 36 528
# 2023-11-01: 35 325
# 2024-01-01: 34 676
# 2024-03-08: 34 543
# 2024-04-29: 33 057
# 2024-06-04: 33 051
# 2024-08-08: 33 400
# 2024-09-04: 27 952 (update: labels covering all lanes)
# 2024-09-16: 27 796
# 2024-10-03: 27 536
# 2024-10-23: 27 294
# 2024-11-01: 27 297
# 2024-11-11: 26 415
# 2024-11-25: 27 265
# 2024-12-09: 27 263
# 2025-01-02: 24 368 (I took some bike TRPs)
# 2025-01-31: 23 906
# 2025-03-10: 23 906
# 2025-04-08: 22 286
# 2025-05-02: 22 286
# 2025-06-02: 20 195 (I took some)
# 2025-09-01: 19 752
# 2025-10-01: 19 752
# 2025-11-03: 19 812
# 2025-12-01: 19 203

trp_need_label <-
  zero_dt_filtered |>
  dplyr::left_join(
    distinct_trps,
    by = "trp_id"
  ) |>
  dplyr::filter(
    !is.na(name)
  ) |>
  dplyr::select(
    county_geono,
    county_name,
    municipality_name,
    road_category,
    trp_id,
    name,
    traffic_type,
    road_reference,
    lane,
    day,
    month_number
  ) |>
  dplyr::arrange(
    trp_id,
    lane,
    day
  ) |>
  dplyr::mutate(
    streak_group = base::cumsum(c(TRUE, base::diff(day) != 1))
  ) |>
  dplyr::mutate(
    days_in_a_row = n(),
    .by = c(trp_id, lane, streak_group)
  ) |>
  dplyr::filter(
    !(traffic_type == "BICYCLE" & month_number %in% c(1, 2, 3, 4, 10, 11, 12) & days_in_a_row <= 3)
  ) |>
  dplyr::arrange(
    county_name,
    municipality_name,
    road_category,
    name
  ) |>
  dplyr::select(
    -county_geono,
    -month_number,
    -streak_group
  ) |>
  dplyr::mutate(
    comment = NA
  )

trp_need_label |>
  dplyr::mutate(
    month = lubridate::floor_date(day, "month")
  ) |>
  ggplot(aes(x = month)) +
  geom_histogram()

trp_top_list <-
  trp_need_label |>
  dplyr::filter(
    day >= "2025-01-01"
  ) |>
  dplyr::summarise(
    count = n(),
    .by = c(trp_id, name, traffic_type, road_category, county_name, municipality_name)
  ) |>
  dplyr::arrange(
    desc(count)
  )

# Find the dates of a specified TRP
trp_need_label |>
  dplyr::filter(
    trp_id == "05899V1109722"
  ) |>
  ggplot(aes(x = day)) +
  geom_bar() +
  facet_wrap(~lane, ncol = 1)


# Negative speed ----
# Get data grom "negative_speed" in Kibana raw, use last seven days
negative_speed <-
  readr::read_csv2("zero_dt/negative_speed.csv") |>
  dplyr::mutate(
    speed =
      dplyr::case_when(
        speed == "≥ -10000 and < 0" ~ "neg",
        speed == "≥ 0 and < 2000" ~ "pos"
      )
  ) |>
  tidyr::pivot_wider(
    names_from = speed,
    names_prefix = "speed_",
    values_from = volume
  ) |>
  dplyr::mutate(
    total_volume = speed_neg + speed_pos,
    percentage_negative_speed = round(speed_neg / (total_volume) * 100, 0),
    trs_id = as.character(trs_id)
  ) |>
  dplyr::filter(
    total_volume > 700,
    # When there are just a few events, it is probably just real overtakes, so filter them.
    percentage_negative_speed >= 15,
    !(trs_id %in% c("1100308")) # Eide, smal veg
  ) |>
  dplyr::arrange(
    desc(percentage_negative_speed)
  )

# Need to add some metainfo on stations from TRP-API
# Need not fetch new data every time

trs <- readr::read_rds("trp_info/trs.rds")

negative_speed_trs <-
  negative_speed |>
  dplyr::left_join(
    trs,
    by = dplyr::join_by(trs_id)
  ) |>
  dplyr::select(
    county_name,
    municipality_name,
    trs_id,
    name,
    road_category,
    road_reference,
    traffic_type,
    registration_frequency,
    sensor_port,
    total_volume,
    speed_neg,
    speed_pos,
    percentage_negative_speed
  ) |>
  dplyr::arrange(
    county_name,
    municipality_name,
    trs_id,
    sensor_port
  )


# Write ----
list(
  nulltrafikk = trp_need_label,
  negativ_fart = negative_speed_trs
) |>
writexl::write_xlsx(
  path = "zero_dt/nulltrafikk.xlsx"
)
