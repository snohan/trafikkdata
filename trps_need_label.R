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
  dplyr::distinct(trp_id, .keep_all = T) #%>%
  # dplyr::mutate(
  #   name = stringr::str_to_title(name, locale = "no")
  # )


# Read CSVs from Kibana ----
read_a_file <- function(file_name) {

  # readr::read_csv2(
  #   paste0("zero_dt/", file_name)
  # ) #%>%
  # dplyr::mutate(
  #   periode_start = as.character(periode_start)
  # )

  readr::read_csv(
    file = paste0("zero_dt/", file_name),
    #delim = ","
  )

}

zero_dt <-
  list.files(
    "zero_dt"
  ) %>%
  purrr::map_df(
    ~ read_a_file(.)
  ) %>%
  dplyr::select(
    -number_of_days
  ) %>%
  # Remove invisible trps
  dplyr::filter(
    trp_id %in% distinct_trps$trp_id
  ) %>%
  dplyr::mutate(
    #day = lubridate::dmy(day)
    lane = lane / 100 # 1,00 tolkes visst til 100!
  )

present_trp_ids <-
  zero_dt %>%
  dplyr::select(
    trp_id
  ) %>%
  dplyr::distinct()

labels <-
  get_labels_for_trp_list(
    present_trp_ids$trp_id
  ) %>%
  dplyr::select(
    trp_id,
    lane,
    date_interval
  )

labelled_zero_days <-
  zero_dt %>%
  dplyr::inner_join(
    labels,
    by = c("trp_id", "lane")
  ) %>%
  dplyr::mutate(
    is_day_labelled = day %within% date_interval
  ) %>%
  dplyr::filter(
    is_day_labelled == TRUE
  ) %>%
  dplyr::select(
   trp_id,
   lane,
   day
  )

# Real zero days as informed by owners
real_zero_days <-
  readr::read_csv2("real_zero_days.csv") %>%
  dplyr::mutate(
    day = lubridate::dmy(day)
  )

# TODO: add filter for lonely days in winter for bike

zero_dt_filtered <-
  zero_dt %>%
  dplyr::anti_join(
    labelled_zero_days,
    by = c("trp_id", "lane", "day")
  ) %>%
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
    !(trp_id =="24316B805152" & month_number %in% c(11, 12, 1, 2, 3))
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
    streak_length = n(),
    .by = c(trp_id, lane, streak_group)
  ) |>
  dplyr::filter(
    !(traffic_type == "BICYCLE" & month_number %in% c(1, 2, 3, 4, 10, 11, 12) & streak_length <= 3)
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
  )

writexl::write_xlsx(
  trp_need_label,
  path = "nulltrafikk.xlsx"
)


trp_need_label |>
  dplyr::mutate(
    month = lubridate::floor_date(day, "month")
  ) |>
  ggplot(aes(x = month)) +
  geom_histogram()

trp_top_list <-
  trp_need_label |>
  dplyr::summarise(
    count = n(),
    .by = c(name, traffic_type, municipality_name)
  ) |>
  dplyr::arrange(
    desc(count)
  )

# TODO: add instances of negative speeed