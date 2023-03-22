# List of TRPs in need of labels

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

library(writexl)

# TRP stats ----
trp <- get_points()

distinct_trps <-
  trp %>%
  split_road_system_reference() %>%
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
  ) %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no")
  )


# Read CSVs from Kibana ----
read_a_file <- function(file_name) {

  readr::read_csv2(
    paste0("zero_dt/", file_name)
  ) #%>%
  # dplyr::mutate(
  #   periode_start = as.character(periode_start)
  # )

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
  )
# %>%
#   dplyr::mutate(
#     day = lubridate::dmy(day)
#   )

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
    # Videseter
    !(trp_id =="90465V384130" & month_number %in% c(10, 11, 12, 1, 2, 3, 4, 5)),
    # Aurlandsfjellet
    !(trp_id =="12252V384272" & month_number %in% c(10, 11, 12, 1, 2, 3, 4, 5)),
    # Hogga sluser
    !(trp_id =="86134B493596" & month_number %in% c(10, 11, 12, 1, 2, 3))
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

trp_need_label <-
  zero_dt_filtered %>%
  dplyr::left_join(
    distinct_trps,
    by = "trp_id"
  ) %>%
  dplyr::filter(
    !is.na(name)
  ) %>%
  dplyr::select(
    county_geono,
    county_name,
    municipality_name,
    road_category,
    name,
    traffic_type,
    registration_frequency,
    road_reference,
    lane,
    day
  ) %>%
  dplyr::arrange(
    county_geono,
    municipality_name,
    road_category,
    name,
    day
  ) %>%
  dplyr::select(
    -county_geono
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
  dplyr::group_by(
    name
  ) |>
  dplyr::summarise(
    count = n()
  ) |>
  dplyr::arrange(
    desc(count)
  )
