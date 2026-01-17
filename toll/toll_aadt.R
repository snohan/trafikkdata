# Intro ----
# Calculate AADT for toll stations based on data from Autopass Analytics
# - daily aggregates (for coverage, aadt, se)
# - yearly aggregates (for heavy percentage)
# Only yearly data is feasible to fetch with both class and direction (export limit from Power BI). This will be used to calculate heavy percentage.

# ASSUMPTION:
# All stations that charges in only one direction, register traffic in only that one direction, no matter the lane.
# Some stations have data in more than two directions, e.g. some adjacent bicycle road, and labelling these as a "direction" is of course a misuse of the term.
# Let's call these "surplus" directions.
# Anyways, we can summarise over all lanes per unidirectional station and still have only the traffic in that one direction.

# We are then left with the problem of determining which direction is with and against metering for both-directions-stations. 
# This includes the surplus directions.
# But if traffic is distributed almost 50 % on each direction, we can just add up and divide by 2.

# Need to split the data handling by stations according to whether they are both-directional or not.


# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
  library(jsonlite)
  library(lubridate)
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  library(writexl)
}


# Preliminaries ----
## Toll station data ----
# NVDB
toll_stations <- 
  get_all_tolling_stations() |>
  split_road_system_reference() |>
  dplyr::select(
    nvdb_id,
    operator_id,
    toll_station_id,
    toll_station_name,
    directions,
    road_category,
    road_reference,
    road_link_position
  ) |>
  dplyr::mutate(
    # Remove leading zeros
    nvdb_id = as.numeric(nvdb_id) |> as.character()
  ) |> 
  dplyr::arrange(
    operator_id,
    as.numeric(toll_station_id)
  ) |> 
  dplyr::select(-operator_id, -toll_station_id)

# Autopass Analytics, in case there is useful additional information.
# Lane given here is absolutely not in correspondence with the numbering scheme of NVDB - do not use!
# The description is almost the same as toll_station_name from NVDB. The latter seems more curated.
# The direction can reveal that stations labelled "Begge retninger" in strict meaning registers in only one direction 
# in cases where there is only one direction, e.g. twin-tube tunnels! 
# Supposedly the label comes from thinking in terms of a collection of stations that in sum covers both directions. Again, an unclear use of terms.

# toll_stations_autopass <-
#   readr::read_csv("toll/toll_stations_from_autopass_analytics.csv") |> 
#   dplyr::select(
#     nvdb_id = 'NVDB ID',
#     # toll_station_code = 'toll station code',
#     # name = 'toll station name', # Same as description, but some are cropped
#     # lane = 'toll station lane',
#     # description = 'toll station description',
#     direction = 'toll station direction'
#   ) |> 
#   dplyr::mutate(
#     nvdb_id = as.numeric(nvdb_id) |> as.character()
#     # same_as_name = name == description
#   ) |> 
#   dplyr::distinct()


## Yearly data ----
yearly <-
  readr::read_csv(
    "toll/toll_station_yearly_2025.csv"
  ) |>
  dplyr::select(
    nvdb_id = 'NVDB ID',
    direction_text = 'toll station direction',
    class = 'vehicle class ID',
    traffic = 'Accepted passages'
  ) |>
  dplyr::filter(
    class %in% c(1, 2),
    !(nvdb_id == 0)
  ) |>
  dplyr::mutate(
    class = dplyr::case_when(
      class == 1 ~ "light",
      class == 2 ~ "heavy"
    ),
    # Get rid of leading zeros
    nvdb_id = as.numeric(nvdb_id) |> as.character()
  ) |> 
  dplyr::filter(
    # Some stations are not in NVDB
    nvdb_id %in% toll_stations$nvdb_id
  )


# Toll stations tidied ----
toll_stations_with_data <- 
  toll_stations |> 
  dplyr::filter(
    nvdb_id %in% yearly$nvdb_id
  ) |> 
  dplyr::mutate(
    # Adding missing direction descriptions, derived from data and by looking at Vegkart.
    directions = dplyr::case_when(
      nvdb_id %in% c("1025985157", "1025915958", "1016036098", "1016292474") ~ "Begge retninger",
      nvdb_id %in% c("1026090508", "1026090226", "1026076253") ~ "Med metrering",
      nvdb_id %in% c("1026076748") ~ "Mot metrering",
      TRUE ~ directions
    )
  )

# Split in uni- and both-directional
toll_stations_one_direction <-
  toll_stations_with_data |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(
    nvdb_id %in% yearly$nvdb_id,
    stringr::str_detect(directions, "Begge", negate = TRUE)
  ) |> 
  dplyr::select(
    nvdb_id, directions
  )

toll_stations_both_directions <-
  toll_stations_with_data |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(
    nvdb_id %in% yearly$nvdb_id,
    stringr::str_detect(directions, "Begge", negate = FALSE)
  ) |> 
  dplyr::select(
    nvdb_id, directions
  )

# Need to infer correct direction for the directions on both-directional stations
# Might help to use description found in Autopass Analytics
# toll_stations_and_lanes <-
#   toll_stations_with_data |> 
#   dplyr::left_join(
#     toll_stations_autopass,
#     by = 'nvdb_id'
#   ) |> 
#   dplyr::select(
#     nvdb_id, toll_station_name, 
#     directions, direction,
#     road_category, road_reference, road_link_position
#   ) |> 
#   # This is only interesting for both-direction-stations
#   dplyr::filter(
#     stringr::str_detect(directions, "Begge", negate = FALSE)
#   )


# Heavy percentage ----
# Using only yearly data here
yearly_unidirectional_stations <-
  yearly |> 
  dplyr::filter(
    nvdb_id %in% toll_stations_one_direction$nvdb_id
  ) |>
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, class)
  ) |>
  tidyr::pivot_wider(
    names_from = class,
    names_prefix = "traffic_",
    values_from = traffic
  ) |>
  dplyr::mutate(
    heavy_percentage = (100 * traffic_heavy / (traffic_light + traffic_heavy)) |> round(2)
  ) |> 
  dplyr::left_join(
    toll_stations_one_direction,
    by = 'nvdb_id'
  )

length(unique(yearly_unidirectional_stations$nvdb_id)) == nrow(toll_stations_one_direction) # must be TRUE

# Stations covering both directions need to be looked at as there are some problems.
# Manually set direction in the cases where:
# - there is only one direction anyway (direction ratio = 1), 15 such in 2025.
# - stations with more than two directions, 3 stations in 2025.
# - directionial distribution outside 60-40.

yearly_both_directional_stations_distribution <-
  yearly |> 
  dplyr::filter(
    !(nvdb_id %in% toll_stations_one_direction$nvdb_id),
    # Can safely remove "directions" with few vehicles.
    # These are targeting pedestrian and bicycle lanes etc.
    traffic > 5e3
  ) |> 
  # Sum over class
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, direction_text)
  ) |> 
  # Look at direction distribution
  dplyr::mutate(
    sum_traffic = sum(traffic),
    n_directions = n(),
    .by = c(nvdb_id)
  ) |> 
  dplyr::mutate(
    ratio = traffic / sum_traffic
  )

length(unique(yearly_both_directional_stations_distribution$nvdb_id)) == 
  (nrow(toll_stations_with_data) - nrow(toll_stations_one_direction)) # must be TRUE

# Fix direction labels
with_chr <- "Med metrering"
against_chr <- "Mot metrering"

# 1017139066 has only one direction-value which includes traffic in both directions, can safely be divided by 2.
# Same goes for 929790729 and 1004719849

# Stations originally labelled as both directions, but which turns out to have only one.
toll_stations_one_direction_2 <-
  tibble::tribble(
    ~nvdb_id, ~directions,
     "920567308", with_chr,
    "1016429812", with_chr,
    "1016429813", with_chr,
    "1017139076", with_chr,
    "1017166301", with_chr,
    "1013292378", with_chr,
    "1016036108", with_chr,
    "1016036095", with_chr,
    "1016036106", with_chr,
     "929790731", with_chr,
    "1016036103", with_chr,
     "929790724", with_chr,
    # Stations with data per lane
    "1013292348", with_chr,
    "1016036968", with_chr,
    "1013292386", with_chr,
    "1013292348", with_chr,
    "1013292344", with_chr,
    "1013292326", with_chr,
    "1013292333", with_chr
  )

toll_stations_triple_directions <-
  tibble::tribble(
    ~nvdb_id, ~direction_text, ~directions,
    "929790730", "From city centre", with_chr,
    "929790730", "To city centre", against_chr,
    "929790730", NA_character_, against_chr,
    "929790735", "To city centre", against_chr,
    "929790735", NA_character_, against_chr,
    "929790735", "From city centre", with_chr,
    "929790746", "From city centre", with_chr,
    "929790746", NA_character_, with_chr,
    "929790746", "To city centre", against_chr
  )

# Stations with data per lane all in same direction
toll_stations_data_per_lane <-
  toll_stations_with_data |> 
  sf::st_drop_geometry() |> 
  dplyr::filter(
    nvdb_id %in% toll_stations_both_directions$nvdb_id,
    !(nvdb_id %in% toll_stations_one_direction_2$nvdb_id),
    !(nvdb_id %in% toll_stations_triple_directions$nvdb_id)
  ) |> 
  # Fetch lane info from NVDB
  dplyr::mutate(
    lanes = purrr::map_chr(road_link_position, hent_feltnummer)
  )
# The ones with only odd or only even lane numbers are actually unidirectional

# Imbalanced
yearly_both_directional_stations_distribution_2 <-
  yearly_both_directional_stations_distribution |> 
  dplyr::filter(
    !(nvdb_id %in% toll_stations_one_direction_2$nvdb_id),
    !(nvdb_id %in% toll_stations_triple_directions$nvdb_id),
    ratio != 1,
    ratio < 0.4| ratio > 0.6
  ) |> 
  dplyr::arrange(nvdb_id)

toll_stations_imbalanced_directions_manually_set <-
  tibble::tribble(
    ~nvdb_id, ~direction_text, ~directions,
    "487458622", "From city centre", with_chr,
    "487458622", "To city centre", against_chr,
    "929790704", "From city centre", with_chr,
    "929790704", "To city centre", against_chr,
    "929790705", "From city centre", with_chr,
    "929790705", "To city centre", against_chr,
    "929790708", "From city centre", against_chr,
    "929790708", "To city centre", with_chr,
    "929790716", "From city centre", with_chr,
    "929790716", "To city centre", against_chr,
    "929790749", "From city centre", with_chr,
    "929790749", "To city centre", against_chr,
    "929790754", "From city centre", with_chr,
    "929790754", "To city centre", against_chr
  )

# Take two on calculating heavy percentage
yearly_unidirectional_stations_2 <-
  yearly |> 
  dplyr::filter(
    nvdb_id %in% toll_stations_one_direction_2$nvdb_id
  ) |>
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, class)
  ) |>
  tidyr::pivot_wider(
    names_from = class,
    names_prefix = "traffic_",
    values_from = traffic
  ) |>
  dplyr::mutate(
    heavy_percentage = (100 * traffic_heavy / (traffic_light + traffic_heavy)) |> round(2)
  ) |> 
  dplyr::left_join(
    toll_stations_one_direction_2,
    by = 'nvdb_id'
  )

yearly_tridirectional_stations <-
  yearly |> 
  dplyr::inner_join(
    toll_stations_triple_directions,
    by = dplyr::join_by('nvdb_id', 'direction_text')
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, directions, class)
  ) |>
  tidyr::pivot_wider(
    names_from = class,
    names_prefix = "traffic_",
    values_from = traffic
  ) |>
  dplyr::mutate(
    heavy_percentage = (100 * traffic_heavy / (traffic_light + traffic_heavy)) |> round(2)
  )

yearly_imbalanced_stations <-
  yearly |> 
  dplyr::inner_join(
    toll_stations_imbalanced_directions_manually_set,
    by = dplyr::join_by('nvdb_id', 'direction_text')
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, directions, class)
  ) |>
  tidyr::pivot_wider(
    names_from = class,
    names_prefix = "traffic_",
    values_from = traffic
  ) |>
  dplyr::mutate(
    heavy_percentage = (100 * traffic_heavy / (traffic_light + traffic_heavy)) |> round(2)
  )

# Stations still with "Begge retninger" will be summed and divided equally between the directions.
yearly_both_directional_stations <-
  yearly |> 
  dplyr::filter(
    nvdb_id %in% toll_stations_both_directions$nvdb_id,
    !(nvdb_id %in% yearly_unidirectional_stations_2$nvdb_id),
    !(nvdb_id %in% toll_stations_triple_directions$nvdb_id),
    !(nvdb_id %in% toll_stations_imbalanced_directions_manually_set$nvdb_id)
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic) / 2,
    .by = c(nvdb_id, class)
  ) |>
  tidyr::pivot_wider(
    names_from = class,
    names_prefix = "traffic_",
    values_from = traffic
  ) |>
  dplyr::mutate(
    heavy_percentage = (100 * traffic_heavy / (traffic_light + traffic_heavy)) |> round(2)
  )

yearly_final <-
  dplyr::bind_rows(
    yearly_unidirectional_stations,
    yearly_unidirectional_stations_2,
    yearly_tridirectional_stations,
    yearly_imbalanced_stations,
    yearly_both_directional_stations |> dplyr::mutate(directions = with_chr),
    yearly_both_directional_stations |> dplyr::mutate(directions = against_chr)
  ) |> 
  dplyr::select(nvdb_id, directions, heavy_percentage)

length(unique(yearly_final$nvdb_id)) == nrow(toll_stations_with_data) # Must be TRUE


# Coverage ----
# Using daily aggregates to calculate coverage and look for anomalies
daily <-
  base::list.files(
    path = "toll",
    pattern = "dailty_2025",
    full.names = TRUE
  ) |>
  purrr::map_df(
    ~ readr::read_csv(.)
  ) |>
  dplyr::select(
    nvdb_id = 'NVDB ID',
    direction_text = 'toll station direction',
    month = 'month no.',
    day = 'day no. of year',
    traffic = 'Accepted passages'
  ) |> 
  dplyr::mutate(
    # Get rid of leading zeros
    nvdb_id = as.numeric(nvdb_id) |> as.character()
  ) |>
  dplyr::filter(
    nvdb_id %in% toll_stations_with_data$nvdb_id
  )

# Need to do the same splitting here as with yearly data
# First find all daily data per actual direction
# Then look for anomalies
daily_unidirectional_stations <- 
  daily |> 
  dplyr::filter(
    nvdb_id %in% c(toll_stations_one_direction$nvdb_id, toll_stations_one_direction_2$nvdb_id)
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, month, day)
  ) |> 
  dplyr::left_join(
    dplyr::bind_rows(
      toll_stations_one_direction,
      toll_stations_one_direction_2
    ) |> dplyr::distinct(),
    by = 'nvdb_id'
  )

daily_tridirectional_stations <-
  daily |> 
  dplyr::inner_join(
    toll_stations_triple_directions,
    by = dplyr::join_by('nvdb_id', 'direction_text')
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, directions, month, day)
  )

daily_imbalanced_stations <-
  daily |> 
  dplyr::inner_join(
    toll_stations_imbalanced_directions_manually_set,
    by = dplyr::join_by('nvdb_id', 'direction_text')
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic),
    .by = c(nvdb_id, directions, month, day)
  )

daily_both_directional_stations <-
  daily |> 
  dplyr::filter(
    nvdb_id %in% toll_stations_both_directions$nvdb_id,
    !(nvdb_id %in% toll_stations_one_direction_2$nvdb_id),
    !(nvdb_id %in% toll_stations_triple_directions$nvdb_id),
    !(nvdb_id %in% toll_stations_imbalanced_directions_manually_set$nvdb_id)
  ) |> 
  dplyr::summarise(
    traffic = sum(traffic) / 2,
    .by = c(nvdb_id, month, day)
  )

daily_tidy <-
  dplyr::bind_rows(
    daily_unidirectional_stations,
    daily_tridirectional_stations,
    daily_imbalanced_stations,
    daily_both_directional_stations |> dplyr::mutate(directions = with_chr),
    daily_both_directional_stations |> dplyr::mutate(directions = against_chr)
  ) |> 
  dplyr::mutate(
    median = median(traffic) |> round(-1),
    sd = sd(traffic) |> round(-1),
    .by = c(nvdb_id, directions, month)
  ) |>
  dplyr::mutate(
    date = lubridate::make_date(2025, 1, 1) + lubridate::days(day - 1),
    # Would like to find days when the equipment has been failing
    relative = traffic / median,
    outlier =
      dplyr::case_when(
        median > 300 & traffic < (0.05 * median) ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>
  dplyr::arrange(
    nvdb_id,
    directions,
    month,
    day
  )

length(unique(daily_tidy$nvdb_id)) == length(unique(daily$nvdb_id))

outliers <- daily_tidy |> dplyr::filter(outlier)


# AADT and SE ----
# Finally, total AADT and SE
toll_aadt_direction <-
  daily_tidy |>
  dplyr::filter(
    outlier == FALSE
  ) |>
  dplyr::summarise(
    n_days = n(),
    aadt = mean(traffic) |> round(-1),
    sd = sd(traffic) |> round(0),
    .by = c(nvdb_id, directions)
  ) |>
  dplyr::mutate(
    se = (sd / sqrt(n_days) * sqrt((365 - n_days) / (365 - 1))) |> round(0)
  ) |>
  dplyr::left_join(
    yearly_final,
    by = dplyr::join_by(nvdb_id, directions)
  ) |>
  dplyr::left_join(
    toll_stations_with_data |> dplyr::select(-directions),
    by = dplyr::join_by(nvdb_id)
  ) |>
  dplyr::select(
    nvdb_id,
    toll_station_name,
    road_reference,
    directions,
    #n_days,
    aadt,
    #sd,
    se,
    heavy_percentage
  ) 

# Write ----
toll_aadt_direction |>
  writexl::write_xlsx("toll/toll_station_aadt_2025.xlsx")

toll_aadt_direction |>
  dplyr::select(
    nvdb_id,
    directions,
    aadt,
    se,
    heavy_percentage
  ) |>
  jsonlite::write_json(
    path = "toll/toll_station_aadt_2025.json",
    prettify = TRUE
  )
