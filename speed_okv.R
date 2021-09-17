# Parse csv files from Kibana
# Add trp name, roadref, county andt speed limit

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")


# Point metadata from Traffic Data API
points <- get_points()

trps <- points %>%
  dplyr::rowwise() %>%
  dplyr::mutate(lanes = toString(lane_numbers)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lane_numbers) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

trp_data_time_span <- get_trp_data_time_span()

trps_chosen <- readr::read_csv2("fart_okv/trp_okv.csv")

# One list with how it is like now
trp_okv <- trps %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice_max(validFrom, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::filter(trp_id %in% trps_chosen$trp_id) %>%
  dplyr::select(trp_id, name, road_reference, lanes, county_geono, county_name,
                municipality_name, lat, lon, road_link_position, operational_status) %>%
  dplyr::left_join(trp_data_time_span, by = "trp_id") %>%
  dplyr::mutate(speed_limit = mapply(get_speedlimit_by_roadlink, road_link_position)) %>%
  dplyr::arrange(speed_limit, road_reference)

readr::write_rds(trp_okv, file = "fart_okv/trp_okv.rds")
writexl::write_xlsx(trp_okv, "fart_okv/utvalgte_punkter.xlsx")
# Commission and lane history

# TODO: Speed limit history (LATER)

# Read data ----
all_data_from_kibana <- dir("fart_okv/data") %>%
  purrr::map_dfr(~ readr::read_csv2(file.path("fart_okv/data", .)))

trp_speed_compliance <- all_data_from_kibana %>%
  dplyr::mutate(compliance = dplyr::if_else(
                  stringr::str_starts(speed, "0,0"), "below", "above"),
                length_group = dplyr::if_else(
                  stringr::str_starts(length, "1,0"), "korte", "lange")) %>%
  dplyr::select(-length, -speed) %>%
  tidyr::pivot_wider(names_from = compliance, values_from = vehicles_with_valid_speed) %>%
  dplyr::mutate(compliance_percentage = round((below / (below + above) * 100), digits = 1),
                month_as_date = lubridate::as_date(month),
                year = lubridate::year(month_as_date),
                month = lubridate::month(month_as_date),
                # setting all years to same is a trick to get the plot facet correct
                month_object = lubridate::make_date(year = 2000, month = month),
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)
                ) %>%
  dplyr::inner_join(trp_okv, by = "trp_id") %>%
  split_road_system_reference() %>%
  dplyr::mutate(place_and_limit = paste0(road_category_and_number, ", ", name, ", ", speed_limit)) %>%
  dplyr::select(place_and_limit,
                length_group,
                compliance_percentage,
                month_as_date
                )

trp_speed_compliance_json <- jsonlite::toJSON(trp_speed_compliance)
write(trp_speed_compliance_json, "fart_okv/fartsdata.json")

month_start = lubridate::make_date(year = 2021, month = 1)
month_end = lubridate::make_date(year = 2021, month = 12)

full_year_x_axis <- c(month_start, month_end)

compliance_plot <- trp_speed_compliance %>%
  ggplot(aes(month_as_date, compliance_percentage, color = place_and_limit)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(length_group)) +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 90),
        strip.background = element_rect(fill = "#ececec"),
        axis.text.x = element_text(angle = 90),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom") +
  scale_x_date(breaks = scales::breaks_width("months"),
               labels = scales::label_date("%b"),
               limits = full_year_x_axis) +
  scale_color_brewer(
    palette = "Dark2",
    name = NULL) +
  #scale_color_manual(values = c(#"alle" = "#008ec2",
  #                              "korte" = "#ed9300",
  #                              "lange" = "#444f55"),
  #                   name = "Kjøretøyklasse") +
  labs(x = NULL, y = "Andel kjøretøy under fartsgrensen (%)",
       title = "Overholdelse av fartsgrensen")

compliance_plot
