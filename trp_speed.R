# Points and speed

# Packages ####
library(tidyverse)
library(jsonlite)

# Source files ####
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")


trps <- getPoints()
# TODO: filter periodic

trp_periodic_vehicle <- getPointsFromTRPAPI_filtered()

trps_first <- trps %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::group_by(trp_id) %>%
  dplyr::summarise(start_data = min(validFrom))

# Initial attempt at querying NVDB. 500 at a time should be ok now.
trps_vehicle <- trps %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::select(-road_network_position, -road_network_link) %>%
  dplyr::filter(!(trp_id %in% c("68557V444232", "06510V72810", "19393V72810",
                                "68101V930654"))) %>%
  # To fartsgrenseobjekter ved Blommenholm Neiden tollstasjon!
  # Edit: Spørringa takler dette nå.
  # Storlersbakken er nedlagt og uten gyldig vegreferanse
  dplyr::slice(1:500) %>%
  dplyr::mutate(speed_limit = mapply(getSpeedLimit_roadlink, road_link_position))

trp_speed_1751_n <- trps_vehicle

# De med manglende responser legger vi til manuelt
trp_missing <- trps %>%
  dplyr::select(-road_network_position, -road_network_link) %>%
  dplyr::filter(trp_id %in% c("68557V444232", "68101V930654")) %>%
  dplyr::mutate(speed_limit = "80")

trp_speed <- bind_rows(
  trp_speed_1_100,
  trp_speed_101_200,
  trp_speed_201_300,
  trp_speed_301_400,
  trp_speed_401_500,
  trp_speed_501_600,
  trp_speed_601_650,
  trp_speed_650_675,
  trp_speed_676_700,
  trp_speed_701_800,
  trp_speed_801_851,
  trp_speed_851_1000,
  trp_speed_1001_1250,
  trp_speed_1251_1500,
  trp_speed_1501_1750,
  trp_speed_1751_n,
  trp_missing
)

write.csv2(trp_speed, file = "punkter_med_fartsgrense.csv")

trp_speed_80 <- trp_speed %>%
  dplyr::filter(speed_limit == 80) %>%
  dplyr::left_join(trps_first) %>%
  dplyr::mutate(start_data = ymd(start_data)) %>%
  dplyr::filter(start_data < "2018-01-01") %>%
  dplyr::filter(!(trp_id %in% trp_periodic_vehicle$trp_id)) %>%
  dplyr::select(-road_link_position)

write.csv2(trp_speed_80, file = "punkter_med_fartsgrense_80.csv")

# Points for TS ####

trp_speed_csv <- read.csv2("punkter_med_fartsgrense.csv") %>%
  dplyr::select(-1)

trp_speed_by_limit <- trp_speed_csv %>%
  dplyr::group_by(speed_limit) %>%
  dplyr::summarise(antall = n())

points <- getPoints()

trp_first_commission <- points %>%
  dplyr::select(trp_id, validFrom) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::summarise(igangsetting_1 = min(validFrom))

trp_speed_first_commission <- trp_speed_csv %>%
  dplyr::left_join(trp_first_commission) %>%
  dplyr::filter(igangsetting_1 < "2017-01-01") %>%
  tidyr::separate(road_reference, c("veg", "hp", "meter"), remove = F) %>%
  dplyr::mutate(hp = str_sub(hp, 3, -1)) %>%
  dplyr::select(-meter) %>%
  dplyr::filter(hp < 70) %>%
  dplyr::filter(speed_limit > 40) %>%
  dplyr::filter(speed_limit < 100) %>%
  # Removing trps with nulls
  dplyr::filter(!(trp_id %in% c("05325V930395", "33456V625213", "66220V72824")))

trp_aadt_coverage_1_100 <- get_aadt_for_trp(trp_speed_first_commission$trp_id[1:100])
trp_aadt_coverage_101_300 <- get_aadt_for_trp(trp_speed_first_commission$trp_id[101:200])
trp_aadt_coverage_201_400 <- get_aadt_for_trp(trp_speed_first_commission$trp_id[201:400])
trp_aadt_coverage_401_861 <- get_aadt_for_trp(trp_speed_first_commission$trp_id[401:861])

trp_aadt_coverage <- bind_rows(
  trp_aadt_coverage_1_100,
  trp_aadt_coverage_101_300,
  trp_aadt_coverage_201_400,
  trp_aadt_coverage_401_861
) %>%
  dplyr::filter(year %in% c(2017, 2018)) %>%
  dplyr::mutate(valid_length_ratio = 100 * valid_length_volume / adt,
                valid_speed_ratio = 100 * valid_speed_volume / adt) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::summarise(coverage_mean = mean(coverage),
                   valid_length_mean = mean(valid_length_ratio),
                   valid_speed_mean = mean(valid_speed_ratio),
                   adt_mean = mean(adt))

trp_speed_filtered <- trp_speed_first_commission %>%
  dplyr::left_join(trp_aadt_coverage) %>%
  dplyr::filter(coverage_mean > 99,
                valid_length_mean > 90,
                valid_speed_mean > 90)

trp_speed_by_limit_filtered <- trp_speed_filtered %>%
  dplyr::group_by(speed_limit) %>%
  dplyr::summarise(ant = n())

write.csv2(trp_speed_filtered, file = "punkter_med_god_dekning_i_2017_og_2018.csv",
           row.names = F)

trp_speed_filtered <- read.csv2("punkter_med_god_dekning_i_2017_og_2018.csv") %>%
  dplyr::filter(adt_mean < 1e4)

# Tar inn eksport fra Kibana for gjennomsnittshastighet per punkt per år
# fra volume_vehicle_day

speed_average_2017 <- read.csv2("speed_average_point_2017.csv") %>%
  dplyr::rename(trp_id = 1)
speed_average_2018 <- read.csv2("speed_average_point_2018.csv") %>%
  dplyr::rename(trp_id = 1)

# Device_type_history from TRP-API
trs_devices <- get_trs_device() %>%
  dplyr::select(trp_id, deviceType)

trp_speed_average_filtered <- trp_speed_filtered %>%
  dplyr::left_join(speed_average_2017) %>%
  dplyr::rename(speed_average_2017 = speed_average) %>%
  dplyr::left_join(speed_average_2018) %>%
  dplyr::rename(speed_average_2018 = speed_average) %>%
  dplyr::left_join(trs_devices)

write.csv2(trp_speed_average_filtered, file = "punkter_fart_apparat.csv",
           row.names = F)


# NRK ----
interval_of_interest =
  lubridate::interval(
    lubridate::ymd("2019-02-01"),
    lubridate::ymd("2023-06-01")
  )

trp_all <- get_points()

trp_tidy <-
  trp_all |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::mutate(
    validTo =
      dplyr::case_when(
        is.na(validTo) ~ lubridate::today(),
        TRUE ~ validTo
      ),
    commission_interval = lubridate::interval(validFrom, validTo),
    interval_overlap =
      lubridate::int_overlaps(
        commission_interval,
        interval_of_interest
      )
  ) |>
  dplyr::filter(
    interval_overlap == TRUE
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    n_lanes = nrow(lanes)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    -lanes
  ) |>
  dplyr::distinct() |>
  # Remove TRPs with changed n_lanes
  dplyr::add_count(trp_id) |>
  split_road_system_reference() |>
  dplyr::filter(
    n == 1,
    is.na(intersection_part_number)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    lat, lon,
    n_lanes,
    road_link_position
  )


# trp_speed <-
#   trp_tidy |>
#   dplyr::mutate(
#     speed_limit =
#       purrr::map_dbl(road_link_position, ~ get_speedlimit_by_roadlink(.))
#   )

readr::write_rds(
  trp_speed,
  file = "trp_speed.rds"
)

mdt_coverage <-
  # All MDT with at least 50 % coverage
  readr::read_csv2(
    "spesialbestillinger/mdt_coverage.csv"
  ) |>
  dplyr::rename(
    trp_id = traffic_registration_point_id
  ) |>
  dplyr::mutate(
    month =
      stringr::str_sub(from, 1, 10) |>
      lubridate::ymd(),
    year = lubridate::year(month),
    month_n = lubridate::month(month),
    #invalid_speed_days = total_volume.count - valid_speed_volume.count
  ) |>
  dplyr::filter(
    #invalid_speed_days == 0,
    # At least 16 days with valid speed per month
    valid_speed_volume.count >= 16,
    month_n %in% c(2:5)
  ) |>
  dplyr::add_count(trp_id) |>
  dplyr::select(
    trp_id,
    year,
    month_n,
    n
  ) |>
  dplyr::filter(
    n == 20
  ) |>
  dplyr::select(
    trp_id
  ) |>
  dplyr::distinct()

trp_tidy_ok_coverage <-
  trp_speed |>
  dplyr::filter(
    trp_id %in% mdt_coverage$trp_id
  ) |>
  dplyr::filter(
    stringr::str_detect(road_reference, pattern = "^F|^R|^E"),
    speed_limit > 40
  )

readr::write_rds(
  trp_tidy_ok_coverage,
  file = "trp_speed_nrk.rds"
)

trp_tidy_ok_coverage |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    speed_limit
  ) |>
  writexl::write_xlsx(
  path = "spesialbestillinger/nrk_fart.xlsx"
  )
