# The Factor Curve Method for estimating AADT from periodic registrations
library(tidyverse)
library(lubridate)

# Alternative: use nearby continuous points' variation curves, a.k.a. the reference method, by
# fetching e.g. 10 nearest points with high quality curves for the year.

# Defining the factor curves ####
factor_curve_yearly <- read.csv2("factor_curve_method/factor_curves_yearly.csv") %>%
  tidyr::pivot_longer(cols = M1:M7, names_to = "curve", values_to = "factor_yearly") %>%
  dplyr::mutate(factor_yearly = factor_yearly / 100)

factor_curve_weekly <- read.csv2("factor_curve_method/factor_curves_weekly.csv") %>%
  tidyr::pivot_longer(cols = M1:M7, names_to = "curve", values_to = "factor_weekly") %>%
  dplyr::mutate(factor_weekly = factor_weekly / 100)

factor_curve_daily <- read.csv2("factor_curve_method/factor_curves_daily.csv") %>%
  tidyr::pivot_longer(cols = M1:M7, names_to = "curve", values_to = "factor_daily") %>%
  dplyr::mutate(factor_daily = factor_daily / 100,
                time = time - 1)


# Function to calculate aadt by daily traffic ####

calculate_aadt_by_daily_traffic <- function(daily_traffic) {

  # Daily traffic with coverage > 99 %, i.e. hourly factor is 1.
  # Calculates both estimated AADT
  # TODO: calculate uncertainty

  daily_traffic_expanded <- daily_traffic %>%
    dplyr::left_join(factor_curve_yearly, by = c("weekno" = "uke")) %>%
    dplyr::left_join(factor_curve_weekly, by = c("dayno" = "ukedag",
                                                 "curve" = "curve"),
                     suffix = c("_yearly", "_weekly")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      combined_factor = factor_yearly * factor_weekly,
      estimated_aadt = total_volume / combined_factor)

  final_aadt_estimate <- daily_traffic_expanded %>%
    dplyr::group_by(point_id, curve) %>%
    dplyr::summarise(aadt = round(mean(estimated_aadt, na.rm = TRUE),
                                  digits = -1),
                     squares = sum((estimated_aadt - aadt)^2)) %>%
    #dplyr::slice(which.min(squares)) %>% # better to use:
    dplyr::slice_min(squares)

  # TODO: extract as own function?
  ratio_heavy <- daily_traffic %>%
    dplyr::filter(valid_length > 95) %>%
    dplyr::group_by(point_id) %>%
    dplyr::summarise(heavy_ratio = round(100 * sum(length_range_volume) / sum(total_volume),
                                         digits = 0),
                     days_with_length = n())

  final_aadt_estimate_with_heavy <- final_aadt_estimate %>%
    dplyr::left_join(ratio_heavy) %>%
    dplyr::select(trp_id = point_id, curve, aadt, heavy_ratio)

  return(final_aadt_estimate_with_heavy)
}

# TODO: likewise function for radarpoints (lacking valid_length)
calculate_aadt_by_daily_traffic_radar <- function(daily_traffic) {

  # Complete daily traffic, i.e. hourly factor is 1.
  # Input file must have point_id, total_volume, heavy_ratio, weekno, dayno

  daily_traffic_expanded <- daily_traffic %>%
    dplyr::left_join(factor_curve_yearly, by = c("weekno" = "uke")) %>%
    dplyr::left_join(factor_curve_weekly, by = c("dayno" = "ukedag",
                                                 "curve" = "curve"),
                     suffix = c("_yearly", "_weekly")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      combined_factor = factor_yearly * factor_weekly,
      estimated_aadt = total_volume / combined_factor)

  final_aadt_estimate <- daily_traffic_expanded %>%
    dplyr::group_by(point_id, curve) %>%
    dplyr::summarise(aadt = round(mean(estimated_aadt, na.rm = TRUE),
                                  digits = -1),
                     standard_deviation = round(sd(estimated_aadt, na.rm = TRUE), digits = 0),
                     n_days = n(),
                     # TODO: multiply SE by finite population correction factor?
                     standard_error = round(standard_deviation / sqrt(n_days), digits = 0),
                     squares = sum((estimated_aadt - aadt)^2)) %>%
    dplyr::slice_min(squares)

  ratio_heavy <- daily_traffic %>%
    dplyr::group_by(point_id) %>%
    dplyr::summarise(heavy_ratio = round(mean(heavy_ratio, na.rm = TRUE),
                                         digits = 0),
                     complete_days = n())

  final_aadt_estimate_with_heavy <- final_aadt_estimate %>%
    dplyr::left_join(ratio_heavy) %>%
    dplyr::select(site_id = point_id, complete_days, curve, aadt, standard_error,
                  standard_deviation, heavy_ratio)

  return(final_aadt_estimate_with_heavy)
}

calculate_aadt_by_hourly_traffic <- function(hourly_traffic) {

  # Complete hourly traffic only.
  # Input file must have:
  # point_id, datetime (floored to hour), heavy, total_volume.

  hourly_traffic_complete_hours <- hourly_traffic %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      weekno = lubridate::isoweek(datetime),
      dayno = lubridate::wday(
        datetime,
        week_start = getOption("lubridate.week.start", 1)),
      hourno = lubridate::hour(datetime),
      datetime_epoch = as.numeric(datetime) / 3600,
      hour_before = abs(dplyr::lag(datetime_epoch) - datetime_epoch),
      hour_after = dplyr::lead(datetime_epoch) - datetime_epoch
      ) %>%
    dplyr::filter(
      hour_before == 1,
      hour_after == 1
      )

  hourly_traffic_expanded <- hourly_traffic_complete_hours %>%
    dplyr::left_join(factor_curve_yearly, by = c("weekno" = "uke")) %>%
    dplyr::left_join(factor_curve_weekly, by = c("dayno" = "ukedag",
                                                 "curve" = "curve"),
                     suffix = c("_yearly", "_weekly")) %>%
    dplyr::left_join(factor_curve_daily, by = c("hourno" = "time",
                                                 "curve" = "curve")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(datetime_day = lubridate::floor_date(datetime, unit = "day")) %>%
    dplyr::group_by(point_id, datetime_day, curve, factor_yearly, factor_weekly) %>%
    dplyr::summarise(total_volume = sum(total_volume, na.rm = TRUE),
                     heavy = sum(heavy, na.rm = TRUE),
                     sum_factor_daily = sum(factor_daily)) %>%
    dplyr::mutate(
      combined_factor = factor_yearly * factor_weekly * sum_factor_daily,
      estimated_aadt = total_volume / combined_factor)

  final_aadt_estimate <- hourly_traffic_expanded %>%
    dplyr::group_by(point_id, curve) %>%
    dplyr::summarise(aadt = round(mean(estimated_aadt, na.rm = TRUE),
                                  digits = -1),
                     standard_deviation = round(sd(estimated_aadt, na.rm = TRUE), digits = 0),
                     n_days = n(),
                     # TODO: multiply SE by finite population correction factor?
                     standard_error = round(standard_deviation / sqrt(n_days), digits = 0),
                     squares = sum((estimated_aadt - aadt)^2)) %>%
    dplyr::slice_min(squares)

  heavy_percentage <- hourly_traffic %>%
    dplyr::group_by(point_id) %>%
    dplyr::summarise(
      heavy_percentage = round(
        sum(heavy, na.rm = TRUE) / sum(total_volume, na.rm = TRUE) * 100,
        digits = 0))

  final_aadt_estimate_with_heavy <- final_aadt_estimate %>%
    dplyr::left_join(heavy_percentage) %>%
    dplyr::select(site_id = point_id, curve, aadt, standard_error,
                  standard_deviation, heavy_percentage)

  return(final_aadt_estimate_with_heavy)
}

# Plot to manually check ####

# daily_traffic_expanded %>%
#   dplyr::filter(point_name == "Krossen",
#                 weekno > 26) %>%
#   ggplot2::ggplot() +
#   geom_line(aes(from, estimated_aadt, color = curve)) +
#   geom_point(aes(from, total_volume))
