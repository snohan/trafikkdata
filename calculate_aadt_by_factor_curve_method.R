# The Factor Curve Method for estimating AADT from periodic registrations

# Alternative: use nearby continuous points' variation curves

# Factor_curves
factor_curve_yearly <- read.csv2("factor_curve_method/factor_curves_yearly.csv") %>%
  tidyr::pivot_longer(cols = M1:M7, names_to = "curve", values_to = "factor_yearly") %>%
  dplyr::mutate(factor_yearly = factor_yearly / 100)

factor_curve_weekly <- read.csv2("factor_curve_method/factor_curves_weekly.csv") %>%
  tidyr::pivot_longer(cols = M1:M7, names_to = "curve", values_to = "factor_weekly") %>%
  dplyr::mutate(factor_weekly = factor_weekly / 100)

# Calculate aadt by daily traffic

calculate_aadt_by_daily_traffic <- function(daily_traffic) {

  # Daily traffic with coverage > 99 %, i.e. hourly factor is 1.
  # Calculates both estimated AADT

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
    dplyr::summarise(aadt = round(mean(estimated_aadt),
                                  digits = -1),
                     squares = sum((estimated_aadt - aadt)^2)) %>%
    dplyr::slice(which.min(squares))

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

# Plot

# daily_traffic_expanded %>%
#   dplyr::filter(point_name == "Krossen",
#                 weekno > 26) %>%
#   ggplot2::ggplot() +
#   geom_line(aes(from, estimated_aadt, color = curve)) +
#   geom_point(aes(from, total_volume))
