#

# vbv mutate ----

make_norsikt_classes <- function(vbv_raw_df) {

  # Works for EMU3 and Loop Monitor
  # input must have the column 'vehicle_type_raw'

  vbv_classified <- vbv_raw_df %>%
    dplyr::mutate(
      norsikt_l2 =
        dplyr::case_when(
          # EMU3:
          vehicle_type_raw == "LMV1" ~ "LMV",
          vehicle_type_raw == "LMV2" ~ "LMV",
          vehicle_type_raw == "LMV2+WC" ~ "LMV",
          vehicle_type_raw == "HMV" ~ "HMV",
          vehicle_type_raw == "HMV+WC" ~ "HMV",
          # LM:
          vehicle_type_raw == "1" ~ "LMV",
          vehicle_type_raw == "2" ~ "LMV",
          vehicle_type_raw == "3" ~ "LMV",
          vehicle_type_raw == "4" ~ "LMV",
          vehicle_type_raw == "5" ~ "LMV",
          vehicle_type_raw == "6" ~ "HMV",
          vehicle_type_raw == "71" ~ "HMV",
          vehicle_type_raw == "72" ~ "HMV",
          vehicle_type_raw == "8" ~ "HMV",
          vehicle_type_raw == "9" ~ "HMV",
          TRUE ~ "OMV"
        ),
      norsikt_l2 = factor(norsikt_l2,
                          levels = c("LMV",
                                     "HMV",
                                     "OMV")),
      norsikt_l3 =
        dplyr::case_when(
          # EMU3:
          vehicle_type_raw == "LMV1" ~ "MC_MP",
          vehicle_type_raw == "LMV2" ~ "PC_LGV_LB",
          vehicle_type_raw == "LMV2+WC" ~ "PC_LGV_LB",
          vehicle_type_raw == "HMV" ~ "HMV",
          vehicle_type_raw == "HMV+WC" ~ "HMV",
          # LM:
          vehicle_type_raw == "1" ~ "MC_MP",
          vehicle_type_raw == "2" ~ "PC_LGV_LB",
          vehicle_type_raw == "3" ~ "PC_LGV_LB",
          vehicle_type_raw == "4" ~ "PC_LGV_LB",
          vehicle_type_raw == "5" ~ "PC_LGV_LB",
          vehicle_type_raw == "6" ~ "HMV",
          vehicle_type_raw == "71" ~ "HMV",
          vehicle_type_raw == "72" ~ "HMV",
          vehicle_type_raw == "8" ~ "HMV",
          vehicle_type_raw == "9" ~ "HMV",
          TRUE ~ "OMV"
        ),
      norsikt_l3 = factor(norsikt_l3,
                          levels = c("MC_MP",
                                     "PC_LGV_LB",
                                     "HMV",
                                     "OMV")),
      norsikt_l4h =
        dplyr::case_when(
          # EMU3:
          vehicle_type_raw == "LMV1" ~ "MC_MP",
          vehicle_type_raw == "LMV2" ~ "PC_LGV_LB",
          vehicle_type_raw == "LMV2+WC" ~ "PC_LGV_LB",
          vehicle_type_raw == "HMV" ~ "OMV",
          vehicle_type_raw == "HMV+WC" ~ "OMV",
          # LM:
          vehicle_type_raw == "1" ~ "MC_MP",
          vehicle_type_raw == "2" ~ "PC_LGV_LB",
          vehicle_type_raw == "3" ~ "PC_LGV_LB",
          vehicle_type_raw == "4" ~ "PC_LGV_LB",
          vehicle_type_raw == "5" ~ "PC_LGV_LB",
          vehicle_type_raw == "6" ~ "HB",
          vehicle_type_raw == "71" ~ "HGV_RT_EMS",
          vehicle_type_raw == "72" ~ "HGV_RT_EMS",
          vehicle_type_raw == "8" ~ "HGV_WC",
          vehicle_type_raw == "9" ~ "RT_WC",
          TRUE ~ "OMV"
        ),
      norsikt_l4h = factor(norsikt_l4h,
                          levels = c("MC_MP",
                                     "PC_LGV_LB",
                                     "HB",
                                     "HGV_RT_EMS",
                                     "HGV_WC",
                                     "RT_WC",
                                     "OMV"))
    )
}

make_length_classes <- function(vbv_raw_df) {

  # maps lengths into traditional length classes
  # vbv_raw_df must have column 'length'

  vbv_length_classified <- vbv_raw_df %>%
    dplyr::mutate(length_class_2 =
                    dplyr::case_when(
                      length < 5.6 ~ "[..,5.6)",
                      length >= 5.6 ~ "[5.6,..)"
                      ),
                  length_class_2 = factor(length_class_2,
                                          levels = c("[..,5.6)",
                                                     "[5.6,..)")),
                  length_class_full =
                    dplyr::case_when(
                      length < 5.6 ~ "[..,5.6)",
                      length < 7.6 ~ "[5.6,7.6)",
                      length < 12.5 ~ "[7.6,12.5)",
                      length < 16 ~ "[12.5,16)",
                      length < 24 ~ "[16,24)",
                      length >= 24 ~ "[24,..)",
                    ),
                  length_class_full = factor(length_class_full,
                                             levels = c("[..,5.6)",
                                                        "[5.6,7.6)",
                                                        "[7.6,12.5)",
                                                        "[12.5,16)",
                                                        "[16,24)",
                                                        "[24,..)"))
                  )
}

lane_order <- c("felt 7", "felt 5", "felt 3", "felt 1",
                "felt 2", "felt 4", "felt 6", "felt 8")

# vbv read ----
read_kibana_vbv <- function(vbv_file) {
  read_csv2(vbv_file) %>%
    dplyr::mutate(
      #lane = as.character(lane),
      event_timestamp = with_tz(ymd_hms(event_timestamp, tz = "CET")),
      lane = paste0("felt ", lane),
      lane = factor(lane, levels = lane_order),
      vehicle_type_raw = as.character(vehicle_type_raw)) %>%
    make_norsikt_classes()
}

read_excelsheet <- function(filnavn, arknr) {

  readxl::read_excel(filnavn, sheet = arknr) %>%
    select(lane_number_LM,
           length_LM,
           speed_quality_LM,
           length = length_EMU3,
           speed_quality_EMU3,
           vehicle_type_raw_LM,
           vehicle_type_raw_EMU3) %>%
    filter(!is.na(length_LM),
           !is.na(length)#,
           #speed_quality_LM <= 25,
           #vehicle_type_raw_EMU3 != "UC LOOP"
           ) %>%
    rename(lane = lane_number_LM) %>%
    mutate(length_diff = length - length_LM,
           emu3_valid_length = if_else(speed_quality_EMU3 != 0, FALSE, TRUE),
           emu3_valid_length = paste0("emu3_valid_length ", emu3_valid_length)) %>%
    make_length_classes()
}

# vbv aggregate ----
calculate_heavy_ratio_from_vbv_by_length <- function(vbv_df) {

  hr_df <- vbv_df %>%
    dplyr::group_by(name_and_datalogger, weekday, lane, length_class_2) %>%
    dplyr::summarise(volume = n()) %>%
    dplyr::mutate(length_class_2 =
                    if_else(length_class_2 == "[..,5.6)",
                            "light",
                            "heavy")
                  ) %>%
    tidyr::pivot_wider(names_from = length_class_2, values_from = volume) %>%
    dplyr::mutate(heavy_ratio = round(100 * heavy / (light + heavy), digits = 1))
}

calculate_heavy_ratio_from_vbv_by_class <- function(vbv_df) {

  hr_df <- vbv_df %>%
    dplyr::group_by(name_and_datalogger, weekday, lane, norsikt_l2) %>%
    dplyr::mutate(norsikt_l2 = paste0("class_", norsikt_l2)) %>%
    dplyr::summarise(volume = n()) %>%
    tidyr::pivot_wider(names_from = norsikt_l2, values_from = volume) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_volume = sum(c_across(starts_with("class")), na.rm = TRUE),
                  heavy_ratio = round(100 * class_HMV / total_volume, digits = 1))
}

# vbv plots ----
norsikt_l3_colors <- c("MC_MP" = "#008ec2",
                       "PC_LGV_LB" = "#ed9300",
                       "HMV" = "#58b02c",
                       "OMV" = "#444f55" )

limit_color = "#ed1c2e"

alpha_value = 0.6

plot_qspeed_length <- function(vbv_data) {
  vbv_data %>%
    ggplot() +
    geom_point(aes(qspeed, length, color = norsikt_l3), alpha = alpha_value) +
    geom_vline(aes(xintercept = 25), color = limit_color, alpha = alpha_value) +
    geom_hline(aes(yintercept = 1), color = limit_color, alpha = alpha_value) +
    geom_hline(aes(yintercept = 5.6), color = "#dadada", alpha = alpha_value) +
    geom_hline(aes(yintercept = 27), color = limit_color, alpha = alpha_value) +
    scale_color_manual(values = norsikt_l3_colors) +
    facet_grid(cols = vars(lane)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "#ececec"),
          legend.position = "none") +
    xlab("fartsulikhet\n") +
    ylab("lengde (m)\n")
}

plot_qspeed_speed <- function(vbv_data) {
  vbv_data %>%
    ggplot() +
    geom_point(aes(qspeed, speed, color = norsikt_l3), alpha = alpha_value) +
    geom_vline(aes(xintercept = 25), color = limit_color, alpha = alpha_value) +
    scale_color_manual(values = norsikt_l3_colors) +
    facet_grid(cols = vars(lane)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "#ececec"),
          legend.position = "none") +
    xlab("fartsulikhet\n") +
    ylab("fart (km/h)\n")
}

plot_length_speed <- function(vbv_data) {
  vbv_data %>%
    ggplot() +
    geom_point(aes(length, speed, color = norsikt_l3), alpha = alpha_value) +
    geom_vline(aes(xintercept = 1), color = limit_color, alpha = alpha_value) +
    geom_vline(aes(xintercept = 5.6), color = "#dadada", alpha = alpha_value) +
    geom_vline(aes(xintercept = 27), color = limit_color, alpha = alpha_value) +
    scale_color_manual(values = norsikt_l3_colors) +
    facet_grid(cols = vars(lane)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "#ececec"),
          legend.position = "bottom") +
    labs(
      x = "lengde (m)",
      y = "fart (km/h)\n",
      color = "Kjøretøyklasse (NorSiKT nivå 3)"
    )
}

# plot_speed_relqspeed <- function(vbv_data) {
#   vbv_data %>%
#     mutate(relative_qspeed = qspeed / speed) %>%
#   ggplot() +
#   geom_point(aes(relative_qspeed, speed, color = lane)) +
#   scale_color_discrete() +
#     theme_minimal()
# }

# plot_length_class <- function(vbv_data) {
#   vbv_data %>%
#   ggplot() +
#   geom_point(aes(qspeed, length, color = vehicle_type)) +
#   geom_vline(aes(xintercept = 3)) +
#   geom_hline(aes(yintercept = 2)) +
#   geom_hline(aes(yintercept = 5.6), color = "grey") +
#   geom_hline(aes(yintercept = 27)) +
#   scale_color_discrete() +
#     theme_minimal()
# }

# plot_length_class_per_lane <- function(vbv_data) {
#   vbv_data %>%
#     group_by(lane) %>%
#     ggplot() +
#     geom_point(aes(qspeed, length, color = vehicle_type)) +
#     facet_wrap(. ~ lane, labeller = label_both) +
#     geom_vline(aes(xintercept = 3)) +
#     geom_hline(aes(yintercept = 2)) +
#     geom_hline(aes(yintercept = 5.6), color = "grey") +
#     geom_hline(aes(yintercept = 27)) +
#     scale_color_discrete() +
#     theme_minimal()
# }

# plot_length_class_per_lane_relqspeed <- function(vbv_data) {
#   vbv_data %>%
#     mutate(relative_qspeed = qspeed / speed) %>%
#     group_by(lane) %>%
#     ggplot() +
#     geom_point(aes(relative_qspeed, length, color = vehicle_type)) +
#     facet_wrap(. ~ lane, labeller = label_both) +
#     geom_hline(aes(yintercept = 2)) +
#     geom_hline(aes(yintercept = 5.6), color = "grey") +
#     geom_hline(aes(yintercept = 27)) +
#     scale_color_discrete() +
#     theme_minimal()
# }

# TODO: ensure same class colors across sites
plot_all_in_one <- function(vbv_data) {
  cowplot::plot_grid(
    plot_qspeed_length(vbv_data),
    plot_qspeed_speed(vbv_data),
    plot_length_speed(vbv_data),
    ncol = 1)
}

plot_vbv_data <- function(vbv_data, lane_number, plot_title) {

  length_breaks <- c(5.6, 7.6, 12.5, 16, 24)

  vbv_data %>%
    filter(lane == lane_number) %>%
    ggplot(aes(length_LM, length_diff, color = length_class_full)) +
    geom_jitter(alpha = 0.5) +
    geom_vline(xintercept = length_breaks) +
    facet_grid(rows = vars(emu3_valid_length),
               labeller = label_wrap_gen(width = 12)) +
    ylab("Lengdedifferanse (m)\n") +
    xlab("\nLengde målt med Loop Monitor (m)") +
    scale_x_continuous(breaks = length_breaks) +
    theme_bw() +
    scale_color_brewer(
      palette = "Dark2",
      name = "Lengdeklasse EMU3") +
    theme(strip.text.y = element_text(angle = 90),
          strip.background = element_rect(fill = "#F5F5F5"),
          legend.position = "bottom") +
    ggtitle(plot_title,
            subtitle = paste0("Felt ", lane_number))
}

plot_different_length_classes <- function(diff_class_df, subtitle_text) {

  diff_class_df %>%
    ggplot(aes(length_LM, length_EMU3,
               color = norsikt_l2_EMU3,
               shape = norsikt_l2_LM)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 5.6) +
    facet_grid(rows = vars(lane),
               labeller = label_wrap_gen(width = 12)) +
    xlab("\nLengde LM (m)") +
    ylab("Lengde EMU3 (m)\n") +
    theme_bw() +
    scale_color_brewer(
      palette = "Dark2",
      name = "Kjøretøyklasse EMU3") +
    scale_shape(name = "Kjøretøyklasse LM") +
    theme(strip.text.y = element_text(angle = 90),
          strip.background = element_rect(fill = "#F5F5F5"),
          legend.position = "bottom") +
    ggtitle("Forskjell i klassifisering",
            subtitle = subtitle_text)
}

plot_ecdf <- function(df, subtitle_text) {

  df %>%
    ggplot(aes(length, color = name_and_datalogger)) +
    stat_ecdf(pad = FALSE,
              size = 1, alpha = 0.7) +
    facet_grid(rows = vars(lane_number)) +
    theme_bw() +
    scale_color_brewer(
      palette = "Dark2",
      name = "Datalogger og punkt") +
    theme(strip.text.y = element_text(angle = 90),
          strip.background = element_rect(fill = "#F5F5F5"),
          legend.position = "bottom") +
    labs(x = "\nLengde (m)", y = "Kumulativ tetthet\n") +
    ggtitle("Kumulativ fordeling av lengdemålinger",
            subtitle = subtitle_text)
}

plot_qq <- function(df, subtitle_text) {

  df %>%
    ggplot(aes(sample = length, color = name_and_datalogger)) +
    facet_grid(rows = vars(lane_number)) +
    stat_qq(size = 1, alpha = 0.7) +
    theme_bw() +
    scale_color_brewer(
      palette = "Dark2",
      name = "Datalogger og punkt") +
    theme(strip.text.y = element_text(angle = 90),
          strip.background = element_rect(fill = "#F5F5F5"),
          legend.position = "bottom")+
    ggtitle("Kvantilplott av lengdemålinger",
            subtitle = subtitle_text)
}

plot_length_class_bars <- function(df, length_class_type, subtitle_text) {

  # length_class_type must be either
  # length_class_full
  # or
  #length_class_2

  df %>%
    ggplot(aes(x = {{ length_class_type }}, y = volume, fill = name_and_datalogger)) +
    ggplot2::geom_col(position = "dodge") +
    facet_grid(rows = vars(lane_number)) +
    theme_minimal() +
    scale_fill_viridis_d(name = "Datalogger",
                         option = "cividis") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text.y = element_text(angle = 90),
          strip.background = element_rect(fill = "#F5F5F5"),
          legend.position = "bottom",
          legend.key = element_blank()) +
    labs(x = "\nLengdeklasse", y = "Trafikkmengde\n") +
    ggtitle("Trafikkmengde per lengdeklasse",
            subtitle = subtitle_text)
}

plot_heavy_ratio <- function(df) {

  df %>%
    ggplot(aes(x = weekday, y = heavy_ratio, fill = name_and_datalogger)) +
    ggplot2::geom_col(position = "dodge") +
    geom_text(aes(label = paste0(heavy_ratio, " %"),
                   y = 1),
              color = "grey",
              position = position_dodge(0.9),
              vjust = 0) +
    facet_grid(rows = vars(lane)) +
    theme_minimal() +
    scale_fill_viridis_d(name = "Datalogger",
                         option = "cividis") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text.y = element_text(angle = 90),
          strip.background = element_rect(fill = "#F5F5F5"),
          legend.position = "bottom",
          legend.key = element_blank()) +
    labs(x = "\nUkedag", y = "Andel tunge\n") +
    ggtitle("Andel tunge kjøretøy")
}


# Centric passings ----
options(digits.secs = 3)


read_vbv_for_centric <- function(vbv_file) {

  vbv_data <- read_csv2(vbv_file) %>%
    dplyr::mutate(
      event_timestamp = with_tz(ymd_hms(event_timestamp, tz = "CET")),
      epoch_time = as.numeric(event_timestamp)
    ) %>%
    dplyr::arrange(epoch_time) %>%
    make_norsikt_classes()
}

find_centric_passings <- function(vbv_data) {

  # Not for just two lanes in opposite directions
  # In: vbv with timestamp in ms, lane number
  # Out: possible centric passings in pairs
  # TODO: at least one in the pair must have length 2 or shorter (LM)
  # TODO: set the one of least quality as valid_event == FALSE

  possible_adjacent_lanes <-
    tibble(
      one = c(1, 3, 5, 2, 4, 6),
      two = c(3, 5, 7, 4, 6, 8)
    )

  present_lanes <- vbv_data$lane %>%
    unique()

  present_lanes_to_loop <- possible_adjacent_lanes %>%
    dplyr::mutate(
      present = one %in% present_lanes & two %in% present_lanes
    ) %>%
    dplyr::filter(present == TRUE)


  iterations <- nrow(present_lanes_to_loop)
  iteration_n <- 1
  possible_centrics <- data.frame()

  while (iteration_n <= iterations) {

    centrics <- vbv_data %>%
      dplyr::filter(
        lane %in% c(
          present_lanes_to_loop$one[iteration_n],
          present_lanes_to_loop$two[iteration_n]
          )
        ) %>%
    dplyr::mutate(
      close_in_time =
        abs(epoch_time - dplyr::lag(epoch_time)) < 0.5 |
        abs(epoch_time - dplyr::lead(epoch_time)) < 0.5
    ) %>%
    dplyr::filter(
      close_in_time == TRUE
    )

    possible_centrics <-
      dplyr::bind_rows(
        possible_centrics,
        centrics
      )

    iteration_n <- iteration_n + 1
  }

  return(possible_centrics)

}

find_single_short_vehicles <- function(vbv_data, centric_passings) {

  # filter based on length, leave out event numbers found in centric passings

  short_vehicles_not_in_centric_pairs <-
    vbv_data %>%
    dplyr::filter(length < 3,
                  length >= 0) %>%
    dplyr::filter(!(event_number %in% centric_passings$event_number))

}

# havnegata <- read_vbv_for_centric("vbv_data/havnegata.csv")
# havnegata_sentriske <- find_centric_passings(havnegata)
# havnegate_enslige <- find_single_short_vehicles(havnegata, havnegata_sentriske)
#
# rotvollekra <- read_vbv_for_centric("vbv_data/rotvollekra.csv")
# rotvollekra_sentriske <- find_centric_passings(rotvollekra)
# rotvollekra_enslige <- find_single_short_vehicles(rotvollekra, rotvollekra_sentriske)
#
# holtermannsvegen <- read_vbv_for_centric("vbv_data/holtermannsvegen.csv")
# holtermannsvegen_sentriske <- find_centric_passings(holtermannsvegen)
# holtermannsvegen_enslige <- find_single_short_vehicles(holtermannsvegen,
#                                                        holtermannsvegen_sentriske)


# Read manual video verification files ----
read_manual_video_verification_file <-
  function(manual_file_location, datalogger_type) {

  manual_file <-
    readxl::read_excel(
      manual_file_location,
      1
    ) %>%
    dplyr::filter(
      !is.na(event_type),
      !is.na(true_vehicle)
    ) %>%
    dplyr::mutate(
      datalogger_type = datalogger_type,
      vehicle_type_raw = as.character(vehicle_type_raw)
    ) %>%
    make_norsikt_classes() %>%
    make_length_classes() %>%
    dplyr::select(
      datalogger_type,
      event_type,
      comment,
      true_vehicle,
      length,
      valid_length,
      length_ok,
      true_length_class_2,
      length_class_2,
      vehicle_type_raw,
      valid_classification,
      true_norsikt_2,
      norsikt_l2,
      true_norsikt_3,
      norsikt_l3,
      true_norsikt_4,
      norsikt_l4h,
      speed,
      valid_speed,
      speed_ok
    )
}

