# Prepare data for the Rmd
source("trafficdata_functions.R")

# Øysand June 2021 ----
trp_oysand <- tibble::tibble(trp_id = c("23531V72241",
                                        "23679V72241",
                                        "23827V72241"),
                             trp_name = c("f05",
                                          "f07",
                                          "f09"))

oysand_2021w23_raw <- read_kibana_vbv("vbv_data/oysand_kp_2021w23.csv")

oysand_2021w23 <- oysand_2021w23_raw %>%
  dplyr::select(trp_id = traffic_registration_point_id,
                datalogger_type, firmware_version,
                dato, event_timestamp,
                lane, length, speed, vehicle_type_raw, qspeed,
                valid_event, valid_length, valid_speed, valid_classification) %>%
  dplyr::filter(valid_event == TRUE) %>%
  dplyr::left_join(trp_oysand, by = "trp_id") %>%
  dplyr::mutate(name_and_datalogger = paste0(datalogger_type, "_", trp_name),
                event_timestamp = with_tz(ymd_hms(event_timestamp, tz = "CET")),
                weekday = lubridate::wday(event_timestamp,
                                          label = TRUE,
                                          abbr = FALSE),
                lane = paste0("felt ", lane)) %>%
  make_norsikt_classes() %>%
  make_length_classes()


oysand_2021w23_agg_length_class_full <- oysand_2021w23 %>%
  dplyr::group_by(name_and_datalogger, weekday, length_class_full, lane) %>%
  dplyr::summarise(volume = n())

oysand_2021w23_agg_length_class_2 <- oysand_2021w23 %>%
  dplyr::group_by(name_and_datalogger, weekday, length_class_2, lane) %>%
  dplyr::summarise(volume = n())


data_interval <-
  lubridate::interval(
    start = floor_date(min(oysand_2021w23$event_timestamp), unit = "hour"),
    end = ceiling_date(max(oysand_2021w23$event_timestamp), unit = "hour"))

# Malins vbv ----

read_excelsheet <- function(filnavn, arknr) {

  readxl::read_excel(filnavn, sheet = arknr) %>%
    select(lane_number_LM,
           length_LM,
           speed_quality_LM,
           length = length_EMU3,
           speed_quality_EMU3,
           vehicle_type_LM,
           vehicle_type_EMU3) %>%
    filter(!is.na(length_LM),
           !is.na(length),
           speed_quality_LM <= 25,
           vehicle_type_EMU3 != "UC LOOP") %>%
    rename(lane = lane_number_LM) %>%
    mutate(length_diff = length - length_LM,
           emu3_valid_length = if_else(speed_quality_EMU3 != 0, FALSE, TRUE),
           emu3_valid_length = paste0("emu3_valid_length ", emu3_valid_length)) %>%
    make_length_classes()
}

EMU3_F05 <- read_excelsheet("vbv_data/oysand_comparisons/emu3_unknown_length.xlsx", 1)
EMU3_F07 <- read_excelsheet("vbv_data/oysand_comparisons/emu3_unknown_length.xlsx", 2)

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
    theme_minimal() +
    scale_color_brewer(
      palette = "Dark2",
      name = "Lengdeklasse EMU3") +
    theme(strip.text.y = element_text(angle = 90),
          strip.background = element_rect(fill = "#F5F5F5"),
          legend.position = "bottom") +
    ggtitle(plot_title,
            subtitle = paste0("Felt ", lane_number))
}
