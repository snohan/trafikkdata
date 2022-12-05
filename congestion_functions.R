# Utilities ----

# Read Kibana-exported CSVs ----
read_a_file <- function(file_name) {

  readr::read_csv2(
    paste0("congestion_data/", file_name)
  )

}

find_trp_info_and_direction_names <- function(aggregated_data) {

  # Needs a "trp" df with all TRP info from Trafikkdata API

  trp_here <-
    trp |>
    dplyr::filter(
      trp_id == data_congested$trp_id[1]
    ) |>
    split_road_system_reference()

  trp_info <-
    base::paste0(
      trp_here$road_category_and_number,
      " ",
      stringr::str_to_title(trp_here$name),
      ", ",
      trp_here$municipality_name
    )

  trp_direction_names <-
    trp_here |>
    dplyr::select(
      from,
      to
    ) |>
    tidyr::pivot_longer(
      cols = c(from, to),
      names_to = "trp_direction",
      values_to = "direction_name_to"
    ) |>
    dplyr::mutate(
      direction_name_to = stringr::str_to_title(direction_name_to)
    )

  trp_directions <-
    data_congested |>
    dplyr::distinct(lane) |>
    dplyr::mutate(
      trp_direction = dplyr::if_else(lane %% 2 == 0, "from", "to")
    ) |>
    dplyr::left_join(
      trp_direction_names,
      by = "trp_direction"
    ) |>
    dplyr::mutate(
      name_string = paste0(
        "Felt ",
        lane,
        ": til ",
        direction_name_to
      )
    )

  lane_names <-
    trp_directions$name_string

  names(lane_names) <-
    trp_directions$lane

  result <- list(
    trp_info,
    lane_names
  )

  return(result)
}


# Speed and flow ----

visualize_speed_and_flow <-
  function(
    critical_values,
    aggregated_data,
    trp_info,
    lane_info
    ) {

  svv_background_color <- "#F5F5F5"

  aggregated_data |>
    ggplot(
      aes(
        x = space_mean_speed,
        y = pce_flow,
        color = congestion
      )
    ) +
    geom_point(
      size = 2,
      alpha = 0.4
    ) +
    facet_wrap(
      vars(lane),
      labeller = labeller(lane = lane_info)
    ) +
    geom_hline(
      data = critical_values,
      aes(
        yintercept = pce_max_flow
      ),
      color = "#444f55"
    ) +
    geom_vline(
      data = critical_values,
      aes(
        xintercept = critical_speed
      ),
      color = "#444f55"
    ) +
    theme_light() +
    scale_color_manual(
      values =
        c("Nei" = "#dadada",
          "Ja" = "#008ec2"),
      name = "Redusert\nkapasitet"
    ) +
    theme(
      panel.grid.minor.x = element_blank(),
      legend.position = "right",
      axis.title.x = element_text(
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(vjust = 0.5),
      plot.caption =
        element_text(
          face = "italic",
          size = 8,
          lineheight = 1.5,
          vjust = 0
        ),
      strip.background =
        element_rect(
          colour = "#444f55",
          fill = "#ed9300"
        ),
      strip.text =
        element_text(
          colour = "#444f55"
        ),
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.background = element_rect(fill = svv_background_color),
      legend.key = element_blank()
    ) +
    labs(
      x = "Gjennomsnittsfart (km/time)",
      y = "Trafikkstrøm (kjøretøy/time)",
      caption = "Data: Statens vegvesen, fylkene."
    ) +
    ggtitle(
      "Gjennomsnittsfart og trafikkstrøm i intervaller på 5 min",
      subtitle = trp_info
    )

  }


# Density and flow ----
visualize_density_and_flow <-
  function(
    critical_values,
    aggregated_data,
    trp_info,
    lane_info
  ) {

    svv_background_color <- "#F5F5F5"

    aggregated_data |>
      ggplot(
        aes(
          x = pce_density,
          y = pce_flow,
          color = congestion
        )
      ) +
      geom_point(
        size = 2,
        alpha = 0.4
      ) +
      facet_wrap(
        vars(lane),
        labeller = labeller(lane = lane_info)
      ) +
      geom_hline(
        data = critical_values,
        aes(
          yintercept = pce_max_flow
        ),
        color = "#444f55"
      ) +
      geom_vline(
        data = critical_values,
        aes(
          xintercept = pce_road_capacity
        ),
        color = "#444f55"
      ) +
      theme_light() +
      scale_color_manual(
        values =
          c("Nei" = "#dadada",
            "Ja" = "#008ec2"),
        name = "Redusert\nkapasitet"
      ) +
      theme(
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        axis.title.x = element_text(
          margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(vjust = 0.5),
        plot.caption =
          element_text(
            face = "italic",
            size = 8,
            lineheight = 1.5,
            vjust = 0
          ),
        strip.background =
          element_rect(
            colour = "#444f55",
            fill = "#ed9300"
          ),
        strip.text =
          element_text(
            colour = "#444f55"
          ),
        plot.background = element_rect(fill = svv_background_color),
        panel.background = element_rect(fill = svv_background_color),
        legend.background = element_rect(fill = svv_background_color),
        legend.key = element_blank()
      ) +
      labs(
        x = "Trafikktetthet (kjøretøy/km)",
        y = "Trafikkstrøm (kjøretøy/time)",
        caption = "Data: Statens vegvesen, fylkene."
      ) +
      ggtitle(
        "Trafikktetthet og trafikkstrøm i intervaller på 5 min",
        subtitle = trp_info
      )

  }



# Particular day ----
visualize_speed_day <-
  function(
    critical_values,
    aggregated_data,
    trp_info,
    lane_info,
    selected_day
  ) {

    # selected_day e.g. "2021-10-28"

    svv_background_color <- "#F5F5F5"

    aggregated_data |>
      dplyr::filter(
        date == selected_day
      ) |>
      ggplot(
        aes(
          x = timestamp_floored,
          y = space_mean_speed,
          color = congestion,
          alpha = pce_density
        )
      ) +
      geom_point(
        size = 2
      ) +
      facet_wrap(
        vars(lane),
        labeller = labeller(lane = lane_info)
      ) +
      geom_hline(
        data = critical_values,
        aes(
          yintercept = critical_speed
        ),
        color = "#444f55"
      ) +
      theme_light() +
      scale_x_datetime(
        date_labels = "%H"
      ) +
      scale_color_manual(
        values =
          c("Nei" = "#444f55",
            "Ja" = "#008ec2"),
        name = "Redusert\nkapasitet"
      ) +
      scale_alpha(
        name = "Trafikktetthet"
      ) +
      theme(
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        axis.title.x = element_text(
          margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(vjust = 0.5),
        plot.caption =
          element_text(
            face = "italic",
            size = 8,
            lineheight = 1.5,
            vjust = 0
          ),
        strip.background =
          element_rect(
            colour = "#444f55",
            fill = "#ed9300"
          ),
        strip.text =
          element_text(
            colour = "#444f55"
          ),
        plot.background = element_rect(fill = svv_background_color),
        panel.background = element_rect(fill = svv_background_color),
        legend.background = element_rect(fill = svv_background_color),
        legend.key = element_blank()
      ) +
      labs(
        x = NULL,
        y = "Gjennomsnittsfart (km/time)",
        caption = "Data: Statens vegvesen, fylkene."
      ) +
      ggtitle(
        "Gjennomsnittsfart i intervaller på 5 min",
        subtitle = paste0(trp_info, ", ", selected_day)
      )

  }


# Congestion ratio ----
visualize_congestion_ratio <-
  function(
    congestion_stats,
    trp_info,
    lane_info
  ) {

    svv_background_color <- "#F5F5F5"

    congestion_stats |>
      ggplot(
        aes(
          x = weekday,
          y = congestion_percentage,
          fill = year
        )
      ) +
      geom_col(
        position = "dodge"
      ) +
      scale_fill_grey(
        name = "År"
      ) +
      facet_wrap(
        vars(lane),
        labeller = labeller(lane = lane_info)
      ) +
      theme_light() +
      theme(
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5
        ),
        axis.text.y = element_text(vjust = 0.5),
        plot.caption =
          element_text(
            face = "italic",
            size = 8,
            lineheight = 1.5,
            vjust = 0
          ),
        strip.background =
          element_rect(
            colour = "#444f55",
            fill = "#ed9300"
          ),
        strip.text =
          element_text(
            colour = "#444f55"
          ),
        plot.background = element_rect(fill = svv_background_color),
        panel.background = element_rect(fill = svv_background_color),
        legend.background = element_rect(fill = svv_background_color),
        legend.key = element_blank()
      ) +
      labs(
        x = NULL,
        y = "Andel av døgn (%)",
        caption = "Data: Statens vegvesen, fylkene."
      ) +
      ggtitle(
        "Andel tid av døgn med redusert kapasitet",
        subtitle = trp_info
      )

  }

