## Speed and flow ----

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
        x = mean_speed,
        y = flow,
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
        yintercept = max_flow
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


## Density and flow ----
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
          x = density,
          y = flow,
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
          yintercept = max_flow
        ),
        color = "#444f55"
      ) +
      geom_vline(
        data = critical_values,
        aes(
          xintercept = road_capacity
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



## Particular day ----
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
          y = mean_speed,
          color = congestion,
          alpha = density
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


## Congestion ratio ----
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
        y = "Andel av døgn (%)",
        caption = "Data: Statens vegvesen, fylkene."
      ) +
      ggtitle(
        "Andel tid av døgn med redusert kapasitet",
        subtitle = trp_info
      )

  }

