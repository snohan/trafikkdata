# Prepare data for Rmd

# Model results ----
# Model predictions for Vestland and Rogaland
geopackage_file <-
  "nr_prep_links/trafikklenker_dir_rv_2021_preproc_predaadt_balaadt_noder-kanter-kategorisert.gpkg"
#layers <- sf::st_layers(geopackage_file)

link_query <- "SELECT * FROM \"edges_rv\" LIMIT 10"
#link_query <- "SELECT id, direction, municipality, med_metrering, roadref_category, roadref_number, aadt_alle_mean, pred_aadt, pred_aadt_sd, aadt_prelim, aadt_prelim_sd, aadt_bal, aadt_bal_sd, aadt_bal_tot, geom FROM \"edges_rv\" WHERE (roadref_category = 'F' AND roadref_number = 44)"
#top_row_query <- "SELECT feature_oid, start_node_oid, end_node_oid FROM \"TrafikkLenker\" WHERE geometry IS NOT NULL LIMIT 15"
#link_query <- "SELECT * FROM \"TrafikkLenker\" WHERE start_node_oid IN(3283021)"

# Fetching all rows, but selected columns
link_query <- "SELECT id, direction, municipality, med_metrering, roadref_category, roadref_number, aadt_alle_mean, pred_aadt, pred_aadt_sd, aadt_prelim, aadt_prelim_sd, aadt_bal, aadt_bal_sd, aadt_bal_tot, geom FROM \"edges_rv\""

links_selected <-
  sf::st_read(
    geopackage_file,
    as_tibble = TRUE,
    query = link_query,
    quiet = TRUE
  )

# Those are directed, but to compare to manual AADTs they must de dedirected.
# Directed pairs share ID
# Keeping geometry for later use
unique_link_ids <-
  links_selected %>%
  dplyr::select(ID) %>%
  dplyr::distinct()

traffic_links_wide <-
  links_selected %>%
  dplyr::select(
    ID,
    roadcat = ROADREF_CATEGORY,
    roadn = ROADREF_NUMBER,
    with = med_metrering,
    aadt_bal,
    pred_aadt,
    #sd = pred_aadt_sd,
    trp_aadt = aadt_prelim,
    tm_aadt = AADT_ALLE_MEAN#,
    #nettaadt_mangler
  ) %>%
  sf::st_drop_geometry() %>%
  tidyr::pivot_wider(
    names_from = with,
    names_prefix = "med_",
    values_from = c(aadt_bal, pred_aadt, trp_aadt, tm_aadt)
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    aadt_bal = sum(aadt_bal_med_0, aadt_bal_med_1, na.rm = TRUE),
    pred_aadt = sum(pred_aadt_med_0, pred_aadt_med_1, na.rm = TRUE),
    trp_aadt = sum(trp_aadt_med_0, trp_aadt_med_1, na.rm = TRUE),
    tm_aadt = sum(tm_aadt_med_0, tm_aadt_med_1, na.rm = TRUE)
    #,
    #sd = dplyr::case_when(
    ## if one or both directions are missing
    #is.na(pred_aadt_med_1) || is.na(pred_aadt_med_0) ~
    #  sum(sd_med_1, sd_med_0, na.rm = TRUE),
    ## when both is present, combined sd
    #TRUE ~ sqrt((sd_med_1^2 + sd_med_0^2)/2 +
    #  ((pred_aadt_med_1 - pred_aadt_med_0)/2)^2)
    #)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(
        aadt_bal_med_1,
        aadt_bal_med_0,
        pred_aadt_med_1,
        pred_aadt_med_0,
        #sd_med_1,
        #sd_med_0,
        trp_aadt_med_1,
        trp_aadt_med_0,
        tm_aadt_med_1,
        tm_aadt_med_0,
        aadt_bal,
        pred_aadt,
        trp_aadt,
        tm_aadt
        #sd
      ),
      ~ round(.x)
    ),
    dplyr::across(
      .cols = c(
        trp_aadt,
        tm_aadt
      ),
      ~ dplyr::na_if(.x, 0)
    )
  ) %>%
  dplyr::select(
    ID,
    roadcat,
    roadn,
    aadt_bal,
    pred_aadt,
    trp_aadt,
    tm_aadt
  )

traffic_links_transformed <-
  unique_link_ids %>%
  dplyr::left_join(
    traffic_links_wide,
    by = "ID"
  ) %>%
  sf::st_zm(drop = T, what = "ZM") %>%
  dplyr::mutate(
    line_length =
      round(
        sf::st_length(geom),
        digits = 0)
  ) %>%
  dplyr::mutate(
    road_category_and_number =
      paste0(
        roadcat,
        " ",
        roadn
    ),
    link_label =
      paste(
        "Trafikkregistreringspunkt: ", trp_aadt, "<br/>",
        "Transportmodell: ", tm_aadt, "<br/>",
        "Modell, ikke-balansert: ", pred_aadt, "<br/>",
        "Modell, balansert: ", aadt_bal, "<br/>"
        #line_length, " m"
      ),
    # link_label =
    #   paste(
    #     "Modell-ÅDT ", aadt, "S: ", sd, "TM:", tm_aadt, "<br/>",
    #     "Med ", pred_aadt_med_1, "S: ", sd_med_1,
    #     "TM-ÅDT: ", tm_aadt_med_1,  "<br/>",
    #     "Mot ", pred_aadt_med_0, "S: ", sd_med_0,
    #     "TM-ÅDT: ", tm_aadt_med_0, "<br/>",
    #     line_length, " m"
    #   ),
    link_label = lapply(link_label, htmltools::HTML)
  ) %>%
  sf::st_transform("+proj=longlat +datum=WGS84")


traffic_links_transformed_stats <-
  traffic_links_transformed |>
  sf::st_drop_geometry() |>
  dplyr::group_by(
    road_category_and_number
  ) |>
  dplyr::summarise(
    n_links = n()
  )

chosen_roads <-
  traffic_links_transformed_stats |>
  dplyr::filter(
    n_links > 50
  )


# Manual AADTs ----
manual_aadts <-
  dplyr::bind_rows(
    get_aadt_by_county("11"),
    get_aadt_by_county("46")
  )

# Segmentation is false in order to get just complete links.
# Must remove duplicates (those crossing county borders)
manual_aadts_distinct <-
  manual_aadts |>
  dplyr::distinct(
    nvdb_objekt_id,
    .keep_all = TRUE
  ) |>
  dplyr::filter(
    road_category %in% c("E", "R", "F")
  ) |>
  dplyr::select(
    nvdb_objekt_id,
    road_category,
    road_category_and_number,
    intersection_part,
    year,
    aadt_total,
    heavy_percentage,
    source
  )


# Filter by road category before spatial join
# manual_aadts_erf <-
#   manual_aadts |>
#   dplyr::filter(
#     road_category %in% c("E", "R", "F")
#   )
#
# traffic_links_transformed_erf <-
#   traffic_links_transformed |>
#   dplyr::filter(
#     roadcat %in% c("E", "R", "F")
#   )

manual_aadts_stats <-
  manual_aadts |>
  sf::st_drop_geometry() |>
  dplyr::group_by(
    road_category_and_number
  ) |>
  dplyr::summarise(
    n_links = n()
  )



# Compare ----
# Include just a few rows at a time in the spatial join
# to avoid it taking too long

compare_model_and_manual_aadts <- function(road_input) {

  chosen_manual_aadts <-
    manual_aadts_distinct |>
    dplyr::filter(
      road_category_and_number %in% road_input
    )

  chosen_traffic_links_transformed <-
    traffic_links_transformed |>
    dplyr::filter(
      road_category_and_number %in% road_input
    ) |>
    dplyr::select(
      ID,
      aadt_bal,
      pred_aadt,
      trp_aadt,
      tm_aadt,
      line_length,
      link_label
    )

  model_and_manual_aadts <-
    sf::st_join(
      chosen_traffic_links_transformed,
      chosen_manual_aadts,
      largest = TRUE
    )

  return(model_and_manual_aadts)
}


model_and_manual_aadt_results <-
  dplyr::bind_rows(
    compare_model_and_manual_aadts(c("F 47", "F 511")),
    compare_model_and_manual_aadts(c("F 4828", "F 4824", "F 4826", "F 4838")),
    compare_model_and_manual_aadts(c("F 4836", "F 4832", "F 4840", "F 4830")),
    compare_model_and_manual_aadts(c("F 4844", "F 4846", "F 4842", "F 4802")),
    compare_model_and_manual_aadts(c("E 134")),
    compare_model_and_manual_aadts(c("F 4848")),
    compare_model_and_manual_aadts(c("F 547")),
    compare_model_and_manual_aadts(c("F 553")),
    compare_model_and_manual_aadts(c("F 554"))
  )

# missing_link <-
#   chosen_manual_aadts |>
#   dplyr::filter(
#     !(nvdb_objekt_id %in% model_and_manual_aadts$nvdb_objekt_id)
#   )

# test <-
#   model_and_manual_aadts |>
#   dplyr::filter(
#     nvdb_objekt_id == 1014994160
#   )

model_and_manual_aadts_final <-
  model_and_manual_aadt_results %>%
  #test |>
  dplyr::mutate(
    aadt_diff =
      round((aadt_bal - aadt_total) / aadt_total * 100, digits = 0),
    aadt_diff_abs =
      aadt_bal - aadt_total,
    link_label_final =
      paste0(
        link_label,
        #"<br/>",
        "NVDB: ", aadt_total, " Kilde: ", source,
        "<br/>",
        "Absolutt avvik: ", aadt_diff_abs
      ),
    link_label_final =
      lapply(link_label_final, htmltools::HTML)
  ) %>%
  dplyr::select(-link_label) |>
  dplyr::filter(
    !(is.na(nvdb_objekt_id))
  )

base::saveRDS(
  model_and_manual_aadts_final,
  file = "compare_rv.rds"
)

test <- model_comparisons |> dplyr::filter(nvdb_objekt_id == 1015074078)
test_2 <-
  traffic_links_transformed |>
  dplyr::filter(ID == 1014434222)
