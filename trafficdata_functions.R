#

# vbv ----

make_norsikt_classes <- function(vbv_raw_df) {

  # Works for EMU3 and Loop Monitor
  # input must have the column 'vehicle_type_raw'

  vbv_classified <- vbv_raw_df %>%
    dplyr::mutate(norsikt_l2 =
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
