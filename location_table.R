# Location names from .dat to .xlsx
library(tidyverse)
library(writexl)

# Relevant tables from zip-file:
# Names - all kinds of names (area, place)
# Segments - larger roads broken down to shorter segments
# Soffsets - to get the segments in correct order
# POFFSETS - ordering location points
# Points - coordinates of names at points
# Roads - road sections with names at each end

# name_translations <-
#   readr::read_delim(
#     "location_table/NAMETRANSLATIONS.DAT",
#     col_types = cols(.default = "c")
#   )

names_raw <-
  readr::read_delim(
    "location_table/NAMES.DAT",
    col_types = cols(.default = "c"),
    col_select = c(NID, NAME)
  )

admin_areas_raw <-
  readr::read_delim(
    "location_table/ADMINISTRATIVEAREA.DAT",
    col_types = cols(.default = "c"),
    col_select = c(LCD, NID, POL_LCD)
  )


admin_areas_names <-
  admin_areas_raw |>
  dplyr::left_join(
    names_raw,
    by = join_by(NID)
  ) |>
  dplyr::left_join(
    admin_areas_raw,
    suffix = c("", "_pol"),
    by = join_by(POL_LCD == LCD)
  ) |>
  dplyr::left_join(
    names_raw,
    suffix = c("", "_pol"),
    by = join_by(NID_pol == NID)
  ) |>
  dplyr::select(
    POL_LCD,
    area = NAME_pol
  ) |>
  dplyr::distinct()


soffsets_raw <-
  readr::read_delim(
    "location_table/sOFFSETS.DAT",
    col_types = cols(.default = "c"),
    col_select = c(LCD, NEG_OFF_LCD, POS_OFF_LCD)
  )

segments_raw <-
  readr::read_delim(
    "location_table/SEGMENTS.DAT",
    col_types = cols(.default = "c"),
    col_select =
      c(
        LCD,
        TCD,
        STCD,
        ROADNUMBER,
        N1ID,
        N2ID,
        ROA_LCD
        )
  ) |>
  dplyr::right_join(
    soffsets_raw,
    by = join_by(LCD)
  )

segments_sort <-
  segments_raw |>
  dplyr::select(
    ROA_LCD,
    NEG_OFF_LCD,
    LCD,
    POS_OFF_LCD
  )

# Sorting by offsets
sort_by_offset <- function(unsorted_df) {

  # Build tibble by row
  sorted_df <-
    bind_rows(
      unsorted_df |>
          dplyr::filter(is.na(NEG_OFF_LCD))
    )

  if(!is.na(sorted_df$POS_OFF_LCD)) {

    for(r in 2:nrow(unsorted_df)) {

      next_lcd <-
        sorted_df |>
        slice_tail(n = 1) |>
        pluck("POS_OFF_LCD")

      sorted_df <-
        sorted_df |>
        bind_rows(
          unsorted_df |>
            dplyr::filter(LCD == next_lcd)
        )

    }
  }

  sorted_df_tidy <-
    sorted_df |>
    tibble::rowid_to_column("group_sort")# |>
    #dplyr::select(-ROA_LCD)

  return(sorted_df_tidy)
}

# test_tidy <-
#   segments_sort |>
#   dplyr::filter(
#     ROA_LCD == 482
#   ) |>
#   sort_by_offset()

segments_sorted <-
  segments_sort |>
  dplyr::group_by(ROA_LCD) |>
  dplyr::group_modify(~ sort_by_offset(.x)) |>
  dplyr::ungroup()

# intersections_raw <-
#   readr::read_delim(
#     "location_table/INTERSECTIONS.DAT"
#   )

# points_raw <-
#   readr::read_delim(
#     "location_table/POINTS.DAT"
#   )

# poffsets_raw <-
#   readr::read_delim(
#     "location_table/POFFSETS.DAT"
#   )

roads_raw <-
  readr::read_delim(
    "location_table/ROADS.DAT",
    col_types = cols(.default = "c"),
    col_select =
      c(
        LCD,
        TCD,
        STCD,
        ROADNUMBER,
        #RNID,
        N1ID,
        N2ID,
        POL_LCD
      )
  )

#summary(segments_raw)
#unique(segments_raw$ROA_LCD)

#
segments <-
  segments_sorted |>
  dplyr::select(
    LCD,
    group_sort
  ) |>
  dplyr::left_join(
    segments_raw,
    by = join_by(LCD)
  ) |>
  dplyr::left_join(
    names_raw,
    by = join_by(N1ID == NID)
  ) |>
  dplyr::left_join(
    names_raw,
    suffix = c("_1", "_2"),
    by = join_by(N2ID == NID)
  ) |>
  # Remove singular segments - they will only be duplicates
  dplyr::add_count(
    ROA_LCD
  ) |>
  dplyr::filter(
    n > 1
  )

roads <-
  roads_raw |>
  dplyr::left_join(
    names_raw,
    by = join_by(N1ID == NID)
  ) |>
  dplyr::left_join(
    names_raw,
    suffix = c("_1", "_2"),
    by = join_by(N2ID == NID)
  ) |>
  dplyr::left_join(
    admin_areas_names,
    by = join_by(POL_LCD)
  ) |>
  # Remove roads that have more fine grained segments
  dplyr::filter(
    !(LCD %in% segments$ROA_LCD)
  ) |>
  dplyr::add_count(
    ROADNUMBER
  )

roads_and_names <-
  dplyr::bind_rows(
    segments,
    roads
  ) |>
  dplyr::mutate(
    vegkategori = stringr::str_sub(ROADNUMBER, 1, 1),
    vegnummer = stringr::str_sub(ROADNUMBER, 4, -1) |>
      stringr::str_remove("-.+") |>
      as.numeric(),
    segments = LCD %in% segments$ROA_LCD
  ) |>
  dplyr::select(
    fylke = area,
    vegkategori,
    vegnummer,
    veg = ROADNUMBER,
    navn_1 = NAME_1,
    navn_2 = NAME_2,
    TCD,
    STCD,
    segments
  ) #|>
  #dplyr::arrange(
  #  vegkategori, vegnummer
  #)
  # NB! Sorting here would jumble up segments!

roads_and_names |>
  dplyr::select(
    fylke,
    vegkategori,
    vegnummer,
    veg,
    navn_1,
    navn_2
  ) |>
writexl::write_xlsx(
  "spesialbestillinger/lokasjonstabellen.xlsx"
)
