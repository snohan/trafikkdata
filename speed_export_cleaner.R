base::Sys.setlocale(locale = "nb.utf8")
library(tidyverse)

decimal_point <- function(number) {
  stringr::str_replace(as.character(number), ",", "\\.")
}

# Raw files to be fetched from speed export in trafikkdata.no
# Tidy files shared in Dropbox
# TRP list made in trs_trp.R (TRPs Oslo)
subfolder_raw <- "spesialbestillinger/saraca"
subfolder_tidy <- "spesialbestillinger/saraca_tidy"

raw_files <- list.files(subfolder_raw)
tidied_files <- list.files(subfolder_tidy)

exported_trps <-
  tibble::tibble(
    file_name = raw_files
  ) |>
  dplyr::mutate(
    trp_id = stringr::str_extract(file_name, "^[:alnum:]*_") |> stringr::str_remove("_")
  )

# Missing exports
trps_saraca <-
  readr::write_rds(
    trp_oslo_final,
    "spesialbestillinger/saraca_trps.rds"
  ) |>
  dplyr::filter(
    !(trp_id %in% exported_trps$trp_id)
  )

left_to_tidy <- raw_files[!(raw_files %in% tidied_files)]


read_tidy_and_write_file <- function(file_name) {

  readr::read_csv2(
    paste0(subfolder_raw, "/", file_name),
    col_select = c(1:4, 7, 10, 12, 14:16)
  ) |>
  dplyr::select(
    trp_id = Trafikkregistreringspunkt,
    #trp_name = Navn,
    #road_reference = Vegreferanse,
    lane = Felt,
    date = Dato,
    hour_start = 'Fra tidspunkt',
    traffic_volume = Trafikkmengde,
    coverage = Dekningsgrad,
    mean_speed = Gjennomsnittshastighet,
    percentile_85 = '85-fraktil'
  ) |>
  dplyr::mutate(
    mean_speed = decimal_point(mean_speed) |> as.numeric(),
    percentile_85 = decimal_point(percentile_85) |> as.numeric(),
    mean_speed =
      dplyr::case_when(
        traffic_volume < 6 ~ NA_real_,
        TRUE ~ mean_speed
      ),
    percentile_85 =
      dplyr::case_when(
        traffic_volume < 6 ~ NA_real_,
        TRUE ~ percentile_85
      )
  ) |>
  readr::write_csv2(
    paste0(subfolder_tidy, "/", file_name)
  )

}


for (i in 1:length(left_to_tidy)) {

  read_tidy_and_write_file(left_to_tidy[i])

}
