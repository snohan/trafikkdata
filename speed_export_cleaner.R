base::Sys.setlocale(locale = "nb.utf8")
library(tidyverse)

decimal_point <- function(number) {
  stringr::str_replace(as.character(number), ",", "\\.")
}

subfolder_raw <- "spesialbestillinger/saraca"
subfolder_tidy <- "spesialbestillinger/saraca_tidy"

list_of_files <- list.files(subfolder_raw)

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


for (i in 1:length(list_of_files)) {

  read_tidy_and_write_file(list_of_files[i])

}
