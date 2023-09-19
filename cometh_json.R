library(tidyverse)
library(writexl)

cometh_files <-
  base::list.files(
    path = "spesialbestillinger", # navnet på mappa filene ligger i
    pattern = "COM.*.json", # finner alle filer som begynner på COM og slutter på .json
    full.names = TRUE
  )

# Velger ut en av filene
cometh_all_data <-
  jsonlite::fromJSON(cometh_files[1])

cometh_traffic_volumes <-
  cometh_all_data$countdata$counts |>
  dplyr::mutate(
    date_time = lubridate::as_datetime(t, tz = "CET")
  ) |>
  dplyr::select(
    date_time,
    lane = lId,
    slId, # noe med retning?
    sId, # ?
    direction = dir,
    length = len,
    class = pra,
    speed = spe
  )

# Skrive ut til Excel
writexl::write_xlsx(
  cometh_traffic_volumes,
  path = "spesialbestillinger/cometh_data.xlsx"
)
