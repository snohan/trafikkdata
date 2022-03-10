# Input: vbv data
# output: vbv data with Vehicle Registry data

source("get_from_vehicle_registry.R")

library(readxl)
library(writexl)


# Read vbv data ----
vbv_file <-
  readxl::read_xlsx(
    "vbv_vehicle_registry/2022-02-21_09-10.xlsx"
  )

vbv_clean <-
  vbv_file %>%
  dplyr::filter(
    RegNr != "Ukjent"
  ) %>%
  dplyr::select(RegNr) %>%
  dplyr::distinct()

vehicle_registry_data <-
  vbv_clean$RegNr %>%
  get_vehicle_data_for_list()


vbv_enhanced <-
  vbv_file %>%
  dplyr::left_join(
    vehicle_registry_data,
    by = c("RegNr" = "vehicle_id")
  )

writexl::write_xlsx(
  vbv_enhanced,
  "vbv_vehicle_registry/2022-02-21_09-10_autosys.xlsx"
)
