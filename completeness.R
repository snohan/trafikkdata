#
library(tidyverse)
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")

# TRS commissions ----
# source("H:/Programmering/R/byindeks/get_from_trp_api.R")
#
# commissions <- get_trs_commissions()
#
# readr::write_rds(
#   commissions,
#   file = "commissions.rds"
# )

commissions <- readr::read_rds("commissions.rds")

commissions_of_interest <-
  commissions |>
  dplyr::filter(
    system_name == "TRAFIKKDATA",
    traffic_type == "VEHICLE"
  ) |>
  dplyr::select(
    trs_id,
    trs_name,
    registration_frequency = station_type,
    county_name,
    municipality_name,
    commission_from,
    commission_to,
    trp_id,
    trp_name
  )


# Completeness ----
# Read CSV from Kibana with current timestamp of completeness calculation
# Taken from ES index "completeness", timestamps as of time of ES query

completeness <-
  readr::read_delim(
    "completeness.csv",
    delim = ","
  ) |>
  dplyr::select(
    trs_id = measure_point_number,
    completeness_timestamp = checkpoint_timestamp
  ) |>
  dplyr::mutate(
    trs_id = as.character(trs_id),
    completeness_timestamp = lubridate::dmy_hms(completeness_timestamp, tz = "CET")
  )

latest_completeness_timestamp <- base::max(completeness$completeness_timestamp)


# Commissions and completeness ----
commissions_and_completeness <-
  commissions_of_interest |>
  dplyr::left_join(
    completeness,
    by = "trs_id"
  ) |>
  dplyr::filter(
    # TRS with only dummy commissions
    !is.na(completeness_timestamp)
  ) |>
  dplyr::mutate(
    # Open commissions viewed as lasting until time of completeness CSV download
    commission_to = dplyr::if_else(is.na(commission_to), latest_completeness_timestamp, commission_to),
    lagging_completeness = completeness_timestamp < commission_to,
    trs_id = as.numeric(trs_id)
  ) |>
  dplyr::filter(
    lagging_completeness == TRUE,
    registration_frequency == "PERIODIC"
  ) |>
  dplyr::arrange(
    trs_id
  ) |>
  dplyr::distinct() |>
  dplyr::select(
    trs_id,
    trs_name,
    trp_id,
    commission_from,
    commission_to,
    completeness_timestamp
  )


# Filter ones with manual markings covering a no-data commission tail.
manual_labels <-
  base::unique(commissions_and_completeness$trp_id) |>
  get_labels_for_trp_list() |>
  dplyr::select(-lane) |>
  dplyr::distinct()

#length(base::unique(commissions_and_completeness$trs_id))

trs_id_start_of_first_commission_of_interest <-
  commissions_and_completeness |>
  dplyr::group_by(
    trs_id
  ) |>
  dplyr::slice_min(
    commission_from
  ) |>
  dplyr::select(
    trs_id,
    earliest_commission_from = commission_from
  ) |>
  dplyr::distinct()


commissions_and_completeness_and_labels <-
  commissions_and_completeness |>
  dplyr::left_join(
    trs_id_start_of_first_commission_of_interest,
    by = "trs_id"
  ) |>
  dplyr::left_join(
    manual_labels,
    by = "trp_id",
    multiple = "all"
    #dplyr::join_by(
    #  trp_id,
      #earliest_commission_from <= label_start
      #commission_from <= label_start
      #dplyr::between(completeness_timestamp, label_start, label_end)
    #)
  ) |>
  dplyr::select(
    trs_id,
    trs_name,
    commission_from,
    commission_to,
    completeness_timestamp,
    label_interval = date_interval#,
    #label_end
  ) |>
  dplyr::mutate(
    completeness_timestamp_covered_by_label = completeness_timestamp %within% label_interval#,
    #completeness_timestamp_after_label = completeness_timestamp > label_end
  )

trs_with_completeness_timestamp_covered_by_label <-
  commissions_and_completeness_and_labels |>
  dplyr::filter(
    completeness_timestamp_covered_by_label == TRUE #%in% c(FALSE, NA)
  )

possible_periods_missing_completeness <-
  commissions_and_completeness_and_labels |>
  dplyr::filter(
    !(trs_id %in% base::unique(trs_with_completeness_timestamp_covered_by_label$trs_id))
  ) |>
  dplyr::mutate(
    completeness_before_commission = completeness_timestamp <= commission_from,
    commission_zero_duration = commission_from == commission_to
  ) |>
  dplyr::filter(
    completeness_before_commission == TRUE,
    commission_zero_duration == FALSE,
    commission_to < latest_completeness_timestamp
  ) |>
  dplyr::select(
    trs_id,
    trs_name,
    commission_from,
    commission_to,
    completeness_timestamp
  ) |>
  dplyr::distinct()



# Splunk
# (Busboy* OR completeness) AND 1600017


# Case Ringebu nord ----
# Siste logginnslag i Splunk om completeness:
# "Created new completeness progress timestamp at 2022-03-24T09:00:00.000Z for 501576
# Tidspunktet sammenfaller med starten på forrige commission.
# Merkelig at det ikke er noen logginnslag om at completeness flytter seg framover. Den står jo nå på 1.feb.23, som
# er midt i siste commission, så den må ha flyttet seg...
# Jeg tenkte å se når completeness hadde kjørt. Mistenker at den begynte med noe fra mars/april 2022 da den ble
# igangsatt 24.jan.23.

# Case 500302 Ringebu sør ----
# Ruter ble koblet på 6.58 8. februar. Ryddegutt begynner å hente gamle data kl. 6.59.
# Den første timen den henter for er 2023-01-10T07:00:00.000Z/2023-01-10T08:00:00.000Z.
# Her finner den 464 eventer.
# Ryddegutt er a jour kl. 19.33 samme dag.
# 10. januar er ikke del av noen commission for denne stasjonen. Men Sannom sør ble igangsatt 10. januar, og det er
# denne stasjonen denne dataloggeren sto på før Ringebu sør.
# De første eventene ryddegutt henter lagres på Sannom sør kl. 6.59.11 8. feb.
# MAC-identifier: 6EBC
# Ryddegutt henter kl. 12.58 8. feb. den første timen som har data tilhørende Ringebu sør (24.jan kl. 9-10).
# En skulle tro at Splunk viste spor av fulltallighetsberegning i samme tidsrom , men det finner jeg ikke.

# Konklusjon så langt ----
# Konklusjonen er at siste stasjon i en måleserie må stå tilkoblet ruter helt til ogsp fulltallighet er ajour,
# men det kan jo ta sin tid.
# Fulltallighet burde få ta igjen ryddegutt selv etter at siste stasjon er satt ut av drift. Periodiske stasjoner
# må ikke få slettet sitt ryddeguttidsstempel.