# Get data from data.norge.no
# Kjøretøyopplysninger

get_vehicle_groups <- function() {
  # Kjøretøygrupper
  response <-
    readr::read_csv2("https://hotell.difi.no/api/csv/vegvesen/kjoretoygruppe?")
}

get_vehicle_type_approvals <- function() {
  # Typegodkjenninger
  # Kan ikke begrense på kolonner (felt), så må ta alt
  # Kan ikke begrense på mange verdier på et felt, tar derfor alle og filtrer
  # etterpå
  response <-
    readr::read_csv2("https://hotell.difi.no/download/vegvesen/typg") %>%
    dplyr::select(antall_aksler,
                  egenvekt,
                  kjoretoygruppe,
                  typegodkjenningsnr) %>%
    # No trailers
    dplyr::filter(kjoretoygruppe < 630) %>%
    # No "beltebil"
    dplyr::filter(kjoretoygruppe != 350)

}

