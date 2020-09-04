# Read traffic links from geopackage file


# Packages ####
library(tidyverse)
library(sf)
library(DBI)
library(RSQLite)


# Read ####
geopackage_file <- "trafikklenker_20200903184437.gpkg"

# Layers in gpkg file
geopackage_layers <- sf::st_layers(geopackage_file)


# Exploring the layers ####
# Query string
top_query_trafikklenker <- paste0("SELECT * ",
                                  "FROM \'TrafikkLenker\' ",
                                  "WHERE geometry IS NOT NULL ",
                                  "LIMIT 15")

# Fetching from geopackage file
trafikklenker_top <- sf::st_read(geopackage_file, "TrafikkLenker",
                                 as_tibble = TRUE,
                                 query = top_query_trafikklenker)


# Or to find traffic links with missing geometry
trafikklenker_null <- "SELECT feature_oid FROM \"TrafikkLenker\" WHERE geometry IS NULL"

trafikklenker_top <- sf::st_read(geopackage_file, "TrafikkLenker",
                                 as_tibble = TRUE,
                                 query = trafikklenker_null)

# Kryss
top_row_query_kryss <- "SELECT * FROM \"Kryss\" LIMIT 100"

kryss_top <- sf::st_read(geopackage_file, "Kryss",
                         as_tibble = TRUE,
                         query = top_row_query_kryss)

# Stedfestinger
top_query_stedfestinger <- "SELECT * FROM \"Trafikklenker_stedfesting\" LIMIT 15"

stedfesting_top <- sf::st_read(geopackage_file, "Trafikklenker_stedfesting",
                                      as_tibble = TRUE,
                                      query = top_query_stedfestinger)


# Looking at larger selection ####

# Trafikklenker med funksjonell vegklasse 1 (større riksveger)
trafikklenker_selection <- "SELECT * FROM \"TrafikkLenker\" WHERE geometry IS NOT NULL AND min_funksjonell_vegklasse IS 1 LIMIT 5000"

trafikklenker <- sf::st_read(geopackage_file, "TrafikkLenker",
                             as_tibble = TRUE,
                             query = trafikklenker_selection)

# Tilhørende kryss
unique_start_node_ids <- trafikklenker$START_NODE_OID %>%
  unique()
unique_end_node_ids <- trafikklenker$END_NODE_OID %>%
  unique()

unique_node_ids <- c(unique_start_node_ids, unique_end_node_ids) %>%
  unique() %>%
  stringr::str_c(collapse = "\', \'")

kryss_query = paste0("SELECT * FROM \"Kryss\" WHERE FEATURE_OID IN (",
                     "\'", unique_node_ids, "\'", ")")

kryss <- sf::st_read(geopackage_file, "Kryss",
                     as_tibble = TRUE,
                     query = kryss_query)