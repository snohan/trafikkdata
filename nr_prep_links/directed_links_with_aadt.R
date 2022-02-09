# Create a geopackage-file containing directed links with aadt

# NRs framgangsmåte ----
# Lag vegnettsgraf, steg 1 i ÅDT-modulen
#
# 1. Først isoleres hovedkomponenten i vegnettet gjennom en nettverksanalyse på
#    samlingen av trafikklenker og kryss (NR-kode).
#
# 2. Deretter retningsdekomponeres lenkene i hovedkomponenten (TØI-kode). Dette
#    gir en sammenhengende vegnettsgraf med retningsoppløsning.
#
# 3. Fra vegnettsgrafen beregnes sentralitetsmålet betweenness centrality til bruk i den
#    statistiske modellen (NR-kode). Sentralitetsverdier tilordnes hver enkelt lenke i
#    den rettede grafen.
#
# 4. Preliminær ÅDT fra tilgjengelige trafikkregistreringspunkter (kontinuerlige og
#    periodiske) kobles på sine respektive rettede lenker der hvor slike punkter finnes
#    (Knowit-kode).
#
# 5. For å få med nettutlagt ÅDT fra transportmodellkjøringen for 2018, må det dessuten
#    opprettes en kobling mellom den nye vegnettsgrafen og lenkene i vegnettsgrafen
#    fra det inneværende prosjektet (ny kode).
#    Senere versjoner kan benytte nettutlagt ÅDT fra denne første koblingen.
#
# 6. Den nye vegnettsgrafen med tilhørende attributter sammenstilles til en GeoPackagefil.


# Problemer Norge ----
#
# 1. TØI laget en liste med lenker som skulle fjernes. ID-ene er endret, så disse kan ikke
#    fjernes slik de er angitt nå. Er dette fortsatt nødvendig?
#    Finnes det andre lenker som burde plukkes vekk?
#
# 2. Grafen for hele Norge er veldig usammenhengende med sine over 2 000 komponenter.
#    Vi burde sjekke alle løse vegnettsdeler - skulle de vært en del av hovedkomponenten?
#    Det er tre komponenter som må kobles på.
#
# 3. Transportmodell-ÅDT er bare koblet på for Region Vest tidligere.
#    Nå skulle vi gjort det for hele landet.


# Tilpasset framgangsmåte Region vest ----
#
# 1. Bruk Trionas gpkg-fil for hele Norge og isoler hovedkomponenten.
#    Hekt på deler som skal være med.
#
#      Kode hentet fra:
#      prepros_generer_urettet_hovedkomponent.R
#
# 2. Begrens området til fylkene Rogaland og Vestland.
#
#      Kode hentet fra:
#      2_datasett_region_vest.R
#
# 3. Lag rettet graf.
#
# 4. Koble på ÅDT fra transportmodell fra graf_dir_maincom_20210505.gpkg.
#
# 5. Koble på ÅDT fra Trafikkdata-API.
#
# 6. Beregn sentralitetsparametere.
#
# 7. Skriv til ny GPKG-fil.


# Setup ----
library(tidyverse)
library(sf)
library(sfnetworks)
library(igraph)
library(tidygraph)
library(ggplot2)
library(DBI)
library(RSQLite)
library(tmap)

source("nr_prep_links/nr/prepros_manipuler_trafikklenker_hardkodet.r")

## The Triona file
geopackage_file <-
  'C:/Users/snohan/Desktop/Trafikklenker_20220116.gpkg'

# Trafikklenker som skal fjernes iht til TØIs kartlegging
file_remove_links_toi <-
  'nr_prep_links/nr/Data/ID_trafikklenker_fjernes.csv'


# Read ----
geopackage_layers <- sf::st_layers(geopackage_file)

edges <-
  sf::st_read(
    geopackage_file,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"TrafikkLenker\""
  ) %>%
  sf::st_zm()

nodes <-
  sf::st_read(
    geopackage_file,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"Kryss\""
  ) %>%
  sf::st_zm()

road_net_info <-
  sf::st_read(
    geopackage_file,
    layer = 'Trafikklenker_stedfesting'
  )


# Clean ----
## Targeted data cleansing of individual traffic links and nodes
links_remove_toi <-
  read.csv(
    file_remove_links_toi,
    stringsAsFactors = FALSE
  )

# The link IDs have changed in the original Triona file, so this won't have any effect
# TODO: are these links still a problem, and are there others we should discover?
# How can we remove problematic links automatically?

data_cleansed <-
  manipuler_trafikklenker_hardkodet(
    nn = nodes,
    ee = edges,
    links_remove = links_remove_toi
  )

nodes <- data_cleansed$nodes
edges <- data_cleansed$edges


# Build undirected graph ----

## First, add indices for start and end nodes of each edge wrt nodes object
edges$from <-
  base::match(
    edges$START_NODE_OID,
    nodes$FEATURE_OID
  )

edges$to <-
  base::match(
    edges$END_NODE_OID,
    nodes$FEATURE_OID
  )

## Build undirected graph object from nodes and spatially implicit edges (no geometry)
net_undir <-
  sfnetworks::sfnetwork(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    node_key = "FEATURE_OID",
    edges_as_lines = FALSE,
    force = FALSE
  )


# Graph components ----
## Decompose graph and consider the distribution of nodes among the various components

## Identify connected components
net_undir %>%
  igraph::count_components(mode="weak")

cc <-
  net_undir %>%
  igraph::components(mode="weak")

# Look at component size distribution
rev(table(cc$csize))

# Isolate main vs and all other
net_main <-
  igraph::induced_subgraph(
    net_undir,
    igraph::V(net_undir)[components(net_undir)$membership ==
                              which.max(components(net_undir)$csize)]) %>%
  sfnetworks::as_sfnetwork()

net_all_but_main <-
  igraph::induced_subgraph(
    net_undir,
    igraph::V(net_undir)[components(net_undir)$membership !=
                              which.max(components(net_undir)$csize)]) %>%
  sfnetworks::as_sfnetwork()


# Add geometry ----
# Extract nodes and edges from components, and add geometries.


## Main ----
nodes_main <- net_main %>%
  sfnetworks::activate("nodes") %>%
  st_as_sf()

edges_main <- net_main %>%
  sfnetworks::activate("edges") %>%
  as_tibble()

# Transfer geometries from original edges based on matching IDs
idx <-
  base::match(
    as.character(edges_main$ID),
    as.character(edges$ID)
  )

edges_main <-
  sf::st_set_geometry(
    edges_main,
    edges$GEOMETRY[idx]
  )

# Road net info
road_net_info_main <-
  road_net_info %>%
  dplyr::filter(
    FEATURE_OID %in% edges_main$ID
  )


## All but main  ----
nodes_all_but_main <-
  net_all_but_main %>%
  sfnetworks::activate("nodes") %>%
  sf::st_as_sf()

edges_all_but_main <-
  net_all_but_main %>%
  sfnetworks::activate("edges") %>%
  tibble::as_tibble()

# Transfer geometries from original edges object based on matching IDs
idx_2 <-
  base::match(
    as.character(edges_all_but_main$ID),
    as.character(edges$ID)
  )

edges_all_but_main <-
  sf::st_set_geometry(
    edges_all_but_main,
    edges$GEOMETRY[idx_2]
  )

# No need for road net info for these


# Write ----
## Main ----
file_main <-
  'nr_gpkg/trafikklenker_undir_norge_2.gpkg'

sf::st_write(
  nodes_main,
  dsn = file_main,
  layer = 'nodes_main'
)

sf::st_write(
  edges_main,
  dsn = file_main,
  layer = 'edges_main'
)

sf::st_write(
  road_net_info_main,
  dsn = file_main,
  layer = 'road_net_info'
)


## All but main ----
file_all_but_main <-
  'nr_gpkg/trafikklenker_undir_norge_all_but_main_2.gpkg'

sf::st_write(
  nodes_all_but_main,
  dsn = file_all_but_main,
  layer = 'nodes_all_but_main'
)

sf::st_write(
  edges_all_but_main,
  dsn = file_all_but_main,
  layer = 'edges_all_but_main'
)

# Do not need road net info for these


# Region vest ----
# Polygoner for Vestland og Rogaland fylker er hentet herfra:
# https://kartkatalog.geonorge.no/metadata/administrative-enheter-kommuner/041f1e6e-bdbc-4091-b48f-8a5990f3cc5b

path_vestland <-
  'nr_prep_links/Basisdata_46_Vestland_25833_Kommuner_GEOJSON.geojson'

path_rogaland <-
  'nr_prep_links/Basisdata_11_Rogaland_25833_Kommuner_GEOJSON.geojson'

sf::st_layers(path_vestland)

vestland <-
  sf::st_read(
    path_vestland,
    layer = 'administrative_enheter.kommune'
  )

rogaland <-
  sf::st_read(
    path_rogaland,
    layer = 'administrative_enheter.kommune'
  )

region_vest <-
  dplyr::bind_rows(vestland, rogaland)

region_vest <-
  region_vest %>%
  sf::st_union()

# Overlapping edges
ind <-
  sf::st_intersects(
    edges_main,
    region_vest
  )

ind_rv <-
  lengths(ind) == 1

edges_rv <- edges_main[ind_rv, ]

# Road net info
road_net_info_rv <-
  road_net_info_main %>%
  dplyr::filter(
    FEATURE_OID %in% edges_rv$ID
  )

# Nodes
# nodes_rv_covered_or_not <-
#   sf::st_covered_by(
#     nodes_main,
#     region_vest
#   )
#
# nodes_rv_covered <-
#   which(lengths(nodes_rv_covered_or_not) == 1)

nodes_rv_connected_ids <-
  unique(
    c(
      edges_rv$START_NODE_OID,
      edges_rv$END_NODE_OID
    )
  )

nodes_rv_connected <-
  which(
    nodes_main$FEATURE_OID %in% nodes_rv_connected_ids)

nodes_rv <- nodes_main[nodes_rv_connected, ]


## Write ----
# TODO: functonize write to gpkg
file_rv <-
  'nr_gpkg/trafikklenker_undir_rv.gpkg'

sf::st_write(
  nodes_rv,
  dsn = file_rv,
  layer = 'nodes_rv'
)

sf::st_write(
  edges_rv,
  dsn = file_rv,
  layer = 'edges_rv'
)

sf::st_write(
  road_net_info_rv,
  dsn = file_rv,
  layer = 'road_net_info'
)









