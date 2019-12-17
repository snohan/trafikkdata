# Read Datex2 xml publication from traffic data

library(tidyverse)
library(xml2)

xml_data <- xml2::read_xml("datex2-no_sideName.xml")
