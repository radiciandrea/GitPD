# code to extract Data from "The global compendium of Aedes aegypti and Ae. albopictus occurrence"

# https://datadryad.org/stash/dataset/doi:10.5061/dryad.47v3c

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

name = "W_EU"

folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Kraemer2015/"

data = read.csv2(paste0(folder_in , "aegypti_albopictus.csv"), sep = ",")

#load sf domain
domain <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

domain <- domain %>%
  arrange(region)

# in cycle 

lat_top <- domain$top
lat_bot <- domain$bottom
lon_right <- domain$right
lon_left <- domain$left

#remove obs outside 
data_geo_sel <- data_geo %>%
  filter(longitude<max(lon_right))%>%
  filter(longitude>min(lon_left))%>%
  filter(latitude<max(lat_top))%>%
  filter(latitude>min(lat_bot))

data_sel <- data %>%
  filter(VECTOR == "Aedes albopictus")
