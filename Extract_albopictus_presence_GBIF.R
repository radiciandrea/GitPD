# code to extract Data from GBIF

# https://www.gbif.org/species/1651430

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

name = "W_EU"

folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GBIF/"

# data = read.csv2(paste0(folder_in , "aegypti_albopictus.csv"), sep = ",")
data = read.csv2(paste0(folder_in , "albopictus_presence_GBIF.csv"), sep = "")

data$lat<-as.numeric(data$decimalLatitude)
data$lon<-as.numeric(data$decimalLongitude)

#load sf domain
domain <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

lat_top <- max(domain$top)
lat_bot <- min(domain$bottom)
lon_right <- max(domain$right)
lon_left <- min(domain$left)

#remove obs outside; #remove other vector # keep only points
data_sel <- data %>%
  filter(lon< lon_right) %>%
  filter(lon>lon_left) %>%
  filter(lat<lat_top) %>%
  filter(lat>lat_bot) 

data_Albo <- data 

# WGS1984 Datum
# https://epsg.io/4326
Albo_W_EU <- st_as_sf(x = data_sel,
                      coords = c("lon", "lat"),
                      crs = 4326)

Albo <- st_as_sf(x = data_Albo,
                 coords = c("lon", "lat"),
                 crs = 4326)

st_write(Albo_W_EU, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_Albo_W_EU.shp")

st_write(Albo, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_Albo.shp")
