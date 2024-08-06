# code to create pseudoabsences from GBIF

# https://www.gbif.org/species/1651430
# I do modify and I look for Culex pipiens
# https://www.gbif.org/species/1652991

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

name = "W_EU"

folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GBIF/"

#coordinats are weak: download from gbif website - preprocessing by hand (text to column; coordinates to number; set "," as decimal separator.
data = read.csv2(paste0(folder_in , "download_GBIF_Culex_mod.csv"))

data$lat<-as.numeric(data$decimalLatitude)
data$lon<-as.numeric(data$decimalLongitude)

#filter missing values and absence
data_Culex<- data %>%
  filter(species == "Culex pipiens") %>%
  filter(is.na(lon) == F) %>%
  filter(is.na(lat) == F) %>%
  filter(occurrenceStatus == "PRESENT")

#load sf domain
domain <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

lat_top <- max(domain$top)
lat_bot <- min(domain$bottom)
lon_right <- max(domain$right)
lon_left <- min(domain$left)

#remove obs outside; #remove other vector # keep only points
data_W_EU <- data_Culex %>%
  filter(lon< lon_right) %>%
  filter(lon>lon_left) %>%
  filter(lat<lat_top) %>%
  filter(lat>lat_bot) 

# WGS1984 Datum
# https://epsg.io/4326
Culex_W_EU <- st_as_sf(x = data_W_EU,
                      coords = c("lon", "lat"),
                      crs = 4326)

Culex <- st_as_sf(x = data_Culex,
                 coords = c("lon", "lat"),
                 crs = 4326)

#plot
ggplot()+
  geom_sf(data = Culex_W_EU)

st_write(Culex_W_EU, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Culex_W_EU.shp")

st_write(Culex, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Culex.shp")

