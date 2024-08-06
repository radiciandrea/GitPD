# code to extract Data from GBIF

# https://www.gbif.org/species/1651430

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

name = "W_EU"

folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GBIF/"

# little proble: this is identic to Kramer
# data = read.csv2(paste0(folder_in , "albopictus_presence_GBIF.csv"), sep = ",")

# little problem: more columns than names. preprocessing by hand (text to column)
#data = read.csv2(paste0(folder_in , "albopictus_AIMSurv_updated_GBIF_mod.csv"))

#coordinats are weak: download from gbif website - preprocessing by hand (text to column; coordinates to number
data = read.csv2(paste0(folder_in , "download_GBIF_mod.csv"))

data$lat<-as.numeric(data$decimalLatitude)
data$lon<-as.numeric(data$decimalLongitude)

#filter missing values and absence
data_Albo <- data %>%
  filter(species == "Aedes albopictus") %>%
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
data_W_EU <- data_Albo %>%
  filter(lon< lon_right) %>%
  filter(lon>lon_left) %>%
  filter(lat<lat_top) %>%
  filter(lat>lat_bot) 

# WGS1984 Datum
# https://epsg.io/4326
Albo_W_EU <- st_as_sf(x = data_W_EU,
                      coords = c("lon", "lat"),
                      crs = 4326)

Albo <- st_as_sf(x = data_Albo,
                 coords = c("lon", "lat"),
                 crs = 4326)

#plot
ggplot()+
  geom_sf(data = Albo_W_EU)

st_write(Albo_W_EU, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Albo_W_EU.shp")

st_write(Albo, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Albo.shp")
