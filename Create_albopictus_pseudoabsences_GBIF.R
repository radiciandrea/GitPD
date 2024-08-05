# code to create pseudoabsences from GBIF

# https://www.gbif.org/species/1651430
# I do modify and I look fro Culex pipiens
# https://www.gbif.org/species/1652991

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

name = "W_EU"

folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GBIF/"

#coordinats are weak: download from gbif website - preprocessing by hand (text to column; cooridnates to number
data = read.csv2(paste0(folder_in , "download_GBIF_Culex_mod.csv"))

data$lat<-as.numeric(data$decimalLatitude)
data$lon<-as.numeric(data$decimalLongitude)

#filter missing values and absence
data_Albo <- data %>%
  filter(species == "Culex pipiens") %>%
  filter(is.na(lon) == F) %>%
  filter(is.na(lat) == F) %>%
  filter(occurrenceStatus == "PRESENT")