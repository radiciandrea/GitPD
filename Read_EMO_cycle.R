#code which divides EMO chunks into years

library(ncdf4)
library(raster)
library(sf)
library(dplyr)
library(lubridate)

rm(list = ls())

# download from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php

#load first EOBS to get lon lat
if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EMO-JRC/"
  folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EMO-JRC_elab/"
  folder_shape = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/"
} else {
  folder_eobs = "EMO-JRC/"
  folder_out = "EMO-JRC_elab/"
  folder_shape = "Shp_elab/"
}
