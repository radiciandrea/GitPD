# code to setup sumilations as in the Da Re experiment 23/08/2024

library(ncdf4)
library(raster)
library(sf)
library(dplyr)
library(lubridate)

rm(list = ls())

folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DaRe/"
folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DaRe_elab/"

#upload nc
data_pr <- nc_open(paste0(folder_in, "tp_2018_2023.nc"))
data_tas <- nc_open(paste0(folder_in, "tas_2018_2023.nc"))

print(data_pr)

lat = ncvar_get(data_pr, "latitude")
lon = ncvar_get(data_pr, "longitude")
time = ncvar_get(data_pr, "time") # days since 1970-1-1
