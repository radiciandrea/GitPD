library(ncdf4)
library(raster)
library(sf)
library(dplyr)

rm(list = ls())

# download from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php

folder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS/"

data_rr <- nc_open(paste0(folder, "rr_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tg <- nc_open(paste0(folder, "tg_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tn <- nc_open(paste0(folder, "tn_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tx <- nc_open(paste0(folder, "tx_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))

# 25N-71.5N x 25W-45E

# extract only 2021-2020
years = 2011:2020

print(data_rr)
#days since 1950-01-01 00:00
attributes(data_rr$var)
lat = ncvar_get(data_rr, "latitude")
lon = ncvar_get(data_rr, "longitude")
time = ncvar_get(data_rr, "time") # days since 1950-01-01 00:00

#I build the grid0 on qgis
lat_m = lat[1]-(lat[2]-lat[1])/2
lat_M = tail(lat, 1)+(lat[2]-lat[1])/2
lon_m = lon[1]-(lon[2]-lon[1])/2
lon_M = tail(lon, 1)+(lon[2]-lon[1])/2



#select a subgrid to be kept. 

rr <- ncvar_get(data_rr, attributes(data_rr$var)$names[1])
tg <- ncvar_get(data_tg, attributes(data_tg$var)$names[1])
tn <- ncvar_get(data_tn, attributes(data_tn$var)$names[1])
tx <- ncvar_get(data_tx, attributes(data_tx$var)$names[1])

