library(ncdf4)
library(raster)
library(dplyr)

rm(list = ls())

# download from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php

folder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS"

data_rr <- nc_open(paste0(folder, "rr_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tg <- nc_open(paste0(folder, "tg_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tn <- nc_open(paste0(folder, "tn_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tx <- nc_open(paste0(folder, "tx_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))