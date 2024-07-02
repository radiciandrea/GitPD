library(ncdf4)
library(raster)
library(sf)
library(dplyr)
library(lubridate)

rm(list = ls())

# download from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php

folder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS/"

data_rr <- nc_open(paste0(folder, "rr_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tg <- nc_open(paste0(folder, "tg_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tn <- nc_open(paste0(folder, "tn_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))
data_tx <- nc_open(paste0(folder, "tx_ens_mean_0.25deg_reg_2011-2023_v29.0e.nc"))

# 25N-71.5N x 25W-45E

# extract only 2021-2020
# years = 2011:2020

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
time_0 = time[1]-1


#select a subgrid to be kept. 

name = "W_EU" #Occitanie, W_EU, France
year_f = "2020"

date = as.Date(time, origin=as.Date("1950-01-01"))
date_max = paste0(year_f, "-12-31") #"2020-12-31"

date_sel = date[1:which(date == date_max)]
time_sel = 1:length(date_sel)
year_sel = sapply(date_sel, function(x){substr(x, 1, 4)})

grid_sel = st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/grid_eobs_", name, ".shp"))


#find selected square
i_lon_sel = min(grid_sel$col_index):max(grid_sel$col_index)
i_lat_sel = min(grid_sel$row_index):max(grid_sel$row_index)

# rain - precipitation
# long # lat # time
rr <- ncvar_get(data_rr, attributes(data_rr$var)$names[1])
rr_sel <- rr[i_lon_sel, i_lat_sel, time_sel]
rm(rr)

# mean temperature
tg <- ncvar_get(data_tg, attributes(data_tg$var)$names[1])
tg_sel <- tg[i_lon_sel, i_lat_sel, time_sel]
rm(tg)

#min temperature
tn <- ncvar_get(data_tn, attributes(data_tn$var)$names[1])
tn_sel <- tn[i_lon_sel, i_lat_sel, time_sel]
rm(tn)

#max temperature
tx <- ncvar_get(data_tx, attributes(data_tx$var)$names[1])
tx_sel <- tx[i_lon_sel, i_lat_sel, time_sel]
rm(tx)

# sel also lat lon
lat_sel = lat[i_lat_sel]
lon_sel = lon[i_lon_sel]

#extract weather in each location
W_list <- vector(mode = "list", sum(is.na(tn_sel[,,1])==T))
k = 1

grid_sel <- grid_sel %>%
  mutate(region = NA)

for(j in 1:length(i_lat_sel)){
  for(i in 1:length(i_lon_sel)){
      
    if(is.na(rr_sel[i,j,1])==F){
      W_df <- data.frame(
        region = k,
        r_i = i_lon_sel[i],
        r_j = i_lat_sel[j],
        lon = lon_sel[i],
        lat = lat_sel[j],
        pop = grid_sel$pop[which((grid_sel$row_index == i_lat_sel[j]) & (grid_sel$col_index == i_lon_sel[i]))],
        year = year_sel,
        DOS = time_sel,
        date = date_sel,
        P = rr_sel[i,j,],
        T_av = tg_sel[i,j,],
        T_M = tx_sel[i,j,],
        T_m = tn_sel[i,j,],
        DOY = as.numeric(strftime(date_sel, format = "%j")))
      
      grid_sel$region[which((grid_sel$row_index == i_lat_sel[j]) & (grid_sel$col_index == i_lon_sel[i]))] = k
      
      W_list[[k]]<-W_df
      k = k+1
    }
  }
}

W_tot_df <- do.call("rbind", W_list)

domain_sel = grid_sel%>%
  filter(is.na(region)==F)

#save
save(W_tot_df, file = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sel_2011_", year_f, "_", name, ".RData")) #EOBS_sel_2011_W_EU #EOBS_sel_2011_Occitanie.RData
st_write(domain_sel, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) #domain_sel_W_EU #domain_sel_Occitanie
  

