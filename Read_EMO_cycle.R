#code which divides EMO chunks into years

library(ncdf4)
library(raster)
library(sf)
library(dplyr)
library(lubridate)
library(pracma)

rm(list = ls())

# download from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php

#load first EOBS to get lon lat
if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EMO-JRC/"
  folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EMO-JRC_elab/"
  folder_sh = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/"
} else {
  folder_eobs = "EMO-JRC/"
  folder_out = "EMO-JRC_elab/"
  folder_sh = "Shp_elab/"
}

data_pr <- nc_open(paste0(folder_in, "pr_1d_19900101_20223112_EMO-1arcmin_Occitania.nc"))
data_tas <- nc_open(paste0(folder_in, "tas_1d_19900101_20223112_EMO-1arcmin_Occitania.nc"))
data_tn <- nc_open(paste0(folder_in, "tn_1d_19900101_20223112_EMO-1arcmin_Occitania.nc"))
data_tx <- nc_open(paste0(folder_in, "tx_1d_19900101_20223112_EMO-1arcmin_Occitania.nc"))

print(data_pr)

lat = ncvar_get(data_pr, "lat")
lon = ncvar_get(data_pr, "lon")
time = ncvar_get(data_pr, "time") # days since 1990-01-02 06:00:00

#I build the grid0 on qgis
lat_m = lat[1]-(lat[2]-lat[1])/2
lat_M = tail(lat, 1)+(lat[2]-lat[1])/2
lon_m = lon[1]-(lon[2]-lon[1])/2
lon_M = tail(lon, 1)+(lon[2]-lon[1])/2

# # ADDED: create grid
# # first create polygon bb
# sfc_bb = st_sfc(st_polygon(list(rbind(c(lon_m,lat_m), c(lon_M,lat_m),
#                                    c(lon_M,lat_M), c(lon_m,lat_M),
#                                    c(lon_m,lat_m)))),
#              crs=4326)
# 
# #grid
# grid_Occitanie_EMO <- st_make_grid(
#   sfc_bb,
#   n = c(length(lon), length(lat)),
# )
# 
# # st_write(grid_Occitanie_EMO, paste0(folder_sh, "grid_Occitanie_EMO.shp")) 
# #need to re-uplead to fix class
# domain <- st_read(paste0(folder_sh, "grid_Occitanie_EMO.shp"))
# 
# # each cell contains 4 gpw_v4_population_count_rev11_2015_30_sec_crop data
# # apprently id is not important in the other shp grids (EOBS) so we'll do the same
# # there is not any col index or row index.
# 
# # extract raster of population
# 
# folder_r = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/"
# 
# # load gpw-v4
# pop_gpw <- raster(paste0(folder_r, "gpw_v4_population_count_rev11_2015_30_sec.tif"))
# res(pop_gpw)
# 
# #crop raster
# pop_gpw_crop <- raster::crop(pop_gpw, st_bbox(domain))
# 
# # the "fact is the number of "sides per aggregatedside" or the sqrt(number of pixels in a aggregated pixel)
# pop_gpw_agg <- aggregate(pop_gpw_crop, fact=2, fun = sum)
# 
# pop_gpw_agg[which(pop_gpw_agg==0)] = NA
# 
# # #aggregate raster
# # writeRaster(pop_gpw_agg,paste0(folder_r, "gpw_v4_population_count_rev11_2015_01.tif"),options=c('TFW=YES'))
# 
# #extract from centroids - fix crs
# pop_gpw_values <- extract(pop_gpw_agg, st_transform(st_centroid(domain), st_crs(pop_gpw_agg)))
# 
# domain <- domain%>%
#   mutate(pop = pop_gpw_values) 
# 
# domain$popkm2 <- domain$pop*10^6/as.numeric(st_area(domain))
# 
# # st_write(domain, paste0(folder_sh, "grid_Occitanie_EMO+pop.shp"))
domain <- st_read(paste0(folder_sh, "grid_Occitanie_EMO+pop.shp"))

####

date = as.Date(time, origin=as.Date("1990-01-01"))
year_rep = sapply(date, function(x){substr(x, 1, 4)})

#let's consider dats only >= 2005
years_sel = 2004:2022
index_date_sel <- which(year_rep %in% years_sel)
year_rep_sel = year_rep[index_date_sel]
date_sel = date[index_date_sel]

# rain - precipitation
# long # lat # time
pr <- ncvar_get(data_pr, attributes(data_pr$var)$names[1])
pr_sel <- pr[,,index_date_sel]
rm(pr, data_pr)

# mean temperature
tas <- ncvar_get(data_tas, attributes(data_tas$var)$names[1])
tas_sel <- tas[,,index_date_sel]
rm(tas, data_tas)

#min temperature
tn <- ncvar_get(data_tn, attributes(data_tn$var)$names[1])
tn_sel <- tn[,,index_date_sel]
rm(tn, data_tn)

#max temperature
tx <- ncvar_get(data_tx, attributes(data_tx$var)$names[1])
tx_sel <- tx[,,index_date_sel]
rm(tx, data_tx)

tic()
for(year in years_sel){
  
  index_year = which(year_rep_sel ==year)
  
  #extract weather in each location
  W_list <- vector(mode = "list", sum(is.na(tas_sel[,,1])==T))
  k = 1
  
  for(j in 1:length(lat)){
    for(i in 1:length(lon)){
      
      if(is.na(pr_sel[i,j,1])==F){
    
        W_df <- data.frame(
          region = k,
          lon = lon[i],
          lat = lat[j],
          pop = domain$popkm2[i+(j-1)*length(lon)], # TO CHECK
          year = year,
          DOS = as.numeric(strftime(date_sel[index_year], format = "%j")),
          date = date_sel[index_year],
          P = pr_sel[i,j,index_year],
          T_av = tas_sel[i,j,index_year],
          T_M = tx_sel[i,j,index_year],
          T_m = tn_sel[i,j,index_year],
          DOY = as.numeric(strftime(date_sel[index_year], format = "%j")))
        
        W_list[[k]]<-W_df
        k = k+1
      }
    }
  }
  
  # W_tot_df <- do.call("rbind", W_list)
  W_tot_df <- rbindlist(W_list)
  
  #save
  save(W_tot_df,
       file = paste0(folder_out, "EMO-JRC_sel_", year, "_Occitanie_EMO.RData")) #EOBS_sel_2011_W_EU #EOBS_sel_2011_Occitanie.RData
  toc()
}

# #create shape
# 
# k = 1
# 
# domain <- domain %>%
#   mutate(region = NA)
# 
# for(j in 1:length(lat)){
#   for(i in 1:length(lon)){
#     if(is.na(pr_sel[i,j,1])==F){
#       domain$region[i+(j-1)*length(lon)] = k
#     k = k+1
#     }
#   }
# }
# 
# domain_sel = domain%>%
#   filter(is.na(region)==F)
# 
# #save
# st_write(domain_sel, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_Occitanie_EMO.shp")) #domain_sel_W_EU #domain_sel_Occitanie
