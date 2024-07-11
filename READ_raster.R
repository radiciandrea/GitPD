# read https://sedac.ciesin.columbia.edu/data/collection/gpw-v4/sets/browse

# we choose 	Population Count, v4.11 (2000, 2005, 2010, 2015, 2020)

# years 2015

# After running Read OBS

rm(list = ls())

library(raster)
library(sf)
library(stars)
library(dplyr)

rm(list = ls())

# download from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php

folder_r = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/"
folder_sh = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/"

# load eobs gid
#domain <- st_read(paste0(folder_sh, "grid_eobs.shp")) 
domain <- st_read(paste0(folder_sh, "grid_eobs_0.1.shp")) 

# load gpw-v4
#pop_gpw <- raster(paste0(folder_r, "gpw_v4_population_count_rev11_2015_15_min.tif"))
pop_gpw <- raster(paste0(folder_r, "gpw_v4_population_count_rev11_2015_30_sec.tif"))
res(pop_gpw)

pop_gpw_crop <- raster::crop(pop_gpw, st_bbox(domain))

# writeRaster(pop_gpw_crop,paste0(folder_r, "gpw_v4_population_count_rev11_2015_30_sec_crop.tif"),options=c('TFW=YES'))

# #replace NA with zeros 
# pop_gpw[which(is.na(pop_gpw)==T)] = 0

pop_gpw_agg <- aggregate(pop_gpw_crop, fact=12, fun = sum)

# pop_gpw_agg[which(pop_gpw_agg==0)] = NA

#aggregate raster
writeRaster(pop_gpw_agg,paste0(folder_r, "gpw_v4_population_count_rev11_2015_01.tif"),options=c('TFW=YES'))

#extract from centroids - fix crs
pop_gpw_values <- extract(pop_gpw_agg, st_transform(st_centroid(domain), st_crs(pop_gpw_agg)))

domain <- domain %>%
  mutate(pop = pop_gpw_values) 

domain$popkm2 <- domain$pop*10^6/as.numeric(st_area(domain))

# st_write(domain, paste0(folder_sh, "grid_eobs_01+pop.shp"))


