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
domain <- st_read(paste0(folder_sh, "grid_eobs.shp")) 

# load gpw-v4
pop_gpw <- raster(paste0(folder_r, "gpw_v4_population_count_rev11_2015_15_min.tif"))

#extract from centroids - fix crs
pop_gpw_values <- extract(pop_gpw, st_transform(st_centroid(domain), st_crs(pop_gpw)))

domain <- domain %>%
  mutate(pop = pop_gpw_values) 

domain$popkm2 <- domain$pop*10^6/as.numeric(st_area(domain))

# st_write(domain, paste0(folder_sh, "grid_eobs+pop.shp"))


