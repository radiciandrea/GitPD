# code to extract abundances from VectAbundance (Da Re et al)

#https://ecoevorxiv.org/repository/view/6444/

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

name = "W_EU"

folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/VectAbundance/"

#read excel
data <- read_excel(path = paste0(folder_eobs, "Vectabundace_v015.xlsx"))

#the database contains only "eggs" of "albopictus" in "ovitrap" 

data_sel <- data %>%
  select(c("ID", "year", "date", "value", "longitude", "latitude"))

#extract geo

data_geo <- data %>%
  select(c("ID", "longitude", "latitude", "Country"))%>%
  unique()

data_geo$region = NA

#load sf domain
domain <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

domain <- domain %>%
  arrange(region)

#IdVectAbundance
domain$IdVAb =NA

# in cycle 

lat_top <- domain$top
lat_bot <- domain$bottom
lon_right <- domain$right
lon_left <- domain$left

#remove obs outside 
data_geo_sel <- data_geo %>%
  filter(longitude<max(lon_right))%>%
  filter(longitude>min(lon_left))%>%
  filter(latitude<max(lat_top))%>%
  filter(latitude>min(lat_bot))

for(i in row(data_geo)){
  lon_cen <- data_geo$longitude[i]
  lat_cen <- data_geo$latitude[i]
  
  dist_2 = (lon_cen-(lon_right+lon_left)/2)^2 + (lat_cen-(lat_top+lat_top)/2)^2
  
  k <- which(dist_2 == min(dist_2))
  data_geo$region[i] = k
  
  #write also in domain
  domain$IdVAb[domain$region == k] = data_geo$ID[i]
}

#plot test
ggplot()+
  geom_sf(data = domain, aes(fill = IdVAb))
