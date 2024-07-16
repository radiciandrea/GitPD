# code to extract abundances from VectAbundance (Da Re et al)
#in our domain 025

#https://ecoevorxiv.org/repository/view/6444/

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)

name = "W_EU"

folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/VectAbundance/"
folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"

#read excel
data <- read_excel(path = paste0(folder_in, "Vectabundace_v015.xlsx"))

#the database contains only "eggs" of "albopictus" in "ovitrap" 
#keep only important info, remove NA
data_sel <- data %>%
  select(c("ID", "year", "date", "value", "longitude", "latitude"))%>%
  filter(is.na(value)==F)

#extract geo

data_geo <- data %>%
  select(c("ID", "longitude", "latitude", "Country"))%>%
  unique()

data_geo$region = NA

#load sf domain
domain <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

domain <- domain %>%
  arrange(region)

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
  data_geo$region[i] = k[1]
  
  # #write also in domain
  # domain$IdVAb[domain$region == k] = data_geo$ID[i]
}

#join by region
data_sel_geo <- left_join(data_geo, data_sel)
id_more_frequent<- c()

#extract for each locations only the longest series
for (k in unique(data_sel_geo$region)){
  IDs <- data_sel_geo$ID[data_sel_geo$region == k]
  id_more_frequent <- c(id_more_frequent, names(sort(table(IDs), decreasing =T)[1]))
}

data_sel_geo <- data_sel_geo%>%
  filter(ID %in% id_more_frequent)

Eggs_tot_df <- data_sel_geo %>%
  mutate(eggs = value) %>%
  mutate(type = "observed") %>%
  mutate(date = as.Date(date)) %>%
  group_by(region)%>%
  mutate(DOY = yday(date)) %>%
  mutate(DOS = julian(as.Date(date), origin = as.Date(paste0(min(as.numeric(year)-1),'-12-31')))) %>%
  ungroup()%>%
  select(c("region", "DOS", "eggs","DOY", "date"))

# save(Eggs_tot_df, file = paste0(folder_out, "VectAbundance_025.RData"))


# Save shp

domain_VectAbundance <- data_sel_geo %>%
  mutate("IDVectAb" = ID) %>%
  select(c("region", "IDVectAb")) %>%
  unique()
  
domain_VectAbundance <- left_join(domain, domain_VectAbundance)%>%
  select(c("region", "IDVectAb"))
  
#plot test
ggplot()+
  geom_sf(data = domain_VectAbundance, aes(fill = IDVectAb))

# st_write(domain_VectAbundance, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, "_IDVectAb.shp"))
