# code to extract abundances from VectAbundance (Da Re et al)

#https://ecoevorxiv.org/repository/view/6444/

rm(list = ls())

library(readxl)
library(dplyr)
library(sf)

name = "W_EU"

folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/VectAbundance/"

#read excel
data <- read_excel(path = paste0(folder_eobs, "Vectabundace_v015.xlsx"))

#the database contains only "eggs" of "albopictus" in "ovitrap" 

data_sel <- data %>%
  select(c("ID", "year", "date", "value", "longitude", "latitude"))

#extract geo

data_geo <- data %>%
  select(c("ID", "longitude", "latitude"))%>%
  unique()

data_geo$region = NA

#load sf domain
domain <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

#IdVectAbundance
domain$IdVAb =NA

# in cycle 

for(i in row(data_geo)){
  lon_cen <- data_geo$longitude[i]
  lat_cen <- data_geo$latitude[i]
  
  domain_sel <- domain %>%
    filter(left < lon_cen) %>%
    filter(right > lon_cen) %>%
    filter(top > lat_cen) %>%
    filter(bottom < lat_cen)
  
  #some regions (data_geo) may fall outside the domain
  if(nrow(domain_sel)>0){
    #take only one region
    k <- domain_sel$region[1]
    data_geo$region[i] = k
    
    #write also in domain
    domain$IdVAb[domain$region == k] = data_geo$ID[i]
  }
}

