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

#load sf domain
domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 
