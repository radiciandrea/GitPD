# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R

#Franche scale

# to vaildate with data of presence and absence

# presence: data from Johanna Fite (ANSES) 
# Absence: will take probably (??)

# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R

# to vaildate with data of presence and absence

rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(tidyverse)

type = "_01"

folder_out = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim", type)

# name = "W_EU"
# year = 2005

files = list.files(folder_out)

name = substring(files[1], nchar(type)+10, nchar(type)+13)
years = substring(files, nchar(type)+15, nchar(type)+18) #CORREGGGERE QUI

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

E0_m = matrix(NA, ncol = length(E0_v), nrow = length(files))
rm(Sim)

for (i in 1:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  E0_m[i,]= E0_v
  rm(Sim)
}


# correzione degli NaN con formula geomatrica
E0_m_c <- apply(E0_m, 2, function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})

#Metelamnn, geometric mean. = exp(mean(log))

years_sel_2 = 2015:2023 # 2017:2023 
E0_m_c_sel_2 <- apply(E0_m[which(years %in% years_sel_2),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_2015_2023 = apply(E0_m_c_sel_2, 2,
                     function(x){exp(mean(log(x)))})

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) 


# select France only

domain_years_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_2015_2023 = E0_2015_2023) %>%
  filter(Country == "France")

# load presence data

folder_CANNET = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DGS_Cannet"
data_presence <- read.csv2(paste0(folder_CANNET, "/IRD communes années.csv"))











folder_COMM = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/Eurostat_2010"
shp_communes <- st_read(paste0(folder_COMM, "/Comuni.shp"))
