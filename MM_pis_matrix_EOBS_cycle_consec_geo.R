# plot geo delle attività stagionali di 

# MM_pis_matrix_EOBS_cycle_consec_geo

#per plottare anni indipendenti


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)

type = "_consec"

folder_out = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim", type)

files = list.files(folder_out)

name = "W_EU"
years = substring(files, nchar(type)+8, nchar(type)+11)

# load domain
domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name,".shp")) %>%
  arrange(region)

people_ha = domain_sel$popkm2/100

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

#indicatori

# fist appereance (date where M/A > 1)
F0_m = matrix(NA, ncol = length(E0_v), nrow = length(files))

# seasonal lenght (number of days where M/A > 1)
S0_m = matrix(NA, ncol = length(E0_v), nrow = length(files))

# abundance - mean number of adult per person during season
A0_m = matrix(NA, ncol = length(E0_v), nrow = length(files))

