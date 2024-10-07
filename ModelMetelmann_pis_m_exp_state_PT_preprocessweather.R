# extract weather for specific places and run 
# with ModelMetelmann_pis_m_exp_state_PT


rm(list = ls())
gc()

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)

# place
# Montpellier 1524
# Paris 3163
# Paris suburbs sud # 3082

folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_elab" # "EOBS_elab"
name = "W_EU"

city = "Montpellier"
years = 2000:2023

region_x = 1524 # region of Montpellier

W_df_l <- vector(mode = "list", length = length(years))

for (y in years){
  load(paste0(folder_eobs, "/EOBS_sel_", y, "_", name, ".RData"))

  W_df_l[[which(years == y)]] <- W_tot_df %>% filter(region ==region_x)
}

W_tot_df <-do.call("rbind", W_df_l)

save(W_tot_df, file = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_EOBS_",
     city, "_", min(years), "_", max(years), ".RData")) #Nizza
