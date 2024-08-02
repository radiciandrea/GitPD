# set initial condition X0 for  MM_pis_matrix_EOBS_cycle_consec

# load previous 10-years-later simulation from MM_pis_matrix_EOBS_cycle_consec_plot

rm(list = ls())

library(dplyr)
library(pracma)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec"
folder_in = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Data_sim"

# name = "W_EU"
# year = 2005

files = list.files(folder_out)

name = substring(files[1], 10, 13)
years = substring(files, 15, 18)

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[which(years == min(as.numeric(years))+10)]))

#correct NAs and < 1
E0_v[which(E0_v<1)] = 1
E0_v[which(is.na(E0_v))] = 1

E_d_0 = E0_v

save(E_d_0, file = paste0(folder_in, "/X0_E0_consec_W_EU.RData"))
