# plot del cycle
# MM_pis_matrix_EOBS_cycle_consec_01.R

#per individuare indicatori di attività e di presenza di adulti


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(lubridate)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec_01" # EOBS_sim_consec
folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_elab_01" # "EOBS_elab"

# name = "W_EU"
# year = 2005

files = list.files(folder_out)

name = substring(files[1], 10, 13)
years = substring(files, 15, 18)

first_day = as.Date(paste0(min(years), "-01-01"))
last_day = as.Date(paste0(max(years), "-12-31"))

date_sim = seq(from = first_day, to = last_day, by = 'day')
DOS_sim = 1:length(date_sim)
n_d = length(DOS_sim)

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

n_c = 5 # numero di classi
n_r = (ncol(Sim)-1)/n_c #numero di regioni
regions = 1:n_r

n_d_i = nrow(Sim)
Sim_tot = matrix(NA, ncol = n_c*n_r+1, nrow = n_d) 
Sim_tot[1:n_d_i,]=Sim

#same for beta
beta_approx_tot = matrix(NA, ncol = n_r, nrow = n_d) 
beta_approx_tot[1:n_d_i,]=beta_approx

k = n_d_i
for (i in 2:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  n_d_i = nrow(Sim)
  Sim_tot[k + 1:n_d_i,]=Sim
  beta_approx_tot[k + 1:n_d_i,]=beta_approx
  k = k + n_d_i
}


#########################
#plot pop
Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
                      "region" = rep(rep(regions, each = max(DOS_sim)), n_c),
                      "t" = rep(DOS_sim, n_r*n_c),
                      "value" = c(Sim_tot[, 2:(1+n_c*n_r)])) #5 classes

beta_approx_m_df = data.frame("variable" = "beta",
                              "region" = rep(regions, each = max(DOS_sim)),
                              "t" = rep(DOS_sim, n_r),
                              "value" = c(beta_approx_tot)) #5 classes