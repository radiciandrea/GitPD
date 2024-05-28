#Climatik
# c'è montpellier ma non c'è Nizza :(


# https://agroclim.inrae.fr/siclima/extraction/#HomePlace:

# Base climatique: SAFRAN
# Maille Nice: 8623

#no, bisogna chiedere autorizzazione

#Drias
#	Simulations 'DRIAS-2020': données quotidiennes corrigées [format Netcdf]
# Non va bene: la "référence" arriva fino al 2005

#MeteoFrance
#https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=230&id_rubrique=40

read.csv("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance/SIM2_2010_2019.csv")

# Climate data store, Copernicus

# SICLIMA

load("C:/Users/Andrea/Desktop/Alcuni file permanenti/Dottorato/Codice/07_METAPOP_model/Data SICLIMA/tas_tasmin_pr_1980_2021.RData")

# Nice 2008 2011

time <- which(year %in% 2008:2011)
cell <- 330

W_tot_df <- data.frame(region = "Nice",
                   year = year[time],
                   DOY = jd[time],
                   date = NA,
                   P = pr[cell, time],
                   T_av = tas[cell, time])

W_tot_df$DOS = 1:nrow(W_tot_df)
W_tot_df$date = as.Date(W_tot_df$DOS, origin = "2007-12-31")
