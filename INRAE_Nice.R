#Climatik
# c'è montpellier ma non c'è Nizza :(

#### BELOW: DATA FROM NICE


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
library(dplyr)
load("C:/Users/Andrea/Desktop/Alcuni file permanenti/Dottorato/Codice/07_METAPOP_model/Data SICLIMA/tas_tasmin_pr_1980_2021.RData")

# Nice 2008 2011

time <- which(year %in% 2008:2011)
cell <- 330

W_tot_df <- data.frame(region = "NICE",
                   year = year[time],
                   DOY = jd[time],
                   date = NA,
                   P = pr[cell, time],
                   T_av = tas[cell, time])

W_tot_df$DOS = 1:nrow(W_tot_df)
W_tot_df$date = as.Date(W_tot_df$DOS, origin = "2007-12-31")

time_df <- W_tot_df%>%
  select(c("DOS", "DOY", "date"))

plot(W_tot_df$DOS, W_tot_df$T_av)

plot(W_tot_df$DOS, W_tot_df$P)

save(W_tot_df, time_df, file =  "C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Weather_Nice_200811.RData")

# DATA FROM NICE

library(dplyr)
library(lubridate)

folder = "C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Tran 2013/"

data = read.delim(paste0(folder, "Data_Nice_synthese.txt"), header = TRUE)

# cleaning data and elaborations to homogenize with 

data$Noeufs[which(data$Noeufs == -99)] = NA

Eggs_tot_df <- data %>%
  mutate(date = paste(ANNEE, MOIS, JOUR, sep = "-")) %>%
  mutate(DOY = yday(date)) %>%
  mutate(DOS = julian(as.Date(date), origin = as.Date('2007-12-31'))) %>%
  group_by(DOS) %>%
  mutate(eggs = mean(Noeufs, na.rm = T)) %>%
  mutate(ovitrps = sum(DOY>0, na.rm = T)) %>%
  ungroup()%>%
  distinct(DOS, .keep_all = TRUE) %>%
  mutate(region = "NICE")%>%
  mutate(type = "observed") %>%
  select(c("region", "DOS", "eggs", "ovitrps", "type", "DOY", "date"))

time_df <- W_tot_df%>%
  filter(region == region_names[1]) %>%
  dplyr::select(c("DOS", "DOY", "date"))    

plot(data_m$JJULIEN, data_m$EGGS)  
