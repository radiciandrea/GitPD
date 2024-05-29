# I try to code the model by Metelmann 2019
# I'll need to compare with octave code

rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)

#load T and P

#Getting T and P and Eggs from Arpae (see ReadNc+ARPAE) + nc by Cyril

# load("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Weather_Nice_200811.RData") #Nizza
load("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather_ER_20112021.RData") #Emilia Romagna

# chose a region and years
region_x = "BOLOGNA" # "BOLOGNA" "PIACENZA
year_x = 2011:2021 #2011:2021  2008:2011 for Nice

W_df <- W_tot_df %>%
  filter(region == region_x) %>%
  filter(year %in% year_x )

temp <- W_df$T_av
prec <- W_df$P

# temp and prec
t_s = W_df$DOS[1] # simulate multiple year
t_end = tail(W_df$DOS, n = 1)
# t_end = 365
d = t_s:t_end
doy = W_df$DOY

#elaborate temp and prec
temp_7 = sapply(1:length(temp), function(x){return(mean(temp[max(1,x-7):x]))}) # temp of precedent 7 days

#photoperiod (which variables should I take? sunrise - sunset)
SunTimes_df<- getSunlightTimes(as.Date(W_df$date), lat= 44.5, lon = 11.5)# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
W_df$Ph_P= as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)

#parameters (Metelmann 2019)

CTT_s = 11 #critical temperature over one week in spring (°C )
CPP_s = 11.25 #critical photoperiod
