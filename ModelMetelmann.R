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

# temp and prec (by now, only at a daily step: it should change at least hour by hour)

W_df <- W_tot_df %>%
  filter(region == region_x) %>%
  filter(year %in% year_x )

temp <- W_df$T_av
prec <- W_df$P

t_s = W_df$DOS[1] # simulate multiple year
t_end = tail(W_df$DOS, n = 1)
# t_end = 365
d = t_s:t_end
doy = W_df$DOY

#elaborate temp and prec
temp_7 = sapply(1:length(temp), function(x){return(mean(temp[max(1,x-7):x]))}) # temp of precedent 7 days
temp_h = temp #this will be modified with equation

# T_h = ((TM+Tm)/2 + (TM-Tm)/2*cos((h+10)/(10+ts)))*(h<ts)+
#   ((TM+Tm)/2 - (TM-Tm)/2*cos((h-ts)/(14-ts)))*(h>ts)*(h<14)+
#   ((TM+Tm)/2 + (TM-Tm)/2*cos((h-14)/(10+ts)))*(h>14)

#photoperiod Ph_P (which variables should I take? sunrise - sunset)
SunTimes_df<- getSunlightTimes(as.Date(W_df$date), lat= 44.5, lon = 11.5)# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
Ph_P= as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)

#parameters (Metelmann 2019)

CTT_s = 11 #critical temperature over one week in spring (°C )
CPP_s = 11.25 #critical photoperiod in spring
L = 44 # latitute more or less in ER - Nice
CPP_A = 10.058 + 0.08965 * L # critical photperiod in autumn
sigma = 0.1 *(temp_7 > CTT_s)*(Ph_P > CPP_s) # spring hatching rate (1/day)
omega = 0.5 *(Ph_P < CPP_a)*(doy > 183) # fraction of eggs going into diapause
delta_E = 1/7.1 #normal egg development rate (1/day)
delta_J = 1/(83.85 - 4.89*temp_h + 0.08*temp_h^2) #juvenile development rate 
delta_I = 1/(50.1 - 3.574*temp_h + 0.069*temp_h^2) #first pre blood mean rate
mu_E = -ln(0.955 * exp(-0.5*((temp_h-18.8)/21.53)^6)) # egg mortality rate
mu_J = -ln(0.977 * exp(-0.5*((temp_h-21.8)/16.6)^6)) # juvenile mortality rate
mu_A = -ln(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
gamma = 0.93*exp(-0.5((temp_min -20.9)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
beta = (33.2*exp(-0.5*((temp_h-70.3)/14.1)^2)*(38.8 - temp_h)^1.5)*(temp_h<= 38.8)
lambda = 10^6 # capacity parameter (larvae/day/ha)

# advanced parameter for carrying capacity
H = 3000 #human population density per km²  #NICE = 4,840/km² #BOLOGNA = 2,772/km² #RAVENNA = 239.1/km² # https://www.citypopulation.de/en/france/alpesmaritimes/nice/06088__nice/
alpha_evap = 0.9
alpha_dens = 0.001
alpha_rain = 0.00001
K = lambda * (1-alpha_evap)/(1 - al^ha_evap[d])*sum( alpha_evap[1:d])*(alpha_rain*prec + alpha_dens*H)

# advanced parameter for hatching
eps_rat = 0.2
eps_0 = 1.5
eps_var = 0.05
eps_opt = 8
eps_dens = 0.01
eps_fac = 0.01

h = (1-eps_rat)*(1+eps_0)*exp(-epsvar*(prec-eps_opt)^2)/
  (exp(-epsvar*(prec-eps_opt)^2)+ eps_0) +
  eps_rat*eps_dens/(eps_dens + exp(-eps_fac*H))
