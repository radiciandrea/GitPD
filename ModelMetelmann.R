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
t_end = 10
d = t_s:t_end
doy = W_df$DOY

#elaborate temp and prec
temp_7 = sapply(1:length(temp), function(x){return(mean(temp[max(1,x-7):x]))}) # temp of precedent 7 days
temp_h = temp #this will be modified with equation
temp_min_DJF = sapply(1:length(temp), function(x){return(min(temp[max(1,x-300):x]))}) #min temp of last winter (daily or hours?)

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
CPP_a = 10.058 + 0.08965 * L # critical photperiod in autumn
sigma = 0.1 *(temp_7 > CTT_s)*(Ph_P > CPP_s) # spring hatching rate (1/day)
omega = 0.5 *(Ph_P < CPP_a)*(doy > 183) # fraction of eggs going into diapause
delta_E = 1/7.1 #normal egg development rate (1/day)
delta_J = 1/(83.85 - 4.89*temp_h + 0.08*temp_h^2) #juvenile development rate 
delta_I = 1/(50.1 - 3.574*temp_h + 0.069*temp_h^2) #first pre blood mean rate
mu_E = -log(0.955 * exp(-0.5*((temp_h-18.8)/21.53)^6)) # egg mortality rate
mu_J = -log(0.977 * exp(-0.5*((temp_h-21.8)/16.6)^6)) # juvenile mortality rate
mu_A = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
gamma = 0.93*exp(-0.5*((temp_min_DJF -20.9)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
beta = (33.2*exp(-0.5*((temp_h-70.3)/14.1)^2)*(38.8 - temp_h)^1.5)*(temp_h<= 38.8) #fertility rate 
lambda = 10^6 # capacity parameter (larvae/day/ha)

# advanced parameter for carrying capacity
H = 3000 #human population density per km²  #NICE = 4,840/km² #BOLOGNA = 2,772/km² #RAVENNA = 239.1/km² # https://www.citypopulation.de/en/france/alpesmaritimes/nice/06088__nice/
alpha_evap = 0.9
alpha_dens = 0.001
alpha_rain = 0.00001

K = lambda * (1-alpha_evap)/(1 - alpha_evap^d)*
  sapply(d, function(x){return(sum(alpha_evap^(x:1) * (alpha_rain*prec[1:x] + alpha_dens*H)))})

# advanced parameter for hatching
eps_rat = 0.2
eps_0 = 1.5
eps_var = 0.05
eps_opt = 8
eps_dens = 0.01
eps_fac = 0.01

h = (1-eps_rat)*(1+eps_0)*exp(-eps_var*(prec-eps_opt)^2)/
  (exp(-eps_var*(prec-eps_opt)^2)+ eps_0) +
  eps_rat*eps_dens/(eps_dens + exp(-eps_fac*H))

n_s = 1 # number of locations (added; 1 for no dimension)

# list with parameters to be passed to the ODE system
parms = list(beta = beta,
             omega = omega,
             h = h,
             mu_E = mu_E,
             mu_J = mu_J,
             mu_A = mu_A,
             delta_E = delta_E,
             delta_J = delta_J,
             delta_I = delta_I,
             sigma = sigma,
             gamma = gamma) 

df <- function(t, x, parms) {
  
  # initial conditions and parameters
  with(parms, { 
    
    E = x[(1+n_s*0):(1*n_s)]
    J = x[(1+n_s*1):(2*n_s)]
    I = x[(1+n_s*2):(3*n_s)]
    A = x[(1+n_s*3):(4*n_s)]
    E_d = x[(1+n_s*4):(5*n_s)]
    
    t_n = t[1]-t_s+1 # time of numerical integration
    
    # ODE definition 
    dE = beta[t_n]*(1-omega[t_n])*A - (h[t_n]*delta_E - mu_E[t_n])*E
    dJ = h[t_n]*(delta_E*E + sigma[t_n]*gamma[t_n]*E_d) - (delta_J[t_n] + mu_J[t_n] + J/K[t_n])*J  
    dI = 0.5*delta_J[t_n]*J - (delta_I[t_n] + mu_A[t_n])*I
    dA = delta_I[t_n]*I - mu_A[t_n]*A
    dE_d = beta[t_n]*omega[t_n]*A -  h[t_n]*sigma[t_n]*gamma[t_n]*E_d #I believe there should be an additional mortality due to winter

    
    dx <- c(dE, dJ, dI, dA, dE_d)
    
    return(list(dx))})
}

# System initialization
E0 = 0
J0 = 0
I0 = 0
A0 = 0
E_d_0 = 10^6 # at 1st of January (10^6)

X_0 = c(E0, J0, I0, A0, E_d_0)

#integration
Sim <- as.data.frame(ode(X_0, d, df, parms))

