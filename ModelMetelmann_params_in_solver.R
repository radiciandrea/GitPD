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
year_x = 2011:2011 #2011:2021  2008:2011 for Nice

# temp and prec (by now, only at a daily step: it should change at least hour by hour)

W_df <- W_tot_df %>%
  filter(region == region_x) %>%
  filter(year %in% year_x )

temp <- W_df$T_av
temp_M <- W_df$T_M
temp_m <- W_df$T_m
prec <- W_df$P

t_s = W_df$DOS[1] # simulate multiple year
t_end = tail(W_df$DOS, n = 1)
# t_end = 365*2
d = t_s:t_end
W_df <- W_df %>%
  filter(DOS %in% d)
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
Ph_P = as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)
t_sr = as.numeric(SunTimes_df$sunrise- as.POSIXct(SunTimes_df$date) +2) # time of sunrise: correction needed since time is in UTC

#parameters (Metelmann 2019)

CTT_s = 11 #critical temperature over one week in spring (°C )
CPP_s = 11.25 #critical photoperiod in spring
L = 44 # latitute more or less in ER - Nice
CPP_a = 10.058 + 0.08965 * L # critical photperiod in autumn
sigma = 0.1 *(temp_7 > CTT_s)*(Ph_P > CPP_s) # spring hatching rate (1/day)
omega = 0.5 *(Ph_P < CPP_a)*(doy > 183) # fraction of eggs going into diapause
delta_E = 1/7.1 #normal egg development rate (1/day)
# delta_J = 1/(83.85 - 4.89*temp_h + 0.08*temp_h^2) #juvenile development rate (in SI: 82.42 - 4.87*temp_h + 0.08*temp_h^ 2)
# delta_I = 1/(50.1 - 3.574*temp_h + 0.069*temp_h^2) #first pre blood mean rate
# mu_E = -log(0.955 * exp(-0.5*((temp_h-18.8)/21.53)^6)) # egg mortality rate
# mu_J = -log(0.977 * exp(-0.5*((temp_h-21.8)/16.6)^6)) # juvenile mortality rate
mu_A = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
mu_A[which(is.na(mu_A))] = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)) #correct the problems due to negative values from SI

gamma = 0.93*exp(-0.5*((temp_min_DJF -11.68)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
beta = (33.2*exp(-0.5*((temp_h-70.3)/14.1)^2)*(38.8 - temp_h)^1.5)*(temp_h<= 38.8) #fertility rate 
lambda = 10^6 # capacity parameter (larvae/day/ha)

# advanced parameter for carrying capacity
H = 3000 #human population density per km²  #NICE = 4,840/km² #BOLOGNA = 2,772/km² #RAVENNA = 239.1/km² # https://www.citypopulation.de/en/france/alpesmaritimes/nice/06088__nice/
alpha_evap = 0.9
alpha_dens = 0.001
alpha_rain = 0.00001

# K = lambda * (1-alpha_evap)/(1 - alpha_evap^d)*
#   sapply(d, function(x){return(sum(alpha_evap^(x:1) * (alpha_rain*prec[1:x] + alpha_dens*H)))})

K = lambda * (1-alpha_evap)/(1 - alpha_evap^d)*
  sapply(d, function(x){return(sum(alpha_evap^(x:1-1) * (alpha_dens*prec[1:x] + alpha_rain*H)))}) # how it looks like from code

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
parms = list(omega = omega,
             h = h,
             mu_A = mu_A,
             delta_E = delta_E,
             sigma = sigma,
             gamma = gamma,
             t_s = t_s,
             temp_M = temp_M,
             temp_m = temp_m,
             t_sr = t_sr)

df <- function(t, x, parms) {
  
  # initial conditions and parameters
  with(parms, { 
    
    E = x[(1+n_s*0):(1*n_s)]
    J = x[(1+n_s*1):(2*n_s)]
    I = x[(1+n_s*2):(3*n_s)]
    A = x[(1+n_s*3):(4*n_s)]
    E_d = x[(1+n_s*4):(5*n_s)]
    
    t_n = t[1]-t_s+1 # time of numerical integration to index matrix
    t_h = 24*(t - t_n) #shoud put t and not t[1]
    ts = t_sr[t_n]
    TM = temp_M[max(1,t_n-1)]*(t_h<ts) + temp_M[t_n]*(t_h>ts)
    Tm = temp_m[t_n]*(t_h<14) + temp_M[min(t_n+1, length(temp_m))]*(t_h>14)
    
    temp_h = ((TM+Tm)/2 + (TM-Tm)/2*cos(pi*(t_h+10)/(10+ts)))*(t_h<ts)+
      ((TM+Tm)/2 - (TM-Tm)/2*cos(pi*(t_h-ts)/(14-ts)))*(t_h>ts)*(t_h<14)+
      ((TM+Tm)/2 + (TM-Tm)/2*cos(pi*(t_h-14)/(10+ts)))*(t_h>14)
    
    delta_J = 1/(83.85 - 4.89*temp_h + 0.08*temp_h^2) #juvenile development rate (in SI: 82.42 - 4.87*temp_h + 0.08*temp_h^ 2)
    delta_I = 1/(50.1 - 3.574*temp_h + 0.069*temp_h^2) #first pre blood mean rate
    mu_E = -log(0.955 * exp(-0.5*((temp_h-18.8)/21.53)^6)) # egg mortality rate
    mu_J = -log(0.977 * exp(-0.5*((temp_h-21.8)/16.6)^6)) # juvenile mortality rate
    
        
    # ODE definition 
    dE = beta[t_n]*(1-omega[t_n])*A - (h[t_n]*delta_E - mu_E)*E
    dJ = h[t_n]*(delta_E*E + sigma[t_n]*gamma[t_n]*E_d) - (delta_J + mu_J + J/K[t_n])*J  
    dI = 0.5*delta_J*J - (delta_I + mu_A[t_n])*I
    dA = delta_I*I - mu_A[t_n]*A
    dE_d = beta[t_n]*omega[t_n]*A -  h[t_n]*sigma[t_n]*E_d #I believe there should be an additional mortality due to winter
    
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

# #integration
# Sim <- as.data.frame(ode(X_0, d, df, parms))

#integration on multiple years #updated at each february
d_i = d[W_df$year == W_df$year[1]]

if (max(d)> max(d_i)){
  d_i = c(d_i, max(d_i)+ 1:min(31, max(d)-max(d_i))) #first simulation is computed until until 1st of February of second year, DOY =32
}
Sim_i <- as.data.frame(ode(X_0, d_i, df, parms))
colnames(Sim_i) = c("t", "E", "J", "I", "A", "E_d")
Sim = Sim_i
n_y = length(unique(W_df$year)) # number of years

for(y in 1:(n_y-1)){
  t_x = max(d_i)
  E_d_i = Sim_i$E_d[nrow(Sim_i)]# last eggs
  gamma_i = gamma[t_x] #
  X_0 = c(0, 0, 0, 0, E_d_i*gamma_i)
  d_i = d[which(d == max(d_i))]:min(max(d), d[which(W_df$DOY==3)[2+y]], na.rm = T)+1 #from current 1st of February to the next, if possible
  Sim_i <- as.data.frame(ode(X_0, d_i, df, parms))
  colnames(Sim_i) = c("t", "E", "J", "I", "A", "E_d")
  Sim = rbind(Sim, Sim_i)
}


#plot

Sim_m = reshape2::melt(Sim, id = 't')

ggplot(Sim_m, aes(x = t, y = value, color = variable))+
  geom_line()

#Simulated eggs (eq 4 Tran et al 2013)

Eggs_laid_sim_df <- data.frame(DOS = Sim$t[d-t_s+1],
                               eggs = beta[d]*Sim$A[d-t_s+1], #"all eggs, diapaused or not"
                               type = "laid, simulated")

#plot

Eggs_df <- Eggs_tot_df %>%
  filter(region == region_x) %>%
  filter(DOS %in% Sim$t[d-t_s+1]) %>%
  select("DOS", "eggs", "type")

# cumulate eggs over 2 weeks

Eggs_laid_sim_cum_df <-Eggs_laid_sim_df %>%
  mutate(type = "laid, simulated, cumulated") %>%
  filter(DOS %in% Eggs_df$DOS)

#if observatios every two weeks
Eggs_laid_sim_cum_df$eggs <- sapply(Eggs_df$DOS, function(x){return(sum(Eggs_laid_sim_df$eggs[(x-13):x]))}) 

Egg_comp_df <- rbind(Eggs_laid_sim_df, Eggs_df, Eggs_laid_sim_cum_df)

Egg_comp_df <- Egg_comp_df %>%
  group_by(type)%>%
  mutate(relative_eggs = eggs/max(eggs, na.rm = T))%>%
  ungroup()

ggplot(data = Egg_comp_df, aes(x = DOS, y = relative_eggs, color = type))+
  geom_line()+
  geom_point()

ggplot(data = Eggs_laid_sim_df, aes(x = DOS, y = eggs, color = type))+
  geom_line()+
  geom_point()
