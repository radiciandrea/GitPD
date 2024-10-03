# I try to code the model by Metelmann 2019
# Running on matrix in which we alter the weather variable

# EXPOSURE STATE (T, P) + PARAMETERS IN SOLVER

#T' = T + T_add
#P' = P^n/sum(P)/sum(P^n)

rm(list = ls())
gc()

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)

#load T and P

#Getting T and P and Eggs from Arpae (see ReadNc+ARPAE) + nc by Cyril

load("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_Nice_200811.RData") #Nizza

H_dens = 4800 # 4800 # humans/km2

#To be needed next: LAT and LON for each place; Human population in each pixel;
LAT = 43.5
LON = 7.3

#photoperiod Ph_P (which variables should I take? sunrise - sunset): to be modified in the future
SunTimes_df<- getSunlightTimes(data = data.frame("date" = as.Date(W_tot_df$date), "lat"= LAT, "lon" = LON))# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
Ph_P = as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)
t_sr = as.numeric(SunTimes_df$sunrise- as.POSIXct(SunTimes_df$date) +2) # time of sunrise: correction needed since time is in UTC

#T_av = 14.66509
T_add = -2:10
P_pow = seq(0.1, 2.6, by = 0.25)

W_tot_cycle_l <- vector(mode = "list", length = length(T_add)*length(P_pow))

Ind_df = data.frame(T_add = rep(NA, length(T_add)*length(P_pow )),
                   T_av = rep(NA, length(T_add)*length(P_pow )),
                   P_pow  = rep(NA, length(T_add)*length(P_pow )),
                   sdP = rep(NA, length(T_add)*length(P_pow )))

for (i in 1:length(T_add)){
  for (j in 1:length(P_pow)){
    
    k = j + (i-1)*length(P_pow )
    
    W_tot_cycle_df <- W_tot_df %>%
      mutate(T_add = T_add[i]) %>%
      mutate(T_av = T_av + T_add[i]) %>%
      mutate(P_pow  = P_pow[j]) %>%
      mutate(P = P^P_pow[j]*(sum(P)/sum(P^P_pow[j]))) %>%
      mutate(H = H_dens) %>%
      mutate(region = paste0(region, "_tadd_", T_add[i], "_Ppw_", round(P_pow , 2))) %>%
      mutate(Ph_P = Ph_P) %>%
      mutate(t_sr =t_sr)
    
    W_tot_cycle_l[[k]] <- W_tot_cycle_df
    
    Ind_df$T_add[k] = T_add[i] 
    Ind_df$T_av[k] = mean(W_tot_cycle_df$T_av)
    Ind_df$P_pow[k] = P_pow[j]
    Ind_df$sdP[k] = sd(W_tot_cycle_df$P)
    Ind_df$H[k] = W_tot_cycle_df$H[1]
  }
}

W_tot_df <-do.call("rbind", W_tot_cycle_l)
rm(W_tot_cycle_l)

#Create a matrix over which integrate; each colums is a city, each row is a date
regions = unique(W_tot_df$region)
DOS = unique(W_tot_df$DOS)

# set simualtion horizon
t_s = DOS[1] # simulate multiple year
t_end = tail(DOS, n = 1)
# t_end = 365*3+1
DOS_sim = t_s:t_end

# reduce simulation horizon and redefine DOY and DOS
W_df <- W_tot_df %>%
  filter(DOS %in% DOS_sim)

DOY = W_df$DOY[DOS_sim]
years = W_df$year[DOS_sim]
years_u = unique(years)
date = W_df$date

#dimensions
n_r = length(regions) # number of regions/locations (added; 1 for no dimension)
n_d = length(DOS_sim) # simulation length

temp = matrix(W_df$T_av, nrow = n_d)
prec = matrix(W_df$P, nrow = n_d)
H = matrix(W_df$H, nrow = n_d)

if (any(names(W_df)=="T_M")){
  temp_M <- matrix(W_df$T_M, nrow = n_d)
  temp_m <- matrix(W_df$T_m, nrow = n_d)
} else {
  cat("T_M and T_m are not available, repaced by T_av")
  temp_M <- temp
  temp_m <- temp
}

#elaborate temp and prec + sapply transpose matrices: need to t()
temp_7 = temp[1,]
temp_7 = rbind(temp_7, t(sapply(2:n_d,
                                function(x){return(colMeans(temp[max(1,(x-7)):x,]))}))) # temp of precedent 7 days
temp_min_DJF = temp_m[1,]
temp_min_DJF = rbind(temp_min_DJF, t(sapply(2:n_d,
                                            function(x){return(apply(temp_m[max(1,x-300):x, ], 2, min))}))) #min temp of last winter (daily or hours?)

Ph_P = W_df$Ph_P
t_sr = W_df$t_sr

#parameters (Metelmann 2019)
CTT_s = 11 #critical temperature over one week in spring (°C )
CPP_s = 11.25 #critical photoperiod in spring
CPP_a = 10.058 + 0.08965 * LAT # critical photperiod in autumn
sigma = 0.1 *(temp_7 > CTT_s)*(Ph_P > CPP_s) # spring hatching rate (1/day)
omega = 0.5 *(Ph_P < CPP_a)*(matrix(rep(DOY, n_r), ncol = n_r) > 183) # fraction of eggs going into diapause
delta_E = 1/7.1 #normal egg development rate (1/day)
mu_A = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
mu_A[which(temp<=0)] = -log(0.677 * exp(-0.5*((temp[which(temp<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI

gamma = 0.93*exp(-0.5*((temp_min_DJF -11.68)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
lambda = 10^6 # capacity parameter (larvae/day/ha)

# advanced parameter for carrying capacity
alpha_evap = 0.9
alpha_dens = 0.001
alpha_rain = 0.00001

eps_rat = 0.2
eps_0 = 1.5
eps_var = 0.05
eps_opt = 8
eps_dens = 0.01
eps_fac = 0.01

h = (1-eps_rat)*(1+eps_0)*exp(-eps_var*(prec-eps_opt)^2)/
  (exp(-eps_var*(prec-eps_opt)^2)+ eps_0) +
  eps_rat*eps_dens/(eps_dens + exp(-eps_fac*H)) #matrix(H, nrow = n_d, byrow = T )

df <- function(t, x, parms) {
  
  # initial conditions and parameters
  with(parms, { 
    
    E = x[(1+n_r*0):(1*n_r)]
    J = x[(1+n_r*1):(2*n_r)]
    I = x[(1+n_r*2):(3*n_r)]
    A = x[(1+n_r*3):(4*n_r)]
    E_d = x[(1+n_r*4):(5*n_r)]
    
    #t_n = t[1]-t_s+1 # time of numerical integration to index matrix
    t_n = t[1]
    t_h = 24*(t - t_n) #should put t and not t[1]
    TM = temp_M[max(1,t_n-1),]*(t_h<t_sr[t_n]) + temp_M[t_n,]*(t_h>t_sr[t_n])
    Tm = temp_m[t_n, ]*(t_h<14) + temp_M[min(t_n+1, length(temp_m))]*(t_h>14)
    
    temp_h = ((TM+Tm)/2 + (TM-Tm)/2*cos(pi*(t_h+10)/(10+t_sr[t_n])))*(t_h<t_sr[t_n])+
      ((TM+Tm)/2 - (TM-Tm)/2*cos(pi*(t_h-t_sr[t_n])/(14-t_sr[t_n])))*(t_h>t_sr[t_n])*(t_h<14)+
      ((TM+Tm)/2 + (TM-Tm)/2*cos(pi*(t_h-14)/(10+t_sr[t_n])))*(t_h>14)
    
    delta_J = 1/(83.85 - 4.89*temp_h + 0.08*temp_h^2) #juvenile development rate (in SI: 82.42 - 4.87*temp_h + 0.08*temp_h^ 2)
    delta_I = 1/(50.1 - 3.574*temp_h + 0.069*temp_h^2) #first pre blood mean rate
    mu_E = -log(0.955 * exp(-0.5*((temp_h-18.8)/21.53)^6)) # egg mortality rate
    mu_J = -log(0.977 * exp(-0.5*((temp_h-21.8)/16.6)^6)) # juvenile mortality rate
    beta = (33.2*exp(-0.5*((temp_h-70.3)/14.1)^2)*(38.8 - temp_h)^1.5)*(temp_h<= 38.8) #fertility rate
    
    # ODE definition 
    dE = beta*(1-omega[t_n, ])*A - (h[t_n, ]*delta_E + mu_E)*E
    dJ = h[t_n, ]*(delta_E*E + sigma[t_n, ]*gamma*E_d) - (delta_J + mu_J + J/K[t_n, ])*J  
    dI = 0.5*delta_J*J - (delta_I + mu_A[t_n, ])*I
    dA = delta_I*I - mu_A[t_n, ]*A
    dE_d = beta*omega[t_n, ]*A -  h[t_n, ]*sigma[t_n, ]*E_d #I believe there should be an additional mortality due to winter
    
    dx <- c(dE, dJ, dI, dA, dE_d)
    
    return(list(dx))})
}

# System initialization
E0 = rep(0, n_r)
J0 = rep(0, n_r)
I0 = rep(0, n_r)
A0 = rep(0, n_r)
E_d_0 = 10^4*rep(1, n_r) # IN THIS EXPERIMENT WE FIX Ed0 to 10^4
X_0 = c(E0, J0, I0, A0, E_d_0)

#integration on multiple years 

Sim <- matrix(nrow = length(DOS_sim), ncol = 1+n_r*5)

tic()
for (year in years_u){
  id_d_y = which(years == year)# vector of if days
  DOS_y = DOS_sim[id_d_y]
  DOY_y = DOY[id_d_y]
  source("MM_y_parameters_extraction.R")
  
  #break at 1/8 to zero diapausing eggs, even for odd years
  DOY_y_1 = DOY_y[1:(max(DOY_y)-153)] 
  Sim_y_1<- ode(X_0, DOY_y_1, df, parms)
  X_0 = c(Sim_y_1[nrow(Sim_y_1), 1+1:(n_r*4)], rep(0, n_r))
  
  DOY_y_2 = DOY_y[(max(DOY_y)-152): max(DOY_y)]
  Sim_y_2<- ode(X_0, DOY_y_2, df, parms)
  
  #break at 31/12 to zero everything except diapausing eggs
  Sim[id_d_y,] = rbind(Sim_y_1, Sim_y_2)
  X_0 = c(rep(0, n_r*4), Sim_y_2[nrow(Sim_y_2), 1+(n_r*4+1):(n_r*5)])
}
toc()

Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
                      "region" = rep(rep(regions, each = max(DOS_sim)), 5),
                      "t" = rep(DOS_sim, n_r*5),
                      "value" = c(Sim[, 2:(1+5*n_r)])) #5 classes


#E0
Ed = Sim[nrow(Sim), 1+(n_r*4+1):(n_r*5)]
E0 = (pmax(Ed, 0)/E_d_0)^(1/length(years_u))

Ind_df$E0 = E0 

#Adults in summer mjjas

Adults <- Sim[, 1+(n_r*3+1):(n_r*4)]

i_mjjas <- which(as.numeric(substr(W_tot_cycle_df$date, 7, 7)) %in% 5:9)

Ad <- colMeans(Adults[i_mjjas,])

Ind_df$Ad = Ad 

# Average R0 in in summer mjjas
m <- Adults/H
b_H2v_DG = 0.31 # beta Mtl 2021
b_v2H = 0.5 # b Blagrove 2020
a = (0.0043*temp + 0.0943)/2
phi_a = 0.9 #human biting preference (urban)
IIP_DG = 5 #Benkimoun 2021
EIP_DG = 1.03*(4*exp(5.15 - 0.123*temp)) #Metelmann 2021
R0_DG = (a*phi_a)^2*m/(mu_A+mu_A^2*EIP_DG)*b_v2H*b_H2v_DG*IIP_DG # as Zanardini et al.

R0 = colMeans(R0_DG[i_mjjas,]) # ndays R0>1
Ind_df$R0 = R0

# #https://hihayk.github.io/scale/#4/7/40/36/-50/151/0/14/F8C358/248/195/91/white
# # Ind_df <- Ind_df %>%
# #   mutate(E0_level=cut(E0, breaks=c(-1, 10^-10, 0.01, 0.1, 1, 10, 20, 50, 100),
# #                       labels=c("0", "0-0.01", "0.01-0.1", "0.1-1", "1-10", "10-20", "20-50", "50-100"))) %>%
# #   mutate(E0_level=factor(as.character(E0_level), levels=rev(levels(E0_level))))
# # 
# # ggplot() + 
# #   geom_tile(data = Ind_df, aes(x = T_av, y = sdP, fill= E0_level))+
# #   scale_fill_manual(values = rev(c("#007917", "#65992E", "#8FB338", "#E2CF4D", "#F89061", "#F96970", "#F97ADC", "#A494FB")))+
# #   theme_test()
# 
# ggplot() + 
#   geom_tile(data = Ind_df, aes(x = T_add, y = P_pow, fill= E0))+
#   scale_fill_viridis()+
#   theme_test()

#https://ggplot2.tidyverse.org/reference/geom_contour.html
ggplot()+
  geom_contour_filled(data = Ind_df, aes(x = T_add, y = P_pow, z = E0))+
  ggtitle("E0")+
  theme_test()

ggplot()+
  geom_contour_filled(data = Ind_df, aes(x = T_add, y = P_pow, z = Ad))+
  ggtitle("Average adults/ha between may and september")+
  theme_test()

ggplot()+
  geom_contour_filled(data = Ind_df, aes(x = T_add, y = P_pow, z = R0))+
  ggtitle("Average R0  between may and september")+
  theme_test()

