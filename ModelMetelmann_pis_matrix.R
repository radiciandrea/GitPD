# I try to code the model by Metelmann 2019
# Running on matrix
# Here the model works with day-varying temperature 

rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)

#load T and P

#Getting T and P and Eggs from Arpae (see ReadNc+ARPAE) + nc by Cyril

#load("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Weather_Nice_200811.RData") #Nizza
load("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather_ER_20112021.RData") #Emilia Romagna

#Create a matrix over which integrate; each colums is a city, each row is a date
regions = unique(W_tot_df$region)
DOS = unique(W_tot_df$DOS)

# set simualtion horizon
t_s = DOS[1] # simulate multiple year
t_end = tail(DOS, n = 1)
t_end = 365
DOS_sim = t_s:t_end

# reduce simulation horizon and redefine DOY and DOS
W_df <- W_tot_df %>%
  filter(DOS %in% DOS_sim)

DOY = W_df$DOY[DOS_sim]
years = W_df$year[DOS_sim]
date = W_df$date

#dimensions
n_r = length(regions) # number of regions/locations (added; 1 for no dimension)
n_d = length(DOS_sim) # simulation length

temp = matrix(W_df$T_av, nrow = n_d)
prec = matrix(W_df$P, nrow = n_d)
  
if (any(names(W_df)=="T_M")){
  temp_M <- matrix(W_df$T_M, nrow = n_d)
  temp_m <- matrix(W_df$T_m, nrow = n_d)
} else {
  cat("T_M and T_m are not available, repaced by T_av")
  temp_M <- temp
  temp_m <- temp
}

#To be needed next: LAT and LON for each place; Human population in each pixel;
LAT = 44.5*rep(1, n_r)
LON = 11.5*rep(1, n_r)
H = 1000*c(2.78, 0.3, 0.4, 1, 0.8, 0.9, 0.2, 0.7, 1.1) #human population density per km² in E R

#elaborate temp and prec + sapply transpose matrices: need to t()
temp_7 = temp[1,]
temp_7 = rbind(temp_7, t(sapply(2:n_d,
                                function(x){return(colMeans(temp[max(1,(x-7)):x,]))}))) # temp of precedent 7 days
temp_min_DJF = temp[1,]
temp_min_DJF = rbind(temp_min_DJF, t(sapply(2:n_d,
                                            function(x){return(apply(temp[max(1,x-300):x, ], 2, min))}))) #min temp of last winter (daily or hours?)

#photoperiod Ph_P (which variables should I take? sunrise - sunset): to be modified in the future
SunTimes_df<- getSunlightTimes(data = data.frame("date" = as.Date(W_df$date), "lat"= rep(LAT, n_d), "lon" = rep(LON, n_d)))# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
Ph_P = as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)
t_sr = as.numeric(SunTimes_df$sunrise- as.POSIXct(SunTimes_df$date) +2) # time of sunrise: correction needed since time is in UTC

Ph_P = matrix(Ph_P, nrow = n_d)
t_sr = matrix(t_sr, nrow = n_d)

#parameters (Metelmann 2019)
CTT_s = 11 #critical temperature over one week in spring (°C )
CPP_s = 11.25 #critical photoperiod in spring
CPP_a = 10.058 + 0.08965 * LAT # critical photperiod in autumn
sigma = 0.1 *(temp_7 > CTT_s)*(Ph_P > CPP_s) # spring hatching rate (1/day)
omega = 0.5 *(Ph_P < CPP_a)*(matrix(rep(DOY, n_r), ncol = n_r) > 183) # fraction of eggs going into diapause
delta_E = 1/7.1 #normal egg development rate (1/day)
mu_A = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
mu_A[which(is.na(mu_A))] = -log(0.677 * exp(-0.5*((temp[which(is.na(mu_A))]-20.9)/13.2)^6)) #correct the problems due to negative values from SI

gamma = 0.93*exp(-0.5*((temp_min_DJF -11.68)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
lambda = 10^6 # capacity parameter (larvae/day/ha)

# advanced parameter for carrying capacity
alpha_evap = 0.9
alpha_dens = 0.001
alpha_rain = 0.00001

K = sapply(1:n_r, function(y){return(lambda * (1-alpha_evap)/(1 - alpha_evap^DOS_sim)*
                                       sapply(DOS_sim, function(x){return(sum(alpha_evap^(x:1-1) * (alpha_dens*prec[1:x,y] + alpha_rain*H[y])))}))
}) # this maxes the code to abort quite often.
  
# advanced parameter for hatching
eps_rat = 0.2
eps_0 = 1.5
eps_var = 0.05
eps_opt = 8
eps_dens = 0.01
eps_fac = 0.01

h = (1-eps_rat)*(1+eps_0)*exp(-eps_var*(prec-eps_opt)^2)/
  (exp(-eps_var*(prec-eps_opt)^2)+ eps_0) +
  eps_rat*eps_dens/(eps_dens + exp(-eps_fac*matrix(rep(H, n_d), nrow = n_d, byrow = T )))

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
    
    E = x[(1+n_r*0):(1*n_r)]
    J = x[(1+n_r*1):(2*n_r)]
    I = x[(1+n_r*2):(3*n_r)]
    A = x[(1+n_r*3):(4*n_r)]
    E_d = x[(1+n_r*4):(5*n_r)]
    
    #t_n = t[1]-t_s+1 # time of numerical integration to index matrix
    t_n = t[1]
    t_h = 24*(t - t_n) #should put t and not t[1]
    TM = temp_M[max(1,t_n-1),]*(t_h<t_sr[t_n, ]) + temp_M[t_n,]*(t_h>t_sr[t_n, ])
    Tm = temp_m[t_n, ]*(t_h<14) + temp_M[min(t_n+1, length(temp_m))]*(t_h>14)
    
    temp_h = ((TM+Tm)/2 + (TM-Tm)/2*cos(pi*(t_h+10)/(10+t_sr[t_n, ])))*(t_h<t_sr[t_n, ])+
      ((TM+Tm)/2 - (TM-Tm)/2*cos(pi*(t_h-t_sr[t_n, ])/(14-t_sr[t_n, ])))*(t_h>t_sr[t_n, ])*(t_h<14)+
      ((TM+Tm)/2 + (TM-Tm)/2*cos(pi*(t_h-14)/(10+t_sr[t_n, ])))*(t_h>14)
    
    delta_J = 1/(83.85 - 4.89*temp_h + 0.08*temp_h^2) #juvenile development rate (in SI: 82.42 - 4.87*temp_h + 0.08*temp_h^ 2)
    delta_I = 1/(50.1 - 3.574*temp_h + 0.069*temp_h^2) #first pre blood mean rate
    mu_E = -log(0.955 * exp(-0.5*((temp_h-18.8)/21.53)^6)) # egg mortality rate
    mu_J = -log(0.977 * exp(-0.5*((temp_h-21.8)/16.6)^6)) # juvenile mortality rate
    beta = (33.2*exp(-0.5*((temp_h-70.3)/14.1)^2)*(38.8 - temp_h)^1.5)*(temp_h<= 38.8) #fertility rate
    
    
    # ODE definition 
    dE = beta*(1-omega[t_n, ])*A - (h[t_n, ]*delta_E - mu_E)*E
    dJ = h[t_n, ]*(delta_E*E + sigma[t_n, ]*gamma[t_n, ]*E_d) - (delta_J + mu_J + J/K[t_n, ])*J  
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
E_d_0 = 10^6*rep(1, n_r) # at 1st of January (10^6)

X_0 = c(E0, J0, I0, A0, E_d_0)

# #integration
# Sim <- as.data.frame(ode(X_0, d, df, parms))

# following: to be modified

#integration on multiple years #updated at each february
d_i = DOS_sim[years == years[1]]

if (n_d> max(d_i)){
  d_i = c(d_i, max(d_i)+ 1:min(31, n_d-max(d_i))) #first simulation is computed until until 1st of February of second year, DOY =32
}
Sim_i <- as.data.frame(ode(X_0, d_i, df, parms))
colnames(Sim_i) = c("t", "E", "J", "I", "A", "E_d")
Sim = Sim_i
n_y = length(unique(years)) # number of years

for(y in 1:(n_y-1)){
  t_x = max(d_i)
  E_d_i = Sim_i$E_d[nrow(Sim_i)]# last eggs
  gamma_i = gamma[t_x] #
  X_0 = c(0, 0, 0, 0, E_d_i*gamma_i)
  d_i = d[which(DOS_sim == max(d_i))]:min(n_d, DOS_sim[which(W_df$DOY==3)[2+y]], na.rm = T)+1 #from current 1st of February to the next, if possible
  Sim_i <- as.data.frame(ode(X_0, d_i, df, parms))
  colnames(Sim_i) = c("t", "E", "J", "I", "A", "E_d")
  Sim = rbind(Sim, Sim_i)
}


#plot

Sim_m = reshape2::melt(Sim, id = 't')

ggplot(Sim_m, aes(x = t, y = value, color = variable))+
  geom_line()

#Simulated eggs (eq 4 Tran et al 2013)

Eggs_laid_sim_df <- data.frame(DOS = Sim$t[DOS_sim],
                               eggs = beta[DOS_sim]*Sim$A[DOS_sim], #"all eggs, diapaused or not"
                               type = "laid, simulated")

#plot

Eggs_df <- Eggs_tot_df %>%
  filter(region == region_x) %>%
  filter(DOS %in% Sim$t[DOS_sim]) %>%
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
