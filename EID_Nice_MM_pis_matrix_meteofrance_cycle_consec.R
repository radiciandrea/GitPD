# I try to code the model by Metelmann 2019
# Running on MeteoFrance (Nice)
# Here the model works with day-varying temperature 

# Same settings of homonimous file

rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)
library(sf)

#load T and P

name = "France"

years = 2007:2023

#load first MeteoFrance to get lon lat
folder_meteofrance = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_elab"
folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_elab_consec"

dir.create(folder_out)

#Getting weather from MeteoFrance
load(paste0(folder_meteofrance, "/Nice_", years[1], "_", name, ".RData")) #Nice #France

# distinct space
regions_df <- W_tot_df %>% 
  distinct(region, .keep_all = TRUE) %>%
  dplyr::select(c("region","lon", "lat", "pop"))

regions = regions_df$region
n_r = length(regions) # number of regions/locations (added; 1 for no dimension)

# lat and lon
LAT = regions_df$lat
LON = regions_df$lon

# Model time independent parameters

#parameters (Metelmann 2019)
CTT_s = 11 #critical temperature over one week in spring (°C )
CPP_s = 11.25 #critical photoperiod in spring
CPP_a = 10.058 + 0.08965 * LAT # critical photperiod in autumn
delta_E = 1/7.1 #normal egg development rate (1/day)
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

# System initialization
E0 = rep(0, n_r)
J0 = rep(0, n_r)
I0 = rep(0, n_r)
A0 = rep(0, n_r)
E_d_0 = 1*rep(1, n_r) # at 1st of January (10^6)

#integration step
is_1 = 1/24
is_2 = 1/60

#distinguisch between condition of first year (00) and of every year
X_0 = c(E0, J0, I0, A0, E_d_0)

for (year in years){
  
  #getting weather from MeteoFrance <- previous year
  load(paste0(folder_meteofrance, "/Nice_", year-1, "_", name, ".RData")) #MeteoFrance W_EU #Occitanie #France
  
  #Extract only temp in December
  W_D_df <- W_tot_df %>%
    filter(DOY >= (max(DOY)-30))
  
  #Getting weather from MeteoFrance
  load(paste0(folder_meteofrance, "/Nice_", year, "_", name, ".RData")) #MeteoFrance W_EU #Occitanie #France
  
  #Create a matrix over which integrate; each colums is a city, each row is a date
  DOS_y = unique(W_tot_df$DOS)
  DOY_y = unique(W_tot_df$DOY)
  
  # set simualtion horizon
  t_s = DOS_y[1] # simulate multiple year
  t_end = tail(DOS_y, n = 1)
  
  date = W_tot_df$date
  
  #dimensions
  n_d = length(DOS_y) # simulation length
  
  #exctract params
  temp = matrix(W_tot_df$T_av, nrow = n_d)
  
  #LITTLE CORRECTION (there's a NaN in temperatures)
  id_temp_na <- which(is.na(temp))
  temp[id_temp_na] = 0.5*(temp[id_temp_na-1]+temp[id_temp_na+1])
  
  prec = matrix(W_tot_df$P, nrow = n_d)
  
  # we simplified the following, (max(DOY_y)-306), to 61 to reduce complexity with unfinished years
  temp_DJF = rbind(matrix(W_D_df$T_m, nrow = 31),
                   matrix(W_tot_df$T_m[which(W_tot_df$DOY <= 60)], nrow = 60))
  
  if (any(names(W_tot_df)=="T_M")){
    temp_M <- matrix(W_tot_df$T_M, nrow = n_d)
    temp_m <- matrix(W_tot_df$T_m, nrow = n_d)
  } else {
    cat("T_M and T_m are not available, repaced by T_av")
    temp_M <- temp
    temp_m <- temp
  }
  
  #rehape human matrix
  H =   matrix(rep(regions_df$pop, n_d), nrow = n_d, byrow = T ) 
  
  #elaborate temp and prec + sapply transpose matrices: need to t()
  # temp_7 = temp[1,]
  # temp_7 = rbind(temp_7, t(sapply(2:n_d,
  #                                 function(x){return(colMeans(temp[max(1,(x-7)):x,]))}))) # temp of precedent 7 days
  
  #previous not working for vectors: 
  temp_7 = sapply(1:length(temp), function(x){return(mean(temp[max(1,x-7):x]))}) # temp of precedent 7 days
  
  temp_min_DJF = apply(temp_DJF, 2, function(x){min(x)}) #min temp of last winter (daily or hours?)
  
  # #photoperiod Ph_P (which variables should I take? sunrise - sunset): to be modified in the future
  SunTimes_df<- getSunlightTimes(data = data.frame("date" = as.Date(W_tot_df$date), "lat"= rep(LAT, n_d), "lon" = rep(LON, n_d)))# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
  Ph_P = as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)
  t_sr = as.numeric(SunTimes_df$sunrise- as.POSIXct(SunTimes_df$date) +2) # time of sunrise: correction needed since time is in UTC

  # # Daylight model from SI Metelmann: faster
  # theta = 0.2163108 + 2*atan(0.9671396*tan(0.0086*(W_tot_df$DOY - 186))) 
  # phi = asin(0.39795 *cos(theta)) 
  # 
  # Ph_P = 24-24/pi*acos(sin(pi*rep(LAT,n_d)/180)*sin(phi)/(cos(pi*rep(LAT,n_d)/180)*cos(phi)))
  # t_sr = 12/pi*acos(sin(pi*rep(LAT,n_d)/180)*sin(phi)/(cos(pi*rep(LAT,n_d)/180)*cos(phi))) 
  
  Ph_P = matrix(Ph_P, nrow = n_d, byrow = T)
  t_sr = matrix(t_sr, nrow = n_d, byrow = T)
  
  #parameters (Metelmann 2019)
  sigma = 0.1 *(temp_7 > CTT_s)*(Ph_P > CPP_s) # spring hatching rate (1/day)
  omega = 0.5 *(Ph_P < CPP_a)*(matrix(rep(DOY_y, n_r), ncol = n_r) > 183) # fraction of eggs going into diapause
  mu_A = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
  mu_A[which(temp<=0)] = -log(0.677 * exp(-0.5*((temp[which(temp<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  gamma = 0.93*exp(-0.5*((temp_min_DJF -11.68)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
  
  h = (1-eps_rat)*(1+eps_0)*exp(-eps_var*(prec-eps_opt)^2)/
    (exp(-eps_var*(prec-eps_opt)^2)+ eps_0) +
    eps_rat*eps_dens/(eps_dens + exp(-eps_fac*H))
  
  # Compute K
  K = sapply(1:n_r, function(y){return(lambda * (1-alpha_evap)/(1 - alpha_evap^DOY_y)*
                                         sapply(DOY_y, function(x){return(sum(alpha_evap^(x:1-1) * (alpha_dens*prec[1:x,y] + alpha_rain*H[x,y])))}))
  }) 
  
  source("MM_integration_functions.R")
  
  tic() #previous cycle
  parms = list(omega = omega,
               h = h,
               K = K,
               mu_A = mu_A,
               delta_E = delta_E,
               sigma = sigma,
               gamma = gamma,
               temp_M = temp_M,
               temp_m = temp_m)
  
  #break at 1/8 to zero diapausing eggs, even for odd years
  
  #DOY_y_1 = DOY_y[1:(max(DOY_y)-153)]
  #Sim_y_1<- ode(X_0, DOY_y_1, df, parms)
  
  #here smooth
  
  #simplify (max(DOY_y)-153) with 212
  
  
  DOY_y_1_sim = seq(1, min(max(DOY_y), 212), by = is_1)
  Sim_y_1_sim<- deSolve::rk4(X_0, DOY_y_1_sim, df, parms)
  Sim_y_1 <-Sim_y_1_sim[1+(0:min(max(DOY_y)-1, 211))/is_1,]
  
  X_0 = c(Sim_y_1[nrow(Sim_y_1), 1+1:(n_r*4)], rep(0, n_r))
  
  
  #uncomment to run as lsoda instead of rk
  # DOY_y_2 = DOY_y[(max(DOY_y)-152): max(DOY_y)]
  # Sim_y_2<- ode(X_0, DOY_y_2, df, parms)
  # X_0 = c(rep(0, n_r*4), Sim_y_2[nrow(Sim_y_2), 1+(n_r*4+1):(n_r*5)])
  
  #correct uncompleted years
  
  if (max(DOY_y) > 212){
    X_0_log = X_0
    X_0_log[1:(n_r*4)] = log(X_0[1:(n_r*4)])
    DOY_y_2_sim = seq(213, max(DOY_y), by = is_2)
    Sim_y_2_sim<- deSolve::rk4(X_0_log, DOY_y_2_sim, df_log, parms)
    Sim_y_2 <-Sim_y_2_sim[1+(0:(max(DOY_y)-213))/is_2,]
    Sim_y_2[, 1+1:(n_r*4)] = exp(Sim_y_2[, 1+1:(n_r*4)])
    
    #break at 31/12 to zero everything except diapausing eggs
    Sim = rbind(Sim_y_1, Sim_y_2)
  } else {
    Sim = Sim_y_1
  }
  
  E0_v = pmax(Sim[nrow(Sim), 1+(n_r*4+1):(n_r*5)], 0)/E_d_0
  
  # Compute beta_approx
  beta_approx = (33.2*exp(-0.5*((temp-70.3)/14.1)^2)*(38.8 - temp)^1.5)*(temp<= 38.8) #fertility rate
  
  save(Sim, E0_v, beta_approx, file = paste0(folder_out, "/Sim_Nice_", name, "_", year, ".RData"))
  
  E_d_0_y = pmax(1, Sim_y_2[nrow(Sim_y_2), 1+(n_r*4+1):(n_r*5)])
  X_0 = c(rep(0, n_r*4), E_d_0_y)
  
  toc()
}