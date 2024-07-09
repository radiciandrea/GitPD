# I try to code the model by Metelmann 2019
# Running on matrix EOBS SEl
# Here the model works with day-varying temperature 

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
year_f = "2011"
#Getting weather from EOBS
load(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sel_2011_", year_f, "_", name, ".RData")) #EOBS domain_sel_W_EU #Occitanie #France

#Create a matrix over which integrate; each colums is a city, each row is a date
DOS = unique(W_tot_df$DOS)

# set simualtion horizon
t_s = DOS[1] # simulate multiple year
t_end = tail(DOS, n = 1)
#t_end = 365*3+1
DOS_sim = t_s:t_end

# reduce simulation horizon and redefine DOY and DOS
W_df <- W_tot_df %>%
  filter(DOS %in% DOS_sim)

DOY = W_df$DOY[DOS_sim]
years = W_df$year[DOS_sim]
years_u = unique(years)
date = W_df$date

# distinct space
regions_df <- W_df %>% distinct(region, .keep_all = TRUE) %>%
  dplyr::select(c("region","r_i","r_j", "lon", "lat", "pop"))

regions = regions_df$region

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

# lat and lon
LAT = regions_df$lat
LON = regions_df$lon
#H = rep(100, n_r) #human population density per km² SO FAR
H =   matrix(rep(regions_df$pop, n_d), nrow = n_d, byrow = T ) 

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
  eps_rat*eps_dens/(eps_dens + exp(-eps_fac*H))

# System initialization
E0 = rep(0, n_r)
J0 = rep(0, n_r)
I0 = rep(0, n_r)
A0 = rep(0, n_r)
E_d_0 = 1*rep(1, n_r) # at 1st of January (10^6)

X_0 = c(E0, J0, I0, A0, E_d_0)

source("MM_integration_functions.R")

#integration on multiple years 

Sim <- matrix(nrow = length(DOS_sim), ncol = 1+n_r*5)

#rk integration step
is = 1/48 #24 or 48 or 100

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
  
  X_0_log = X_0
  X_0_log[1:(n_r*4)] = log(X_0[1:(n_r*4)])
  
  DOY_y_2_sim = seq((max(DOY_y)-152), max(DOY_y), by = is)
  Sim_y_2_sim<- deSolve::rk4(X_0_log, DOY_y_2_sim, df_log, parms)
  Sim_y_2 <-Sim_y_2_sim[1+(0:152)/is,]
  Sim_y_2[, 1+1:(n_r*4)] = exp(Sim_y_2[, 1+1:(n_r*4)])
  # DOY_y_2 = DOY_y[(max(DOY_y)-152): max(DOY_y)]
  # Sim_y_2<- ode(X_0, DOY_y_2, df, parms, hmin = is)
  X_0 = c(rep(0, n_r*4), Sim_y_2[nrow(Sim_y_2), 1+(n_r*4+1):(n_r*5)])
  
  #break at 31/12 to zero everything except diapausing eggs
  Sim[id_d_y,] = rbind(Sim_y_1, Sim_y_2)
}
toc()

E0_v = (pmax(Sim[nrow(Sim), 1+(n_r*4+1):(n_r*5)], 0)/E_d_0)^(1/length(years_u))
#E0_v = (pmax(Sim[nrow(Sim), 1+1:n_r], 0)/E_d_0)

log10(max(E0_v, na.rm=T))

#da trasferire in un altro file


domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

br = c(0, 10^(-3:3), 10^10)

#Metelmann map
#br = c(0, 10^(-3:3), 10^10)
#col_br= c("#384AB9", "#4F71D4", "#8CB1FF", "#8CB1FF", "#8CB1FF", "#8CB1FF", "#8CB1FF", "#8CB1FF")

domain_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0 = E0_v)%>%
  mutate(E0_level=cut(E0, breaks=br,
                      labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_level=factor(as.character(E0_level), levels=rev(levels(E0_level))))

regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")

if (name == "Occitanie") {
  regions_sh <- regions_sh %>%
    filter(Region == "Languedoc-Roussillon et Midi-P")
}


ggplot()+
  geom_sf(data = domain_sel, aes(fill = E0_level))+ #
  scale_fill_manual(values = rev(c("#007917", "#65992E", "#8FB338", "#E2CF4D", "#F89061", "#F96970", "#F97ADC", "#A494FB")))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")
  # + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = E0_v))+ #E0_v
  scale_fill_gradient(trans = "log")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")


#plot pop
Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
                      "region" = rep(rep(regions, each = max(DOS_sim)), 5),
                      "t" = rep(DOS_sim, n_r*5),
                      "value" = c(Sim[, 2:(1+5*n_r)])) #5 classes

# st_write(domain_sel, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/res_sim_2011_", name, ".shp"))
#plot
id_reg = 340 #Montpellier = 93 in Occitanie #340 cella maledetta

region_x = regions[id_reg]

Sim_m_x_df <- Sim_m_df %>%
  filter(region == region_x)
Sim_x_df<- dcast(Sim_m_x_df, t ~ variable)

ggplot(Sim_m_x_df, aes(x = t, y = value, color = variable))+
  geom_line()+
  scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_x_df$value)))+
  # ylim(1, max(Sim_m_x_df$value))+
  # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
  labs(color = paste0("Abundances per classes (", region_x, ")")) +
  theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),

# compute laid eggs: change into integration function #beta should be calculatedd hour by hour
