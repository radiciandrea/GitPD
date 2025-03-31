# I try to code the model by Metelmann 2019
# Running on matrix in which we alter the weather variable

# EXPOSURE STATE (T, P) + PARAMETERS IN SOLVER

#T' = T + T_add
#P' = P*P_mol in summer

rm(list = ls())
gc()

library(deSolve)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)
library(data.table)
library(pracma)

#Plot
library(ggplot2)
library(metR)
library(ggrepel)
library(ggpubr)


#load T and P

#Getting T and P and Eggs from Arpae (see ReadNc+ARPAE) + nc by Cyril

# load("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_Nice_200811.RData") #Nizza
# 
# H_dens = 4800 # 4800 # humans/km2
# 
# #To be needed next: LAT and LON for each place; Human population in each pixel;
# LAT = 43.5
# LON = 7.3

cities = c("Montpellier", "Paris", "Paris_S", "Madrid", "Rome_E", 
           "London_N", "Berlin", "Lisbon", "Lyon", "Barcelona", 
           "Milan", "Zurich", "Munich", "Bruxelles", "Amsterdam", 
           "Sicily (Catania)", "Vienna", "Copenhagen", "Praga", 
           "Ljubjiana", "Zagreb", "Stockhom", "Oslo", "Dublin")

cities = c("Viterbo",
           "Coimbra", "Firenze", "Lugano", "Frankfurt aum Main", "Innsbruck",
           "Antwerp")

cities = c("Montpellier", "Bilbao", "Augsburg", "Paris-centre", "Paris-suburbs", "Paris-region")

cities = c("Montpellier", "Rennes", "Strasbourg")

cities = c("Montpellier", "Toulouse", "Rodez")

cities = c("Antwerp")

cities =c("Marseille", "Avignon", "Gap")

rk = "on" #on if integration crashes! (it is way slower)
folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Exposure_state_mol/"

years = 2000:2023

#settings
#years ref
years_u = 2019:2023

# advanced parameter for carrying capacity_x
alpha_evap = 0.9
alpha_dens = 0.001
alpha_rain = 0.00001

eps_rat = 0.2
eps_0 = 1.5
eps_var = 0.05
eps_opt = 8
eps_dens = 0.01
eps_fac = 0.01

for(city_x in cities){
  
  load(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_EOBS_",
              city_x, "_", min(years), "_", max(years), ".RData"))
  
  LAT = W_tot_df$lat[1]
  LON = W_tot_df$lon[1]
  H_dens = W_tot_df$pop[1]
  
  #recompute years for simulation
  W_tot_df0 <- W_tot_df
  W_tot_df <-  W_tot_df %>% filter(year %in% years_u)
  W_tot_df$DOS <- 1:nrow(W_tot_df)
  
  #photoperiod Ph_P (which variables should I take? sunrise - sunset): to be modified in the future
  SunTimes_df<- getSunlightTimes(data = data.frame("date" = as.Date(W_tot_df$date), "lat"= LAT, "lon" = LON))# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
  Ph_P = as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)
  t_sr = as.numeric(SunTimes_df$sunrise- as.POSIXct(SunTimes_df$date) +2) # time of sunrise: correction needed since time is in UTC
  
  #T_av = 14.66509
  T_add = seq(-2, 8, by = 0.5)
  P_mol = seq(0, 2, by = 1/6)
  
  # #change P_mol: determined Newton Rapson method
  # P_summ = W_tot_df %>% 
  #   filter(DOY >= 121) %>% #1st may
  #   filter(DOY < 274) %>% #1st of october
  #   pull(P)
  
  W_tot_cycle_l <- vector(mode = "list", length = length(T_add)*length(P_mol))
  
  Ind_df = data.frame(T_add = rep(NA, length(T_add)*length(P_mol )),
                      T_av = rep(NA, length(T_add)*length(P_mol )),
                      T_av_summer =rep(NA, length(T_add)*length(P_mol )),
                      P_mol  = rep(NA, length(T_add)*length(P_mol )),
                      P_summ_tot = rep(NA, length(T_add)*length(P_mol )))
  
  for (i in 1:length(T_add)){
    for (j in 1:length(P_mol)){
      
      k = j + (i-1)*length(P_mol)
      
      W_tot_cycle_df <- W_tot_df %>%
        mutate(T_add = T_add[i]) %>%
        mutate(T_av = T_av + T_add[i]) %>%
        mutate(T_m = T_m + T_add[i]) %>%
        mutate(T_M = T_M + T_add[i]) %>%
        mutate(P_summ = P*(DOY >= 121)*(DOY < 304)) %>%
        mutate(P_mol  = P_mol[j]) %>%
        mutate(P_summ = P_summ*P_mol[j]) %>%
        mutate(P = P_summ*(DOY >= 121)*(DOY < 304) + P*(DOY < 121) + P*(DOY >= 304))%>%
        mutate(H = H_dens) %>%
        mutate(region = paste0(region, "_tadd_", T_add[i], "_Pmol_", round(P_mol[j], 2))) %>%
        mutate(Ph_P = Ph_P) %>%
        mutate(t_sr =t_sr)
      
      
      W_tot_cycle_l[[k]] <- W_tot_cycle_df
      
      Ind_df$T_add[k] = T_add[i] 
      Ind_df$T_av[k] = mean(W_tot_cycle_df$T_av)
      Ind_df$T_av_summer[k] = W_tot_cycle_df %>%
        group_by(year) %>%
        filter(DOY >= 121) %>% #1st may
        filter(DOY < 304) %>% #1st of november
        ungroup() %>%
        dplyr::summarize(T_av_summer = mean(T_av)) %>%
        pull(T_av_summer)
      
      Ind_df$P_mol[k] = P_mol[j]
      Ind_df$P_summ_tot[k] = W_tot_cycle_df %>%
        group_by(year) %>%
        filter(DOY >= 121) %>% #1st may
        filter(DOY < 304) %>% #1st of october
        ungroup() %>%
        dplyr::summarize(P_summ_tot = sum(P)/length(years_u)) %>%
        pull(P_summ_tot)
      Ind_df$H[k] = W_tot_cycle_df$H[1]
    }
  }
  
  W_tot_df <-rbindlist(W_tot_cycle_l)
  rm(W_tot_cycle_l)
  
  #Create a matrix over which integrate; each colums is a city_x, each row is a date
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
  years_rep = W_df$year[DOS_sim]
  
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
  lambda = 10^6 # capacity_x parameter (larvae/day/ha)
  
  h = (1-eps_rat)*(1+eps_0)*exp(-eps_var*(prec-eps_opt)^2)/
    (exp(-eps_var*(prec-eps_opt)^2)+ eps_0) +
    eps_rat*eps_dens/(eps_dens + exp(-eps_fac*H))
  
  source("MM_integration_functions_exp_state.R")
  
  #integration step
  is = 1/60
  
  #### exp 2: E_d_0 == 1
  
  # System initialization
  E0 = rep(0, n_r)
  J0 = rep(0, n_r)
  I0 = rep(0, n_r)
  A0 = rep(0, n_r)
  E_d_0 = 1*rep(1, n_r) # IN THIS EXPERIMENT WE FIX Ed0 to 1
  X_00 = c(E0, J0, I0, A0, E_d_0)
  
  #integration on multiple years 
  Sim <- matrix(nrow = length(DOS_sim), ncol = 1+n_r*5)
  Ed_m = matrix(NA, nrow = length(years_u), ncol = n_r)
  
  tic()
  for (year in years_u){
    X_0 = X_00
    
    id_d_y = which(years_rep == year)# vector of if days
    DOS_y = DOS_sim[id_d_y]
    DOY_y = DOY[id_d_y]
    source("MM_y_parameters_extraction.R")
    
    #break at 1/8 to zero diapausing eggs, even for odd years
    
    if(rk == "on"){
      
      #log system
      DOY_y_1_sim = seq(1, (max(DOY_y)-153), by = is)
      Sim_y_1_sim<- deSolve::rk4(X_0, DOY_y_1_sim, df, parms)
      Sim_y_1 <-Sim_y_1_sim[1+(0:(max(DOY_y)-154))/is,]
      
      X_0 = c(Sim_y_1[nrow(Sim_y_1), 1+1:(n_r*4)], rep(0, n_r))
      
      X_0_log = X_0
      X_0_log[1:(n_r*4)] = log(X_0[1:(n_r*4)])
      DOY_y_2_sim = seq((max(DOY_y)-152), max(DOY_y), by = is)
      Sim_y_2_sim<- deSolve::rk4(X_0_log, DOY_y_2_sim, df_log, parms)
      Sim_y_2 <-Sim_y_2_sim[1+(0:152)/is,]
      Sim_y_2[, 1+1:(n_r*4)] = exp(Sim_y_2[, 1+1:(n_r*4)])
      
    } else {
      DOY_y_1 = DOY_y[1:(max(DOY_y)-153)]
      Sim_y_1<- ode(X_0, DOY_y_1, df, parms)
      
      X_0 = c(Sim_y_1[nrow(Sim_y_1), 1+1:(n_r*4)], rep(0, n_r))
      
      DOY_y_2 = DOY_y[(max(DOY_y)-152): max(DOY_y)]
      Sim_y_2<- ode(X_0, DOY_y_2, df, parms)
      
    }
    
    Ed_m[which(years_u ==  year),] = Sim_y_2[nrow(Sim_y_2), 1+(n_r*4+1):(n_r*5)]
  
  }
  toc()
  
  #E0

  E0 = apply(Ed_m, 2, function(x){exp(mean(log(x)))})
  
  Ind_df$E0 = E0 
  
  #### exp 2: E_d_0 != 2
  
  # System initialization
  E_d_0 = 10^4*rep(1, n_r) # IN THIS EXPERIMENT WE FIX Ed0 to 10^4
  X_0 = c(E0, J0, I0, A0, E_d_0)
  
  #integration on multiple years 
  Sim <- matrix(nrow = length(DOS_sim), ncol = 1+n_r*5)
  
  tic()
  for (year in years_u){
    id_d_y = which(years_rep == year)# vector of if days
    DOS_y = DOS_sim[id_d_y]
    DOY_y = DOY[id_d_y]
    source("MM_y_parameters_extraction.R")
    
    #break at 1/8 to zero diapausing eggs, even for odd years
    
    if(rk == "on"){
      
      #log system
      DOY_y_1_sim = seq(1, (max(DOY_y)-153), by = is)
      Sim_y_1_sim<- deSolve::rk4(X_0, DOY_y_1_sim, df, parms)
      Sim_y_1 <-Sim_y_1_sim[1+(0:(max(DOY_y)-154))/is,]
      
      X_0 = c(Sim_y_1[nrow(Sim_y_1), 1+1:(n_r*4)], rep(0, n_r))
      
      X_0_log = X_0
      X_0_log[1:(n_r*4)] = log(X_0[1:(n_r*4)])
      DOY_y_2_sim = seq((max(DOY_y)-152), max(DOY_y), by = is)
      Sim_y_2_sim<- deSolve::rk4(X_0_log, DOY_y_2_sim, df_log, parms)
      Sim_y_2 <-Sim_y_2_sim[1+(0:152)/is,]
      Sim_y_2[, 1+1:(n_r*4)] = exp(Sim_y_2[, 1+1:(n_r*4)])
      
    } else {
      DOY_y_1 = DOY_y[1:(max(DOY_y)-153)]
      Sim_y_1<- ode(X_0, DOY_y_1, df, parms)
      
      X_0 = c(Sim_y_1[nrow(Sim_y_1), 1+1:(n_r*4)], rep(0, n_r))
      
      DOY_y_2 = DOY_y[(max(DOY_y)-152): max(DOY_y)]
      Sim_y_2<- ode(X_0, DOY_y_2, df, parms)
      
    }
    
    #break at 31/12 to zero everything except diapausing eggs
    Sim[id_d_y,] = rbind(Sim_y_1, Sim_y_2)
    X_0 = c(rep(0, n_r*4), Sim_y_2[nrow(Sim_y_2), 1+(n_r*4+1):(n_r*5)])
  }
  toc()

  #Adults in summer mjjas
  
  Adults <- Sim[, 1+(n_r*3+1):(n_r*4)]
  
  i_mjjaso <- which(as.numeric(substr(W_tot_cycle_df$date, 6, 7)) %in% 5:10)
  
  Ad <- colMeans(Adults[i_mjjaso,])
  
  Ind_df$Ad = Ad 
  
  # Average R0 in in summer mjjas
  m <- Adults/H*100
  b_H2v_DG = 0.31 # beta Mtl 2021
  b_v2H = 0.5 # b Blagrove 2020
  a = (0.0043*temp + 0.0943)/2
  phi_a = 0.9 #human biting preference (urban)
  IIP_DG = 5 #Benkimoun 2021
  EIP_DG = 1.03*(4*exp(5.15 - 0.123*temp)) #Metelmann 2021
  R0_DG = (a*phi_a)^2*m/(mu_A+mu_A^2*EIP_DG)*b_v2H*b_H2v_DG*IIP_DG # as Zanardini et al.
  
  R0 = colMeans(R0_DG[i_mjjaso,]) # ndays R0>1
  nR0 = colSums(R0_DG[i_mjjaso,]>1)/length(years_u)
  
  Ind_df$R0 = R0
  Ind_df$nR0 = nR0
  
  save(Ind_df, file = paste0(folder_plot, "Ind_", city_x,".RData"))
  load(paste0(folder_plot, "Ind_", city_x,".RData"))
  
  years_eval = 2004:2023
  
  point_df <- data.frame("name" = city_x,
                         "year" = years_eval,
                         "T_av_summer" = NA,
                         "P_summ_tot" = NA)
  
  # recc Didier: between 1 may and 1 oct
  for(y in years_eval){
    
    point_df$T_av_summer[which(point_df$year == y)] <- W_tot_df0 %>% 
      filter(year %in% c((y-4):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 304) %>% #1st of november
      dplyr::summarize(T_av_summer = mean(T_av)) %>%
      pull(T_av_summer)
    
    point_df$P_summ_tot[which(point_df$year == y)] <- W_tot_df0 %>% 
      filter(year %in% c((y-4):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 304) %>% #1st of november
      dplyr::summarize(P_summ_tot = sum(P)/length(c((y-4):y))) %>%
      pull(P_summ_tot)
  }

  #E0
  breaks_E0 = c(10^(-7), 10^7)

  g0_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = log10(E0)))+
    scale_fill_viridis_c(limits = log10(c(min(breaks_E0), max(breaks_E0))),
                         na.value = "#32003C")+
    ggtitle(paste0("Average E0, ", city_x))+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = E0),
                 color = "red", breaks = c(1))+
    theme_test()+
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot, color = as.factor(year)), size = 2) +
    guides(size = "legend", colour = "none")+
    scale_color_grey()+
    geom_label_repel(data = point_df %>% filter(year ==  2004 | year ==  2023), aes(x = T_av_summer, y = P_summ_tot, label = year),
                     label.padding = 0.15) #size = 4
  
  
  #Ad
  breaks_Ad = seq(0.03, 30000, by = 500)

  g1_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = log10(Ad)))+
    scale_fill_viridis_c(limits = c(min(log10(breaks_Ad)), max(log10(breaks_Ad))),
                         na.value = "#32003C")+
    ggtitle(paste0("Average log10 adults/ha (May to Oct)"))+
    theme_test()+
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot, color = as.factor(year)), size = 2) +
    guides(size = "legend", colour = "none")+
    scale_color_grey()+
    geom_label_repel(data = point_df %>% filter(year ==  2004 | year ==  2023), aes(x = T_av_summer, y = P_summ_tot, label = year),
                     label.padding = 0.15) #size = 4
  
  # #R0
  # breaks_R = c(0, 0.5, 1, 2, 4, 7, 10)
  # 
  # g2_c <- ggplot()+
  #   geom_contour_fill(data = Ind_df,
  #                     aes(x = T_av_summer, y = P_summ_tot, z = R0))+
  #   geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = R0),
  #                color = "red", breaks = c(1))+
  #   scale_fill_viridis_c(limits = c(min(breaks_R), max(breaks_R)),
  #                        na.value = "#32003C")+
  #   ggtitle("Average R0 (May to Oct)")+
  #   theme_test()+
  #   geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
  #   geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
  #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
  #                    label.padding = 0.15) #size = 4
  
  
  breaks_nR = c(0, 1, 5, 10, 20, 40, 80, 120, 170)

  g3_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = nR0))+
    scale_fill_viridis_c(limits = c(min(breaks_nR), max(breaks_nR )),
                         na.value = "#32003C")+
    ggtitle("n days with R0 >1")+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = nR0),
                 color = "red", breaks = c(1))+
    theme_test()+
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot, color = as.factor(year)), size = 2) +
    guides(size = "legend", colour = "none")+
    scale_color_grey()+
    geom_label_repel(data = point_df %>% filter(year ==  2004 | year ==  2023), aes(x = T_av_summer, y = P_summ_tot, label = year),
                     label.padding = 0.15) #size = 4
  
  
  # Save
  g_tot <- ggarrange(g0_c, g1_c, g3_c, ncol = 1)
  
  ggsave(paste0(folder_plot, "NEW_g", city_x ,".png"), g_tot, units="in", height=8, width= 5.5, dpi=300)
  
}
