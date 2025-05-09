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
library(data.table)
library(pracma)

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

cities = c("Vienna", "Copenhagen", "Praga", 
           "Ljubjiana", "Zagreb", "Stockhom", "Oslo", "Dublin")

cities = c("Viterbo",
         "Coimbra", "Firenze", "Lugano", "Frankfurt aum Main", "Innsbruck",
         "Antwerp")

#"Sicily (Catania)", not working


rk = "on" #on if integration crashes! (it is way slower)
folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Exposure_state_pow/"

years = 2000:2023


for(city_x in cities){
  
  load(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_EOBS_",
              city_x, "_", min(years), "_", max(years), ".RData"))
  
  LAT = W_tot_df$lat[1]
  LON = W_tot_df$lon[1]
  H_dens = W_tot_df$pop[1]
  
  #recompute years for simulation
  years_u = 2019:2023
  W_tot_df0 <- W_tot_df
  W_tot_df <-  W_tot_df %>% filter(year %in% years_u)
  W_tot_df$DOS <- 1:nrow(W_tot_df)
  
  #photoperiod Ph_P (which variables should I take? sunrise - sunset): to be modified in the future
  SunTimes_df<- getSunlightTimes(data = data.frame("date" = as.Date(W_tot_df$date), "lat"= LAT, "lon" = LON))# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
  Ph_P = as.numeric(SunTimes_df$sunset - SunTimes_df$sunrise)
  t_sr = as.numeric(SunTimes_df$sunrise- as.POSIXct(SunTimes_df$date) +2) # time of sunrise: correction needed since time is in UTC
  
  #T_av = 14.66509
  T_add = seq(-2, 8, by = 0.5)
  #P_pow = seq(0, 2.4, by = 0.2)
  
  #change P_pow: determined Newton Rapson method
  P_summ = W_tot_df %>% 
    filter(DOY >= 121) %>% #1st may
    filter(DOY < 274) %>% #1st of october
    pull(P)
  
  max_P_sd = 12
  
  fun_sd = function(x, p = P_summ, k = max_P_sd){
    px = p^x * sum(p)/sum(p^x)
    return(sd(px) - k)
  }
  
  P_pow_max = newtonRaphson(fun_sd, x0 = 1)$root
  P_pow = seq(0, P_pow_max, length.out = 12)
  
  W_tot_cycle_l <- vector(mode = "list", length = length(T_add)*length(P_pow))
  
  Ind_df = data.frame(T_add = rep(NA, length(T_add)*length(P_pow )),
                      T_av = rep(NA, length(T_add)*length(P_pow )),
                      T_av_summer =rep(NA, length(T_add)*length(P_pow )),
                      P_pow  = rep(NA, length(T_add)*length(P_pow )),
                      sdP = rep(NA, length(T_add)*length(P_pow )),
                      sdP_summer = rep(NA, length(T_add)*length(P_pow )))
  
  for (i in 1:length(T_add)){
    for (j in 1:length(P_pow)){
      
      k = j + (i-1)*length(P_pow)
      
      W_tot_cycle_df <- W_tot_df %>%
        mutate(T_add = T_add[i]) %>%
        mutate(T_av = T_av + T_add[i]) %>%
        mutate(T_m = T_m + T_add[i]) %>%
        mutate(T_M = T_M + T_add[i]) %>%
        mutate(P_summ = P*(DOY >= 121)*(DOY < 274)) %>%
        mutate(P_pow  = P_pow[j]) %>%
        mutate(P_summ = P_summ^P_pow[j]*(sum(P_summ)/sum(P_summ^P_pow[j]))) %>%
        mutate(P = P_summ*(DOY >= 121)*(DOY < 274) + P*(DOY < 121) + P*(DOY >= 274))%>%
        mutate(H = H_dens) %>%
        mutate(region = paste0(region, "_tadd_", T_add[i], "_Ppw_", round(P_pow[j], 2))) %>%
        mutate(Ph_P = Ph_P) %>%
        mutate(t_sr =t_sr)
      
      
      W_tot_cycle_l[[k]] <- W_tot_cycle_df
      
      Ind_df$T_add[k] = T_add[i] 
      Ind_df$T_av[k] = mean(W_tot_cycle_df$T_av)
      Ind_df$T_av_summer[k] = W_tot_cycle_df %>%
        group_by(year) %>%
        filter(DOY >= 121) %>% #1st may
        filter(DOY < 274) %>% #1st of october
        ungroup() %>%
        dplyr::summarize(T_av_m = mean(T_av)) %>%
        pull(T_av_m)
      
      Ind_df$P_pow[k] = P_pow[j]
      Ind_df$sdP[k] = sd(W_tot_cycle_df$P)
      Ind_df$sdP_summer[k] = W_tot_cycle_df %>%
        group_by(year) %>%
        filter(DOY >= 121) %>% #1st may
        filter(DOY < 274) %>% #1st of october
        ungroup() %>%
        dplyr::summarize(P_sd = sd(P)) %>%
        pull(P_sd)
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
  
  h = (1-eps_rat)*(1+eps_0)*exp(-eps_var*(prec-eps_opt)^2)/
    (exp(-eps_var*(prec-eps_opt)^2)+ eps_0) +
    eps_rat*eps_dens/(eps_dens + exp(-eps_fac*H))
  
  source("MM_integration_functions_exp_state.R")
  
  # System initialization
  E0 = rep(0, n_r)
  J0 = rep(0, n_r)
  I0 = rep(0, n_r)
  A0 = rep(0, n_r)
  E_d_0 = 10^3*rep(1, n_r) # IN THIS EXPERIMENT WE FIX Ed0 to 10^4
  X_0 = c(E0, J0, I0, A0, E_d_0)
  
  #integration on multiple years 
  Sim <- matrix(nrow = length(DOS_sim), ncol = 1+n_r*5)
  
  #integration step
  is = 1/60
  
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
  m <- Adults/H*100
  b_H2v_DG = 0.31 # beta Mtl 2021
  b_v2H = 0.5 # b Blagrove 2020
  a = (0.0043*temp + 0.0943)/2
  phi_a = 0.9 #human biting preference (urban)
  IIP_DG = 5 #Benkimoun 2021
  EIP_DG = 1.03*(4*exp(5.15 - 0.123*temp)) #Metelmann 2021
  R0_DG = (a*phi_a)^2*m/(mu_A+mu_A^2*EIP_DG)*b_v2H*b_H2v_DG*IIP_DG # as Zanardini et al.
  
  R0 = colMeans(R0_DG[i_mjjas,]) # ndays R0>1
  nR0 = colSums(R0_DG[i_mjjas,]>1)/length(years_u)
  
  Ind_df$R0 = R0
  Ind_df$nR0 = nR0
  
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
  
  save(Ind_df, file = paste0(folder_plot, "Ind_", city_x,".RData"))
  load(paste0(folder_plot, "Ind_", city_x,".RData"))
  
  years_eval = c(2003, 2008, 2013, 2018, 2023)
  
  point_df <- data.frame("name" = city_x,
                         "year" = years_eval,
                         "T_av_summer" = NA,
                         "sdP_summer" = NA)
  
  # recc Didier: between 1 may and 1 oct
  for(y in years_eval){
    
    # point_df$T_av[which(point_df$year == y)] <- mean(W_tot_df0$T_av[which(W_tot_df0$year %in% c((y-3):y))])
    # point_df$sdP[which(point_df$year == y)] <- sd(W_tot_df0$P[which(W_tot_df0$year%in% c((y-3):y))])
    
    
    point_df$T_av_summer[which(point_df$year == y)] <- W_tot_df0 %>% 
      filter(year %in% c((y-3):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 274) %>% #1st of october
      dplyr::summarize(T_av_m = mean(T_av)) %>%
      pull(T_av_m)
    
    point_df$sdP_summer[which(point_df$year == y)] <- W_tot_df0 %>% 
      filter(year %in% c((y-3):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 274) %>% #1st of october
      dplyr::summarize(P_sd = sd(P)) %>%
      pull(P_sd)
  }
  
  
  # geom_contour_filled
  
  # ggplot()+
  #   geom_contour_fill(data = Ind_df, aes(x = T_add, y = sdP, z = E0))+
  #   scale_fill_viridis()+
  #   ggtitle("E0")+
  #   theme_test() + geom_point(data = point_df, aes(x = T_add, y = sdP)) +
  #   geom_text(data = point_df, aes(x = T_add, y = sdP, label = name), hjust=-0.1, vjust=-0.1)
  
  library(metR)
  library(ggrepel)
  library(ggpubr)
  
  #Ad
  breaks_Ad = seq(4, 40000, by = 500)
  
  g1 <- ggplot() +
    geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = Ad ), breaks = breaks_Ad) +
    ggtitle(paste0("Average log adults/ha (May to Sept) in ", city_x))+
    theme_test()+ 
    geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
    geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
                     label.padding = 0.15) #size = 4
  
  g1_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = sdP_summer, z = log10(Ad)))+
    scale_fill_viridis_c(limits = c(min(log10(breaks_Ad)), max(log10(breaks_Ad))))+
    ggtitle(paste0("Average log10 adults/ha (May to Sept) in ", city_x))+
    theme_test()+
    geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
    geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
                     label.padding = 0.15) #size = 4
  
  #R0
  breaks_R = c(0, 0.5, 1, 2, 4, 7, 10)
  
  g2 <- ggplot()+
    geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = R0), breaks = breaks_R)+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = R0),
                 color = "red", breaks = c(1))+
    ggtitle(paste0("Average R0 (May to Sept)"))+
    theme_test()+ 
    geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
    geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
                     label.padding = 0.15) #size = 4
  
  g2_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = sdP_summer, z = R0))+
    scale_fill_viridis_c(limits = c(min(breaks_R), max(breaks_R)))+
    ggtitle("Average R0 (May to Sept)")+
    theme_test()+
    geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
    geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
                     label.padding = 0.15) #size = 4
  
  
  breaks_nR = c(0.1, 1, 5, 10, 20, 50, 90, 150)
  
  g3 <- ggplot()+
    geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = nR0), breaks = breaks_nR)+
    ggtitle(paste0("n days with R0 >1"))+
    theme_test()+ 
    geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
    geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
                     label.padding = 0.15) #size = 4
  
  g3_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = sdP_summer, z = nR0))+
    scale_fill_viridis_c(limits = c(min(breaks_nR), max(breaks_nR )))+
    ggtitle("n days with R0 >1")+
    theme_test()+
    geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
    geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
                     label.padding = 0.15) #size = 4
  
  
  # Save
  g_tot <- ggarrange(g1_c, g2_c, g3_c, ncol = 1)
  
  ggsave(paste0(folder_plot, "g", city_x ,".png"), g_tot, units="in", height=8, width= 5.5, dpi=300)
  
}
