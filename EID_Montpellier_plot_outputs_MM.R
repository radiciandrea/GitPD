# plot del cycle
# 2

# per plottare anni consecutivi a Montpellier


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(lubridate)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_elab_consec"

# name = "W_EU"
# year = 2005

files = list.files(folder_out, pattern = "Montpellier_Occitanie")
years = substring(files, 27, 27+3)

n_c = 5 # numero di classi

# this will be concatenated
Sim_tot = matrix(NA, ncol = n_c+1, nrow = 0) 
beta_approx_tot = matrix(NA, ncol = 1, nrow = 0) 

for (i in 1:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  Sim_tot= rbind(Sim_tot, Sim)
  beta_approx_tot=rbind(beta_approx_tot, beta_approx)
}

DOS_sim = 1:nrow(Sim_tot)

#########################
#plot pop
Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = max(DOS_sim)),
                      "t" = rep(DOS_sim, n_c),
                      "value" = c(Sim_tot[, 2:(1+n_c)])) #5 classes

Sim_m_df$date = as.Date(Sim_m_df$t, origin = as.Date(paste0(min(as.numeric(years)-1), "-12-31"))) 

ggplot(Sim_m_df, aes(x = t, y = value, color = variable))+
  geom_line()+
  scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_df$value)))+
  # ylim(1, max(Sim_m_x_df$value))+
  # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
  labs(color = paste0("Abundances per classes (Montpellier)")) +
  theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),

# other Eggs

Sim_x_df<- dcast(Sim_m_df, date ~ variable)

beta_approx_m_df = data.frame("variable" = "beta",
                              "t" = DOS_sim,
                              "value" = c(beta_approx_tot)) #5 classes

beta_approx_m_df$date = Sim_m_df$date = as.Date(beta_approx_m_df$t,
                                                origin = as.Date(paste0(min(as.numeric(years)-1), "-12-31"))) 


Eggs_sim_df <- data.frame(date = Sim_x_df$date,
                          year = year(Sim_x_df$date),
                          DOS = beta_approx_m_df$t,
                          eggs = beta_approx_m_df$value*Sim_x_df$A)

Eggs_sim_15_24_df <- Eggs_sim_df %>%
  filter(year > 2014)%>%
  mutate(norm_eggs = 100*eggs/max(eggs))%>%
  group_by(year) %>%
  mutate(DOY = DOS -min(DOS)+1)%>%
  # mutate(year = as.factor(year))%>%
  ungroup() %>%
  mutate(source = "Sim")%>%
  mutate(quartier = "") %>%
  select(-c("DOS", "year"))


#########################
# load 1: Pérols

folder_obs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"
load(paste0(folder_obs, "Altopictus_Perols_2023_2024.RData"))

Eggs_Perols_df <- Eggs_tot_df %>%
  mutate(quartier = "Perols") %>%
  mutate(source = "Altopictus") 

load(paste0(folder_obs, "EID_Montpellier_2016_2017.RData"))

Eggs_Montpellier_df <- Eggs_tot_df  %>%
  mutate(source = "EID")

Eggs_obs_df <- rbind(Eggs_Perols_df, Eggs_Montpellier_df)  %>%
  select(-c("DOS", "region")) %>%
  filter(!is.na(eggs))
  

Eggs_obs_df$norm_eggs = NA

# cycle to determine the normalization (each quartier its own)

quartiers = unique(Eggs_obs_df$quartier) 

for(Q in quartiers){
  
  Eggs_temp_quartier_df <- Eggs_obs_df %>%
    filter(quartier == Q)
  
  max_eggs_temp = Eggs_temp_quartier_df$eggs %>% max()
  
  dates_sel = Eggs_temp_quartier_df$date
  
  max_norm_eggs_temp = Eggs_sim_15_24_df$norm_eggs[Eggs_sim_15_24_df$date %in% dates_sel] %>% max()
  
  Eggs_obs_df$norm_eggs[Eggs_obs_df$quartier == Q] = Eggs_obs_df$eggs[Eggs_obs_df$quartier == Q]/max_eggs_temp*max_norm_eggs_temp
  
}

#

# join sims
Egg_comp_df <- rbind(Eggs_obs_df, Eggs_sim_15_24_df)


#plot 1
ggplot(Egg_comp_df, aes(x = date, y = norm_eggs))+
  geom_line(data = Egg_comp_df %>% filter(source == "Sim"), color = "gray1")+
  geom_point(data = Egg_comp_df %>% filter(source != "Sim"), aes(shape = source, colour = quartier))+
  theme_test()
