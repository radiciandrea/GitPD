# plot del cycle
# 2

# per plottare anni consecutivi a Nice


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

files = list.files(folder_out, pattern = "Nice_France")
years = substring(files, 17, 17+3)

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
  labs(color = paste0("Abundances per classes (Nice)")) +
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
                          eggs = beta_approx_m_df$value*Sim_x_df$A, #"all eggs, diapaused or not"
                          type = "laid, simulated")

Eggs_sim_08_23_df <- Eggs_sim_df %>%
  filter(year > 2007)%>%
  mutate(norm_eggs = 100*eggs/max(eggs))%>%
  group_by(year) %>%
  mutate(DOY = DOS -min(DOS)+1)%>%
  mutate(year = as.factor(year))%>%
  ungroup() %>%
  select(-c("DOS"))

#########################
# load also 

folder_obs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"
load(paste0(folder_obs, "EID_Nice_2008_2023.RData"))

Eggs_obs_df <- Eggs_tot_df %>%
  mutate("type" = "laid, obs") %>%
  mutate(date = as.Date((date))) %>%
  mutate(year = as.factor(year(date))) %>%
  filter(!is.na(eggs))%>%
  mutate(norm_eggs = 100*eggs /max(eggs))%>%
  mutate(doy = yday(date))%>%
  select("date", "year", "eggs", "type", "norm_eggs", "DOY") 

# join sims
Egg_comp_df <- rbind(Eggs_obs_df, Eggs_sim_08_23_df)


#plot 1
ggplot(Egg_comp_df, aes(x = date, y = norm_eggs, color = type))+
  geom_line(data = Egg_comp_df %>% filter(type != "laid, obs"))+
  geom_point(data = Egg_comp_df %>% filter(type == "laid, obs"))+
  ylab("Nromalized laid aggs")+
  theme_test()+
  theme(legend.position = c(0.13, 0.85))

#lo plotto nell'inkcscape

#plot 2
ggplot(Egg_comp_df,
       aes(x = DOY, y = norm_eggs,
           color = year))+
  ggtitle("Daily laid eggs in Nice")+
  geom_line(data = Egg_comp_df %>% filter(type != "laid, obs"))+
  geom_point(data = Egg_comp_df %>% filter(type == "laid, obs"))+
  # geom_point(data = Egg_comp_df %>% filter(type != "laid, simulated"))+
  # guides(color = FALSE)+
  # ylab("normalized abundance (%)")+
  theme_test()

# date_common
date_common = Eggs_obs_df$date
Eggs_sim_08_23_common_df <- Eggs_sim_08_23_df %>%
  filter(date %in% date_common)

cor(Eggs_sim_08_23_common_df$norm_eggs, Eggs_obs_df$norm_eggs)
