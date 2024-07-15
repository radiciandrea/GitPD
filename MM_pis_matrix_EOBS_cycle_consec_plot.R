# plot del cycle
# MM_pis_matrix_EOBS_cycle_consec_plot.R

#per plottare anni consecutivi


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec"
folder_obs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"

# name = "W_EU"
# year = 2005

files = list.files(folder_out)

name = substring(files[1], 10, 13)
years = substring(files, 15, 18)

first_day = as.Date(paste0(min(years), "-01-01"))
last_day = as.Date(paste0(max(years), "-12-31"))

date_sim = seq(from = first_day, to = last_day, by = 'day')
DOS_sim = 1:length(date_sim)
n_d = length(DOS_sim)

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

n_c = 5 # numero di classi
n_r = (ncol(Sim)-1)/n_c #numero di regioni
regions = 1:n_r

n_d_i = nrow(Sim)
Sim_tot = matrix(NA, ncol = n_c*n_r+1, nrow = n_d) 
Sim_tot[1:n_d_i,]=Sim

k = n_d_i
for (i in 2:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  n_d_i = nrow(Sim)
  Sim_tot[k + 1:n_d_i,]=Sim
  k = k + n_d_i
}


#########################
#plot pop
Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
                      "region" = rep(rep(regions, each = max(DOS_sim)), n_c),
                      "t" = rep(DOS_sim, n_r*n_c),
                      "value" = c(Sim_tot[, 2:(1+n_c*n_r)])) #5 classes

# st_write(domain_sel, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/res_sim_2011_", name, ".shp"))
#plot

#210

id_reg = 210 #Montpellier = 93 in Occitanie # in Francia 340 cella maledetta, 568, 569, 608, 650 # 126 (maghreb), 210 max

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

###### plot specific cell in vectAbundance

load(paste0(folder_obs, "VectAbundance_025.RData"))

#Eg Nice = 1537

id_reg = 1537

region_x = regions[id_reg]

Eggs_obs_df <- Eggs_tot_df %>%
  filter(region == region_x) %>%
  mutate("type" = "laid, obs") %>%
  select("eggs", "type", "date") 
  

date_sel = Eggs_obs_df$date

Eggs_obs_df <- Eggs_obs_df

#Sim starts in 2005
date = as.Date(DOS_sim, origin = first_day-1)

Sim_m_x_df <- Sim_m_df %>%
  filter(region == region_x) %>%
  mutate(date = rep(date, n_c)) %>%
  filter(date %in% date_sel)

Sim_x_df<- dcast(Sim_m_x_df, date ~ variable)

#accidenti, dovevo salvare anche beta!

beta_approx = 12

Eggs_sim_df <- data.frame(date = Sim_x_df$date,
                                         eggs = beta_approx*Sim_x_df$A, #"all eggs, diapaused or not"
                                         type = "laid, simulated")

# join sims
Egg_comp_df <- rbind(Eggs_obs_df, Eggs_sim_df) %>%
  group_by(type)%>%
  mutate(relative_eggs_m = 100*eggs/mean(eggs, na.rm = T))%>%
  mutate(relative_eggs_M = 100*eggs/max(eggs, na.rm = T))%>%
  ungroup()

ggplot(Egg_comp_df, aes(x = date, y = relative_eggs_M, color = type))+
  geom_line(data = Egg_comp_df %>% filter(type != "observed"))+
  geom_point(data = Egg_comp_df %>% filter(type == "observed"))+
  theme_test()
