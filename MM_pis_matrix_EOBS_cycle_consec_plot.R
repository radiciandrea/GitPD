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
folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec_plot"

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

#same for beta
beta_approx_tot = matrix(NA, ncol = n_r, nrow = n_d) 
beta_approx_tot[1:n_d_i,]=beta_approx

k = n_d_i
for (i in 2:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  n_d_i = nrow(Sim)
  Sim_tot[k + 1:n_d_i,]=Sim
  beta_approx_tot[k + 1:n_d_i,]=beta_approx
  k = k + n_d_i
}


#########################
#plot pop
Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
                      "region" = rep(rep(regions, each = max(DOS_sim)), n_c),
                      "t" = rep(DOS_sim, n_r*n_c),
                      "value" = c(Sim_tot[, 2:(1+n_c*n_r)])) #5 classes

beta_approx_m_df = data.frame("variable" = "beta",
                      "region" = rep(regions, each = max(DOS_sim)),
                      "t" = rep(DOS_sim, n_r),
                      "value" = c(beta_approx_tot)) #5 classes

# st_write(domain_sel, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/res_sim_2011_", name, ".shp"))
#plot

#210

id_reg = 1092 #

#Roma: 1091, 1992
#Nizza: 1597 e un'altra

region_x = id_reg #regions[id_reg]

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

#plot eg only adults

Sim_A_x_df <- Sim_m_x_df %>% 
  filter(variable == "A") %>%
  mutate(date = date_sim) 

ggplot(Sim_A_x_df, aes(x = date, y = value))+
  geom_line()+
  # scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_x_df$value)))+
  # ylim(1, max(Sim_m_x_df$value))+
  # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
  labs(color = paste0("Abundances per classes, adult")) +
  theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),

#only mean_by_day
Sim_A_x_average_df <- Sim_m_x_df %>% 
  filter(variable == "A") %>%
  mutate(date = date_sim) %>%
  mutate(date_dj = as.Date(substr(date, 6, 10), format = "%m-%d")) %>%
  mutate(year = as.numeric(substr(date_sim, 1, 4)))  %>%
  filter(year > 2009) %>%
  filter(year < 2020) %>%
  # group_by(date)%>%
  # mutate(DOY = julian(date, origin = as.Date(paste0(as.numeric(year)-1, "-12-31"))))%>%
  # ungroup()%>%
  group_by(date_dj)%>%
  summarize(adults_2010s = mean(value))%>%
  ungroup()

ggplot(Sim_A_x_average_df, aes(x = date_dj, y = adults_2010s))+
  geom_line()+
  # scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_x_df$value)))+
  # ylim(1, max(Sim_m_x_df$value))+
  # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
  labs(color = paste0("Abundances per classes, adult")) +
  theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),


# compute laid eggs: change into integration function #beta should be calculatedd hour by hour

###### plot specific cell in vectAbundance

folder_obs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"

load(paste0(folder_obs, "VectAbundance_025.RData"))
#Eggs_tot_df
#Eg Nice = 1597

id_reg = 1597
region_v = regions[id_reg]

# other sources
# load(paste0(folder_obs, "Eggs_Weather_Nice_200811.RData"))
# region_v = "NICE"

Eggs_obs_df <- Eggs_tot_df %>%
  filter(region == region_v) %>%
  mutate("type" = "laid, obs") %>%
  mutate(date = as.Date((date))) %>%
  select("eggs", "type", "date") 

date_sel = Eggs_obs_df$date

#Sim starts in 2005
date = as.Date(DOS_sim, origin = first_day-1)

Sim_m_x_df <- Sim_m_df %>%
  filter(region == region_x) %>%
  mutate(date = rep(date, n_c)) %>%
  filter(date %in% date_sel)

Sim_x_df<- dcast(Sim_m_x_df, date ~ variable)

#accidenti, dovevo salvare anche beta!

beta_approx_x_df = beta_approx_m_df %>%
  filter(region == region_x) %>%
  mutate(date = date) %>%
  filter(date %in% date_sel)

Eggs_sim_df <- data.frame(date = Sim_x_df$date,
                                         eggs = beta_approx_x_df$value*Sim_x_df$A, #"all eggs, diapaused or not"
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


########## plot cycle

###### plot specific cell in vectAbundance

folder_obs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"

load(paste0(folder_obs, "VectAbundance_025.RData"))

regions_availab = sort(unique(Eggs_tot_df$region))

#Sim starts in 2005
date = as.Date(DOS_sim, origin = first_day-1)

for(id_reg in regions_availab){
  
  region_x = regions[id_reg]
  
  Eggs_obs_df <- Eggs_tot_df %>%
    filter(region == region_x) %>%
    mutate("type" = "laid, obs") %>%
    mutate(date = as.Date((date))) %>%
    select("eggs", "type", "date") %>%
    mutate(relative_eggs_M = 100*eggs/max(eggs, na.rm = T))
  
  date_sel = Eggs_obs_df$date
  date_min = min(date_sel)
  date_max = max(date_sel)
  
  Sim_m_x_df <- Sim_m_df %>%
    filter(region == region_x) %>%
    mutate(date = rep(date, n_c)) %>%
    filter(date >= date_min) %>%
    filter(date <= date_max)
  
  Sim_x_df<- dcast(Sim_m_x_df, date ~ variable)
  
  #accidenti, dovevo salvare anche beta!
  
  beta_approx_x_df = beta_approx_m_df %>%
    filter(region == region_x) %>%
    mutate(date = date) %>%
    filter(date >= date_min) %>%
    filter(date <= date_max)
  
  Eggs_sim_df <- data.frame(date = Sim_x_df$date,
                            eggs = beta_approx_x_df$value*Sim_x_df$A, #"all eggs, diapaused or not"
                            type = "laid, simulated")
  
  
  #alcolo il max solo relativo al date_sel per il plot
  
  Eggs_sim_max_date_sel <- max(Eggs_sim_df$eggs[Eggs_sim_df$date %in% date_sel], na.rm = T)
  
  Eggs_sim_df <- Eggs_sim_df %>%
    mutate(relative_eggs_M = 100*eggs/Eggs_sim_max_date_sel)
  
  # join sims
  Egg_comp_df <- rbind(Eggs_obs_df, Eggs_sim_df) 
  
  egg_plot <- ggplot(Egg_comp_df, aes(x = date, y = relative_eggs_M, color = type))+
    ggtitle(paste0("Eggs abundance, VectAbundance (points) vs simulated, cell id ", region_x))+
    geom_line(data = Egg_comp_df %>% filter(type == "laid, simulated"))+
    geom_point(data = Egg_comp_df %>% filter(type != "laid, simulated"))+
    guides(color = FALSE)+
    ylab("normalized abundance (%)")+
    theme_test()
  
  ggsave(paste0(folder_plot, "/egg_plot_cell_id_", region_x, ".png"), plot = egg_plot, dpi = 300)
  
}

