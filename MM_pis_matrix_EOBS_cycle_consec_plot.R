# plot del cycle
# MM_pis_matrix_EOBS_cycle_consec.R

#per plottare anni consecutivi
# compare with VectAbundance
# and make some stats


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(lubridate)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec"
folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/EOBS_sim_consec_plot"

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


# #########################
#plot pop
Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
                      "region" = rep(rep(regions, each = max(DOS_sim)), n_c),
                      "t" = rep(DOS_sim, n_r*n_c),
                      "value" = c(Sim_tot[, 2:(1+n_c*n_r)])) #5 classes

beta_approx_m_df = data.frame("variable" = "beta",
                      "region" = rep(regions, each = max(DOS_sim)),
                      "t" = rep(DOS_sim, n_r),
                      "value" = c(beta_approx_tot)) #5 classes

# # st_write(domain_sel, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/res_sim_2011_", name, ".shp"))
# #plot
# 
# #210
# 
# id_reg = 1092 #
# 
# #Roma: 1091, 1992
# #Nizza: 1597 e un'altra
# 
# region_x = id_reg #regions[id_reg]
# 
# Sim_m_x_df <- Sim_m_df %>%
#   filter(region == region_x)
# 
# Sim_x_df<- dcast(Sim_m_x_df, t ~ variable)
# 
# ggplot(Sim_m_x_df, aes(x = t, y = value, color = variable))+
#   geom_line()+
#   scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_x_df$value)))+
#   # ylim(1, max(Sim_m_x_df$value))+
#   # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
#   labs(color = paste0("Abundances per classes (", region_x, ")")) +
#   theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),
# 
# #plot eg only adults
# 
# Sim_A_x_df <- Sim_m_x_df %>% 
#   filter(variable == "A") %>%
#   mutate(date = date_sim) 
# 
# ggplot(Sim_A_x_df, aes(x = date, y = value))+
#   geom_line()+
#   # scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_x_df$value)))+
#   # ylim(1, max(Sim_m_x_df$value))+
#   # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
#   labs(color = paste0("Abundances per classes, adult")) +
#   theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),
# 
# #only mean_by_day
# Sim_A_x_average_df <- Sim_m_x_df %>% 
#   filter(variable == "A") %>%
#   mutate(date = date_sim) %>%
#   mutate(date_dj = as.Date(substr(date, 6, 10), format = "%m-%d")) %>%
#   mutate(year = as.numeric(substr(date_sim, 1, 4)))  %>%
#   filter(year > 2009) %>%
#   filter(year < 2020) %>%
#   # group_by(date)%>%
#   # mutate(DOY = julian(date, origin = as.Date(paste0(as.numeric(year)-1, "-12-31"))))%>%
#   # ungroup()%>%
#   group_by(date_dj)%>%
#   summarize(adults_2010s = mean(value))%>%
#   ungroup()
# 
# ggplot(Sim_A_x_average_df, aes(x = date_dj, y = adults_2010s))+
#   geom_line()+
#   # scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_x_df$value)))+
#   # ylim(1, max(Sim_m_x_df$value))+
#   # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
#   labs(color = paste0("Abundances per classes, adult")) +
#   theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),
# 

# compute laid eggs: change into integration function #beta should be calculatedd hour by hour

###### plot specific cell in vectAbundance

folder_obs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"

load(paste0(folder_obs, "VectAbundance_025.RData"))
#Eggs_tot_df
# #Eg Nice = 1597 (actually: 1537)
# 
# id_reg = 1597
# region_v = regions[id_reg]
# 
# # other sources
# # load(paste0(folder_obs, "Eggs_Weather_Nice_200811.RData"))
# # region_v = "NICE"
# 
# Eggs_obs_df <- Eggs_tot_df %>%
#   filter(region == region_v) %>%
#   mutate("type" = "laid, obs") %>%
#   mutate(date = as.Date((date))) %>%
#   select("eggs", "type", "date") 
# 
# date_sel = Eggs_obs_df$date
# 
# #Sim starts in 2005
# date = as.Date(DOS_sim, origin = first_day-1)
# 
# Sim_m_x_df <- Sim_m_df %>%
#   filter(region == region_x) %>%
#   mutate(date = rep(date, n_c)) %>%
#   filter(date %in% date_sel)
# 
# Sim_x_df<- dcast(Sim_m_x_df, date ~ variable)
# 
# #accidenti, dovevo salvare anche beta!
# 
# beta_approx_x_df = beta_approx_m_df %>%
#   filter(region == region_x) %>%
#   mutate(date = date) %>%
#   filter(date %in% date_sel)
# 
# Eggs_sim_df <- data.frame(date = Sim_x_df$date,
#                                          eggs = beta_approx_x_df$value*Sim_x_df$A, #"all eggs, diapaused or not"
#                                          type = "laid, simulated")
# 
# # join sims
# Egg_comp_df <- rbind(Eggs_obs_df, Eggs_sim_df) %>%
#   group_by(type)%>%
#   # mutate(norm_eggs = 100*eggs/mean(eggs, na.rm = T))%>%
#   mutate(norm_eggs = 100*eggs/max(eggs, na.rm = T))%>%
#   ungroup()
# 
# ggplot(Egg_comp_df, aes(x = date, y = norm_eggs, color = type))+
#   geom_line(data = Egg_comp_df %>% filter(type != "observed"))+
#   geom_point(data = Egg_comp_df %>% filter(type == "observed"))+
#   theme_test()


########## plot cycle

###### plot specific cell in VectAbundance

folder_obs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"

# load VectAbundance:Eggs_tot_df
load(paste0(folder_obs, "VectAbundance_025.RData"))

# load vectAbundance names
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_W_EU_IDVectAb.shp")

VectDomain <- domain %>%
  filter(!is.na(IDVectAb))

Eggs_tot_df <- left_join(Eggs_tot_df, VectDomain) %>%
  st_drop_geometry()

regions_availab = sort(unique(Eggs_tot_df$region))

VectDomain$r_gross = NA
VectDomain$pv_r_gross = NA
VectDomain$rmse_gross = NA
VectDomain$r_year = NA
VectDomain$pv_r_year = NA
VectDomain$l_y = NA

#Sim starts in 2005
date = as.Date(DOS_sim, origin = first_day-1)

for(region_x in regions_availab){
  
  # region_x = regions[id_reg]
  id_VA = VectDomain$IDVectAb[VectDomain$region == region_x]
  name_region = VectDomain$Name_app[VectDomain$region == region_x]
  
  #max(c(1,eggs) little correction otherwise can't deal with only zeros 
  
  #apparenlty in Ferrara (id VA 7258) there are same non-univocal values. which should I keep?
  
  Eggs_obs_df <- Eggs_tot_df %>%
    filter(region == region_x) %>%
    group_by(date)%>%
    mutate(eggs = mean(eggs)) %>%
    ungroup() %>%
    dplyr::distinct(date, .keep_all = TRUE) %>%
    mutate("type" = "laid, obs") %>%
    mutate(date = as.Date((date))) %>%
    select("eggs", "type", "date") %>%
    mutate(norm_eggs = 100*eggs/max(c(1,eggs), na.rm = T)) 
    
  
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
  
  # moving average eggs: 7 previous days ?
  Eggs_sim_df$eggs = sapply(1:nrow(Eggs_sim_df), function(x){mean(Eggs_sim_df$eggs[max(1,x-6):x])})
  
  
  #Calcolo il max solo relativo al date_sel per il plot
  
  Eggs_sim_max_date_sel <- max(Eggs_sim_df$eggs[Eggs_sim_df$date %in% date_sel], na.rm = T)
  
  Eggs_sim_df <- Eggs_sim_df %>%
    mutate(norm_eggs = 100*eggs/Eggs_sim_max_date_sel)
  
  # join sims
  Egg_comp_df <- rbind(Eggs_obs_df, Eggs_sim_df) 
  
  #####
  # stats (copied from EID_Nice_plot_outputs_MM)
  
  # date_common
  date_common = Eggs_obs_df$date
  Eggs_sim_common_df <- Eggs_sim_df %>%
    filter(date %in% date_common)
  
  #cor
  cor_brut = cor(Eggs_sim_common_df$norm_eggs, Eggs_obs_df$norm_eggs, use = "complete.obs")
  
  #cor test (https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/)
  # Pearson correlation test
  cor_brut_p = cor.test(Eggs_sim_common_df$norm_eggs, Eggs_obs_df$norm_eggs)$p.value
  
  # Number of stars: https://faq.edqm.eu/pages/viewpage.action?pageId=1377305
  #If a p-value is less than 0.05, it is flagged with one star (*). If a p-value is less than 0.01, it is flagged with 2 stars (**). If a p-value is less than 0.001, it is flagged with three stars (***).
  cor_brut_stars = case_when(cor_brut_p < 0.001 ~ "***",
                             cor_brut_p < 0.01 ~ "**",
                             cor_brut_p < 0.05 ~ "*",
                             .default = "")
  #rmse
  rmse_brut = sqrt(mean((Eggs_sim_common_df$norm_eggs/100 - Eggs_obs_df$norm_eggs/100)^2, na.rm = T))
  
  #### corr_per_year
  # at least 9 in summer
  
  Eggs_obs_filtered_df <- Eggs_obs_df %>%
    mutate(month = month(date)) %>%
    filter(month %in% c(6,7,8,9)) %>% #only summer days
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    filter(count > 8)
  
  date_filter = Eggs_obs_filtered_df$date
  
  #computer yearly average
  
  Eggs_obs_year_filtered_df <- Eggs_obs_filtered_df %>%
    group_by(year, type) %>%
    summarise(norm_eggs = mean(norm_eggs))%>%
    ungroup() 
  
  # date_filter
  
  Eggs_sim_year_filtered_df <- Eggs_sim_df %>%
    mutate(year = year(date)) %>%
    filter(date %in% date_filter) %>%
    group_by(year, type) %>%
    summarise(norm_eggs = mean(norm_eggs))%>%
    ungroup()
  
  #consider only data for which at least 3 summers are available
  if(nrow(Eggs_obs_year_filtered_df)>=3){
    #cor_annual
    cor_annual = cor(Eggs_obs_year_filtered_df$norm_eggs, Eggs_sim_year_filtered_df$norm_eggs)
    
    #cor test (https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/)
    cor_annual_p = cor.test(Eggs_obs_year_filtered_df$norm_eggs, Eggs_sim_year_filtered_df$norm_eggs)$p.value
    
    # Number of stars: https://faq.edqm.eu/pages/viewpage.action?pageId=1377305
    #If a p-value is less than 0.05, it is flagged with one star (*). If a p-value is less than 0.01, it is flagged with 2 stars (**). If a p-value is less than 0.001, it is flagged with three stars (***).
    cor_annual_stars = case_when(cor_annual_p < 0.001 ~ "***",
                                 cor_annual_p < 0.01 ~ "**",
                                 cor_annual_p < 0.05 ~ "*",
                                 .default = "") 
  } else {
    #cor_annual
    cor_annual = NA
    
    #cor test (https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/)
    cor_annual_p = NA
    
    # Number of stars: https://faq.edqm.eu/pages/viewpage.action?pageId=1377305
    #If a p-value is less than 0.05, it is flagged with one star (*). If a p-value is less than 0.01, it is flagged with 2 stars (**). If a p-value is less than 0.001, it is flagged with three stars (***).
    cor_annual_stars = ""
  }
  
  # label_cor = paste0("r: ", round(cor_brut, 2), cor_brut_stars,
  #                    "; rmse = ", round(rmse_brut, 3), "; r (annual): ",
  #                    round(cor_annual, 2), cor_annual_stars)
  
  label_cor = paste0("r: ", round(cor_brut, 2), cor_brut_stars)
  
  VectDomain$r_gross[VectDomain$region == region_x] = cor_brut
  VectDomain$pv_r_gross[VectDomain$region == region_x] = cor_brut_p
  VectDomain$rmse_gross[VectDomain$region == region_x] = rmse_brut
  VectDomain$r_year[VectDomain$region == region_x] = cor_annual
  VectDomain$pv_r_year[VectDomain$region == region_x] = cor_annual_p
  VectDomain$l_y[VectDomain$region == region_x] = length(unique(year(date_common)))
  
  #####
  
  egg_plot <- ggplot(Egg_comp_df, aes(x = date, y = norm_eggs, color = type))+
    ggtitle(paste0(name_region," (", id_VA, ")"))+
    geom_line(data = Egg_comp_df %>% filter(type == "laid, simulated"))+
    geom_point(data = Egg_comp_df %>% filter(type != "laid, simulated"))+
    guides(color = FALSE)+
    ylab("normalized abundance (%)")+
    theme_test()#+
    # annotate(geom="text", x= min(Egg_comp_df$date)+(max(Egg_comp_df$date) - min(Egg_comp_df$date))*0.13, y=max(Egg_comp_df$norm_eggs),
    #          label= label_cor, color="black")
  
  #more details
  egg_plot <- ggplot(Egg_comp_df, aes(x = date, y = norm_eggs, color = type))+
    ggtitle(paste0(name_region," (Id VectAdunance: ", id_VA, "), ", label_cor))+
    geom_line(data = Egg_comp_df %>% filter(type == "laid, simulated"))+
    geom_point(data = Egg_comp_df %>% filter(type != "laid, simulated"))+
    guides(color = FALSE)+
    ylab("normalized abundance (%)")+
    theme_test()

  ggsave(paste0(folder_plot, "/Paper_egg_plot_cell_id_", region_x, ".png"), plot = egg_plot,
         units="in", width=5.5, height=2.5, dpi=300)


}

save(VectDomain, file = paste0(folder_plot, "/VectDomainStat.RData"))

# st_write(st_centroid(VectDomain), "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_W_EU_IDVectAb_elab.shp")

### Elab_a posteriori
countries_sh <-  st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/european-countries.shp")

load(paste0(folder_plot, "/VectDomainStat.RData"))

# Replace Nice with EID data
load(paste0(folder_plot, "/VectDomainStat_Nice.RData"))

VectDomain <- VectDomain %>%
  filter(Name_app != "Nice")

VectDomain_Nice <- domain%>%
  filter(region == 1597) %>%
  mutate(Name_app = "Nice (EID)") %>%
  mutate(r_gross = VectDomain_Nice$r_gross) %>%
  mutate(pv_r_gross = VectDomain_Nice$pv_r_gross) %>%
  mutate(rmse_gross = VectDomain_Nice$rmse_gross) %>%
  mutate(r_year  = VectDomain_Nice$r_year) %>%
  mutate(pv_r_year = VectDomain_Nice$pv_r_year) %>%
  mutate(l_y = VectDomain_Nice$l_y) %>%
  select(c("region", "IDVectAb", "Name_app", "geometry", "r_gross", "pv_r_gross",
           "rmse_gross", "r_year", "pv_r_year", "l_y"))

# VectDomain$r_gross[VectDomain$Name_app == "Nice (EID)"] = VectDomain_Nice$r_gross
# VectDomain$pv_r_gross[VectDomain$Name_app == "Nice (EID)"] = VectDomain_Nice$pv_r_gross
# VectDomain$rmse_gross[VectDomain$Name_app == "Nice (EID)"] = VectDomain_Nice$rmse_gross
# VectDomain$r_year[VectDomain$Name_app == "Nice (EID)"] = VectDomain_Nice$r_year
# VectDomain$pv_r_year[VectDomain$Name_app == "Nice (EID)"] = VectDomain_Nice$pv_r_year
# VectDomain$Name_app[VectDomain$Name_app == "Nice (EID)"] = VectDomain_Nice$Name_app
# VectDomain$l_y[VectDomain$Name_app == "Nice (EID)"] = VectDomain_Nice$l_y

#load Montpellier
load(paste0(folder_plot, "/VectDomainStat_Perols.RData"))

VectDomain_Montpellier <- domain%>%
  filter(region == 1524) %>%
  mutate(Name_app = "Montpellier (Altopictus)") %>%
  mutate(r_gross = VectDomain_Perols$r_gross) %>%
  mutate(pv_r_gross = VectDomain_Perols$pv_r_gross) %>%
  mutate(rmse_gross = VectDomain_Perols$rmse_gross) %>%
  mutate(r_year  = VectDomain_Perols$r_year) %>%
  mutate(pv_r_year = VectDomain_Perols$pv_r_year) %>%
  mutate(l_y = VectDomain_Perols$l_y) %>%
  select(c("region", "IDVectAb", "Name_app", "geometry", "r_gross", "pv_r_gross",
         "rmse_gross", "r_year", "pv_r_year", "l_y"))
                              
VectDomain <- rbind(VectDomain, VectDomain_Nice, VectDomain_Montpellier)

###

summary(VectDomain$r_gross)
summary(VectDomain$pv_r_gross)

folder_plot2 = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/ArtiConForm/05_AeAlbopictus_ImpactrecentClimateChange/Images/"

#plot map 
ggplot()+
  geom_sf(data = countries_sh, alpha = 1, colour = "white")+
  geom_sf(data = st_centroid(VectDomain), aes(color = r_gross, size = l_y))+
  # geom_sf(data = st_centroid(VectDomain) %>% filter(pv_r_gross< 0.05), shape = 3, color = "black")+
  geom_sf(data = st_centroid(VectDomain) %>% filter(pv_r_gross< 0.01), shape = 1, color = "black")+
  coord_sf(xlim = c(4.25, 18), ylim = c(37, 46.5))+
  # scale_color_gradient2(
  #   name = waiver(),
  #   low = "#384AB4",
  #   mid = "white",
  #   high = "#B00026",
  #   midpoint = 0)+
  scale_color_viridis_c(limits = c(0,1),
                        na.value = "grey50")+
  ggtitle('Linear correlation')+
  theme_void()

ggsave(file= paste0(folder_plot2, "VectAbundance_corr_withmontpel.png"), units="in", width=6, height=7, dpi=300)

ggplot()+
  geom_sf(data = countries_sh, alpha = 1, colour = "white")+
  geom_sf(data = VectDomain, fill = "black")+
  geom_sf(data = VectDomain %>% filter(r_gross>0), aes(fill = pv_r_gross))+
  coord_sf(xlim = c(4, 17), ylim = c(37, 47))+
  scale_fill_gradient2(
    name = waiver(),
    low = "#384AB4",
    mid = "grey90",
    high = "#B00026",
    midpoint = 0.2)+
  ggtitle('p-value linear corr')+
  theme_minimal()

sum(VectDomain %>%  pull(r_gross) >0.5, na.rm = T)
sum(VectDomain %>% filter(r_gross>0) %>% pull(pv_r_gross) <0.1, na.rm = T)  

ggplot()+
  geom_sf(data = countries_sh, alpha = 1, colour = "white")+
  geom_sf(data = VectDomain, aes(fill = r_year))+
  coord_sf(xlim = c(4, 17), ylim = c(37, 47))+
  scale_fill_gradient2(
    name = waiver(),
    low = "red",
    mid = "grey90",
    high = "blue",
    midpoint = 0)+
  ggtitle('Linear correlation (yearly average)')+
  theme_minimal()

ggplot()+
  geom_sf(data = countries_sh, alpha = 1, colour = "white")+
  geom_sf(data = VectDomain, fill = "black")+
  geom_sf(data = VectDomain %>% filter(r_year > 0), aes(fill = pv_r_year))+
  coord_sf(xlim = c(4, 17), ylim = c(37, 47))+
  scale_fill_gradient2(
    name = waiver(),
    low = "blue",
    mid = "grey90",
    high = "red",
    midpoint = 0.2)+
  ggtitle('p-value linear corr (yearly average)')+
  theme_minimal()

sum(VectDomain %>% filter(r_year>0) %>% pull(pv_r_year) <0.1, na.rm = T)
