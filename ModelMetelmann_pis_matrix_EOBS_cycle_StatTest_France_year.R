# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R

# French scale

# to validate with data of presence and absence

# presence: data from Arnaud Cannet (DGS - Centre de crises sanitaires (CCS))


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(tidyverse)

type = "_01"

folder_out = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim", type)

# name = "W_EU"
# year = 2005

files = list.files(folder_out)

name = substring(files[1], nchar(type)+10, nchar(type)+13)
years = substring(files, nchar(type)+15, nchar(type)+18) #CORREGGGERE QUI

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

E0_m = matrix(NA, ncol = length(E0_v), nrow = length(files))
rm(Sim)

for (i in 1:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  E0_m[i,]= E0_v
  rm(Sim)
}

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) %>%
  arrange(region) %>%
  filter(!(is.na(Country))) %>%
  select(region)

domain_df <- domain_sel %>%
  st_drop_geometry()
           
  # select France only

# load presence data

folder_CANNET = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DGS_Cannet"
data_presence <- read.csv2(paste0(folder_CANNET, "/IRD communes années mod.csv"))

# simplidfied shp (may be too much) with QGIS
folder_COMM = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/communes-20220101-shp"
shp_communes <- st_read(paste0(folder_COMM, "/communes-20220101_metr_simp.shp"))

data_presence_sf <- left_join(shp_communes, data_presence, by = c("insee" = "Code.Insee")) %>%
  rename(presence = Nb.Présence.vecteur.Oui) %>%
  rename(year_col = Année.colonisation.commune) %>%
  select(c("insee","presence", "year_col", "geometry"))

ggplot()+
  geom_sf(data = data_presence_sf, aes(fill = year_col), color = NA)

# CHECK THE FOLLOWING

sf::sf_use_s2(FALSE)
data_presence_sf <- sf::st_make_valid(data_presence_sf)

tic()
domain_presence_intersect <- st_intersection(data_presence_sf, domain_sel)
toc()

# see if with E0 > 1 ther is a correspondence (ex with K Cohens) among observations and prediction
# if K Cohen is increasing: invasion is in course and limits are being reached?
# https://www.personality-project.org/r/html/kappa.html
#install.packages("psych")
library(psych)

years_eval = 2006:2023

delays = 6 #1:8

# remove sf just to accelerate the following
domain_presence_intersect_df <- domain_presence_intersect %>%
  st_drop_geometry()

rm(domain_presence_intersect)

CK = data.frame(year = rep(years_eval, each =length(delays)),
                delay = rep(delays, length(years_eval)),
                K = NA)

domain_df_year <- domain_df %>%
  mutate(s_2006 = NA) %>%
  mutate(s_2007 = NA) %>%
  mutate(s_2008 = NA) %>%
  mutate(s_2009 = NA) %>%
  mutate(s_2010 = NA) %>%
  mutate(s_2011 = NA) %>%
  mutate(s_2012 = NA) %>%
  mutate(s_2013 = NA) %>%
  mutate(s_2014 = NA) %>%
  mutate(s_2015 = NA) %>%
  mutate(s_2016 = NA) %>%
  mutate(s_2017 = NA) %>%
  mutate(s_2018 = NA) %>%
  mutate(s_2019 = NA) %>%
  mutate(s_2020 = NA) %>%
  mutate(s_2021 = NA) %>%
  mutate(s_2022 = NA) %>%
  mutate(s_2023 = NA) 

# for(delay in delays){
delay = delays
for (year in years_eval){
  years_sel = (year-delay):(year) 
  E0_m_c_sel <- apply(E0_m[which(years %in% years_sel),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
  
  E0_sel = apply(E0_m_c_sel, 2,
                 function(x){exp(mean(log(x)))})
  
  domain_years_sel <- domain_df%>%
    mutate(E0 = E0_sel[domain_sel$region])
  
  domain_presence_intersect_years <- domain_presence_intersect_df %>%
          mutate(presence = case_when((presence == 1) & (year_col <= year) ~ 1,
                                .default = 0))
    
  domain_presence_intersect_years_sel <- left_join(domain_presence_intersect_years, domain_years_sel)
  
  domain_prs_abs <-   domain_presence_intersect_years_sel %>%
    select(c("region", "presence", "E0"))%>%
    group_by(region)%>%
    # dplyr::summarise(presence = 1*(sum(presence)>0)) %>%
    dplyr::summarise(pres_obs = 1*(sum(presence)>0), pres_pred = 1*(exp(mean(log(E0)))>1)) %>%
    ungroup() %>%
    mutate(score = case_when((pres_obs == 1)&(pres_pred == 1) ~ "true positive",
                             (pres_obs == 0)&(pres_pred == 0) ~ "true negative",
                             (pres_obs == 0)&(pres_pred == 1) ~ "false positive",
                             (pres_obs == 1)&(pres_pred == 0) ~ "false negative"))
  
  domain_df_year[,which(substr(names(domain_df_year), 3, 6)==year)] <- domain_prs_abs$score
  
  CK$K[which((CK$year == year) & (CK$delay == delay))] = 
    cohen.kappa(x = cbind(domain_prs_abs$pres_obs, domain_prs_abs$pres_pred))$kappa
  
}
# }

# ggplot()+
#   geom_tile(data = CK, aes(x = year, y = delay, fill = K))
# 
# CK %>% group_by(delay) %>% summarise(K = mean(K)) %>% ungroup()

 # Linear regression: K vs year

folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/ArtiConForm/02_ClimaImpact_nov_2024/Images/"

ggplot(CK, aes(x = year, y = K)) +
  geom_point() +
  geom_smooth(method='lm')

s <-summary(lm(K ~ year, CK))

ggplot(CK, aes(x = year, y = K)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill='transparent', color=NA), 
        panel.background = element_rect(fill='transparent'), axis.line = element_line(colour = "black")) +theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(fill='transparent'), #transparent legend bg
  legend.box.background = element_rect(fill='transparent') #transparent legend panel
)

ggsave(file= paste0(folder_plot, "CK.png"), units="in", width=4, height=3, dpi=300)

# plot year by year 
# https://bookdown.org/ededeban/ConsBioMap/GIF.html
# https://www.r-bloggers.com/2021/05/animated-graph-gif-with-gganimate-ggplot/

  
library(gganimate)
library(magick)

domain_df_m <- reshape2::melt(domain_df_year, id.vars = c("region")) %>%
  rename(year = variable) %>%
  mutate(year = as.numeric(substr(year, 3, 6))) %>%
  rename(score = value)

domain_df_m <- left_join(domain_df_m, domain_sel)

domain_sf_m <- st_as_sf(domain_df_m) 

for(y in years_eval){
  
  K = CK %>% filter(year == y) %>% pull(K)
  
  p1 <- ggplot()+
    geom_sf(data = domain_sf_m %>% filter(year == y) , aes(fill = score), color = NA)+ 
    scale_fill_manual(breaks = c("true positive", "true negative", "false positive", "false negative"),
                      values=c("#B0986CFF", "#009474FF", "#EFDDCFFF","#72E1E1FF"))+
    ggtitle(paste0("Year: ", y," - Accuracy (K): ", round(K, 2)))+
    theme_void()
  
  ggsave(file= paste0(folder_plot, "Plot_invasion_", y,".png"), plot= p1, units="in", width=7, height=5.5, dpi=300)
}


# graph1.animation = plot1 +
#   transition_time(year) +
#   labs(subtitle = "Year: {frame_time}") +
#   shadow_wake(wake_length = 0.1)
# 
# x <- animate(graph1.animation, height = 700, width = 400, fps = 5, duration = 10,
#              end_pause = 60, res = 100)
# 
# anim_save("gapminder graph.gif", animation = x)
# 
# 
# ggplot()+
#   geom_sf(data = domain_sf_m  , aes(fill = score), color = NA)+ 
#   scale_fill_manual(breaks = c("true positive", "true negative", "false positive", "false negative"),
#                     values=c("#B0986CFF", "#009474FF", "#EFDDCFFF","#72E1E1FF")) +
#   transition_time(domain_sf_myear)

imgs <- list.files(folder_plot, full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at X frames per second
img_animated <- image_animate(img_joined, fps = 1)

# ## view animated image
# img_animated

## save to disk
image_write(image = img_animated,
            path = paste0(folder_plot, "/Animation.gif"))

            