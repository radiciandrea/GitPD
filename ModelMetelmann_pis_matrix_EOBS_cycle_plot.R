# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R

#per plottare anni indipendenti


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)

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

# correzione degli NaN con formula geomatrica
E0_m_c <- apply(E0_m, 2, function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
# E0_m_c <- E0_m

#Metelamnn, geometric mean. = exp(mean(log))
years_sel_1 = 2007:2014 # # 2006:2016
E0_m_c_sel_1 <- apply(E0_m[which(years %in% years_sel_1),], 2,
                    function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_sel_1 = apply(E0_m_c_sel_1, 2,
               function(x){exp(mean(log(x)))})

years_sel_2 = 2015:2022 # 2017:2023 
E0_m_c_sel_2 <- apply(E0_m[which(years %in% years_sel_2),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_sel_2 = apply(E0_m_c_sel_2, 2,
                 function(x){exp(mean(log(x)))})

E0_diff = (E0_sel_2 - E0_sel_1)/E0_sel_1

# to plot

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) 

br = c(0, 10^(-3:3), 10^10)
br_diff = c(-10^c(2, log(50,10), 0), 0, 10^(0:3), 10^10)

#green-yellow-red-purple
col_br= c("#007917", "#65992E", "#8FB338", "#E2CF4D", "#F89061", "#F96970", "#F97ADC", "#A494FB")

#Metelmann map
col_br= c("#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3", "#F2CDBB", "#F29878", "#D04B45", "#B00026")

domain_years_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_1 = E0_sel_1)%>%
  mutate(E0_1_level=cut(E0_1, breaks=br,
                      labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_1_level=factor(as.character(E0_1_level), levels=rev(levels(E0_1_level)))) %>%
  mutate(E0_2 = E0_sel_2)%>%
  mutate(E0_2_level=cut(E0_2, breaks=br,
                        labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_2_level=factor(as.character(E0_2_level), levels=rev(levels(E0_2_level)))) %>%
  mutate(E0_diff = E0_diff)%>%
  mutate(E0_diff_level=cut(E0_diff, breaks=br_diff,
                           labels=sapply(br_diff[-length(br_diff)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_diff_level=factor(as.character(E0_diff_level), levels=rev(levels(E0_diff_level))))

if (name == "France") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
} else if (name == "Occitanie") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
  regions_sh <- regions_sh %>%
    filter(Region == "Languedoc-Roussillon et Midi-P")
} else if (name == "W_EU") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/W_EU_s.shp")
}

obs_Kramer <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/Kramer_2015_Albo_W_EU.shp")
obs_GBIF <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Albo_W_EU.shp")

# Plots: 2010 and 2020s

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(paste0("R0 diapausing eggs, period = ", min(years_sel_1), "-", max(years_sel_1)))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  geom_sf(data = obs_GBIF, alpha = 1, colour = "green", size=0.3)+
  geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(paste0("R0 diapausing eggs, period = ", min(years_sel_2), "-", max(years_sel_2)))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_diff_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  ggtitle(paste0("% multiplier R0 diapausing eggs, [",
                 min(years_sel_2),"-", max(years_sel_2),"] vs [",
                 min(years_sel_1),"-", max(years_sel_1),"]"))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_v))+ #E0_v
  scale_fill_gradient(trans = "log")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

# alternative

E0_freq_m <- colSums(E0_m>1, na.rm = T)/colSums(E0_m>0, na.rm = T)

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_freq_m), colour = NA)+ #
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow")+
  ggtitle(paste0("R0 diapausing eggs, period = ", min(years_sel), "-", max(years_sel)))
# + scale_fill_gradient(trans = "log")


## New areas with R0 > 1 (Risk of establishment)

domain_indicators <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_hist = E0_sel_1)%>%
  mutate(E0_recent = E0_sel_2)%>%
  mutate(Risk_zones = case_when(
    (E0_hist > 1) & (E0_recent > 1) ~ "Consolidated areas",
    (E0_hist < 1) & (E0_recent > 1) ~ "New risk areas",
    (E0_hist > 1) & (E0_recent < 1) ~ "Decline ares",
    (E0_hist < 1) & (E0_recent < 1) ~ "Persistently absence",
    .default = "No possible comparison"
  ))

ggplot()+
  geom_sf(data = domain_indicators, aes(fill = Risk_zones), colour = NA)

### plot for EMERGENCE conference
library(ggspatial)
library(prettymapr)

regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")

domain_years_sel_FR <- domain_years_sel %>%
  filter(!is.na(Country))

#plot 1

ggplot()+
  geom_sf(data = domain_years_sel_FR, aes(fill = E0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  # geom_sf(data = obs_GBIF, alpha = 1, colour = "green", size=0.3)+
  # geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(bquote(E[0]~~(period~2006-2014)))+
  theme_test()+
  guides(fill=guide_legend(title=bquote(E[0])))
# + scale_fill_gradient(trans = "log")

#plot 2

ggplot()+
  annotation_map_tile(zoom = 6, cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(data = domain_years_sel_FR, aes(fill = E0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  # geom_sf(data = obs_GBIF, alpha = 1, colour = "green", size=0.3)+
  # geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(bquote(E[0]~~(period~2015-2022)))+
  theme_test()+
  guides(fill=guide_legend(title=bquote(E[0])))

#ylab(expression(Anthropogenic~SO[4]^{"2-"}~(ngm^-3)))+