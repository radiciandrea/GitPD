# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R

# to vaildate with data of presence and absence

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

# correzione degli NaN con formula geomatrica
E0_m_c <- apply(E0_m, 2, function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})

#Metelamnn, geometric mean. = exp(mean(log))
years_sel_1 = 2006:2014 # # 2006:2016
E0_m_c_sel_1 <- apply(E0_m[which(years %in% years_sel_1),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_2006_2014 = apply(E0_m_c_sel_1, 2,
                 function(x){exp(mean(log(x)))})

years_sel_2 = 2015:2023 # 2017:2023 
E0_m_c_sel_2 <- apply(E0_m[which(years %in% years_sel_2),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_2015_2023 = apply(E0_m_c_sel_2, 2,
                 function(x){exp(mean(log(x)))})

years_sel_3 = 2018:2023 # 2017:2023 
E0_m_c_sel_3 <- apply(E0_m[which(years %in% years_sel_3),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_2018_2023 = apply(E0_m_c_sel_3, 2,
                     function(x){exp(mean(log(x)))})

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) 

domain_years_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_2006_2014 = E0_2006_2014)%>%
  mutate(E0_2015_2023 = E0_2015_2023)%>%
  mutate(E0_2018_2023 = E0_2018_2023)

#2006_2014
obs_Kramer <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/Kramer_2015_Albo_W_EU.shp")
#2015_2023
obs_GBIF <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Albo_W_EU.shp") 

# observation intersection 

obs_GBIF <- obs_GBIF %>%
  dplyr::select(c("gbifID"))

domain_GBIF <-st_join(obs_GBIF, domain_sel, join = st_within)
#domain_GBIF <-st_join(domain_sel, obs_GBIF, join = st_contains) 

count_intersection <- count(as_tibble(domain_GBIF), region) 

domain_presence <- left_join(domain_sel, count_intersection) %>%
  mutate(presence = 1*(!is.na(n)))%>%
  select(c("region", "presence"))%>%
  st_drop_geometry()

# Load and elaborate ECDC absences

# ECDC <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/ECDC/20230828_VectorFlatFileGDB.shp")
# 
# #filter albopictus, modify status
# 
# ECDC_Albo <- ECDC %>%
#   filter(VectorSpec == "Aedes albopictus")%>%
#   mutate(Status = case_when(AssessedDi == "INV001A" ~ "Established",
#                             AssessedDi == "INV002A" ~ "Introduced",
#                             AssessedDi == "INV003A" ~ "Absent",
#                             AssessedDi == "INV004A" ~ "No data")) %>%
#   select(c("OBJECTID", "VectorSpec", "Status", "LocationCo", "LocationNa", "CountryCod", "Date_Map_T", "geometry"))
#
# ggplot()+
#   geom_sf(data = ECDC_Albo, aes(fill = Status))
# 
# st_write(ECDC_Albo, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/ECDC/20230828_ECDC_Albo.shp")

ECDC_Albo <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/ECDC/20230828_ECDC_Albo.shp")

#spatial issue
sf::sf_use_s2(FALSE)
ECDC_Albo <- sf::st_make_valid(ECDC_Albo)

domain_intersect <- st_intersection(ECDC_Albo, domain_sel)

# st_write(domain_intersect, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/ECDC/domain_intersect.shp")

#once obtained the intersect: i want a vector (or dataframe) of all cells that are only "absent".
# For instance, I could assign 1 to all non-absence, and then consider only those regions who sum to 0

tic()
domain_absence <- domain_intersect %>%
  mutate(Status_code = 1*(Status != "Absent"))%>%
  select(c("region", "Status_code"))%>%
  group_by(region)%>%
  dplyr::summarise(absence = 1*sum(Status_code)==0) %>%
  ungroup() %>%
  st_drop_geometry()
toc() # 89.1 seconds 

# join all and doing some manipulations: set NA absence to 0, 
#overlapping presence and absence -> presence wins.

domain_years_sel_presence <- left_join(domain_years_sel, domain_presence)
domain_years_sel_p_a <- left_join(domain_years_sel_presence, domain_absence)%>%
  mutate(absence = case_when(is.na(absence) ~ 0, 
                             .default = absence))%>%
  mutate(absence = case_when(absence+presence == 2 ~ 0, 
                             .default = absence))%>%
  mutate(status = case_when(absence == 1 ~ 0,
                            presence == 1 ~ 1,
                            .default = NA))

# ROC AUC
# https://www.youtube.com/watch?v=qcvAqAH60Yw

library(pROC)

category = domain_years_sel_p_a$status[which(!is.na(domain_years_sel_p_a$status))]
prediction = domain_years_sel_p_a$E0_2015_2023[which(!is.na(domain_years_sel_p_a$status))]

category = category[which(!is.na(prediction))]
prediction = prediction[which(!is.na(prediction))]

thr = 1
prediction_th = 1*(prediction>thr)
sensitivity_th = sum((prediction_th+category)==2, na.rm = T)/sum(category)
specificity_th = sum((prediction_th+category)==0, na.rm = T)/(sum(category==0))

png(file= paste0(folder_plot, "ROC_AUC_Europe_gbif_ecdc.png"),width = 480, height = 480, res = 150, units = "px")
par(pty = "s")
#roc(obese, glm.fit$fitted.values, plot = TRUE)
roc(category , prediction, plot = TRUE, col = "#377eb8", lwd = 3, print.thres=TRUE)
# points(y = sensitivity_th, x = specificity_th , col = "red")
# text(y = sensitivity_th -0.03, x = specificity_th -0.32, paste0("biological threshold: 1 (",
#                                                           round(specificity_th ,3), ",", round(sensitivity_th,3), ")"))

dev.off()

# x = (1- specificity) = false positive %
# y = (sensitivity) = true positive %


# plot (ModelMetelmann_pis_matrix_EOBS_cycle_plot)

regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/W_EU_s.shp")

E0_sel_1 = E0_2006_2014
E0_sel_2 = E0_2015_2023
E0_sel_3 = E0_2018_2023

br = c(0, 10^(-3:3), 10^10)

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
  mutate(E0_3 = E0_sel_3)%>%
  mutate(E0_3_level=cut(E0_3, breaks=br,
                        labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_3_level=factor(as.character(E0_3_level), levels=rev(levels(E0_3_level))))


folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/ArtiConForm/05_AeAlbopictus_ImpactrecentClimateChange/Images/"

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  xlim(c(-15,20))+
  ylim(c(36, 60))+
  ggtitle(paste0("E0 diapausing eggs, period = ", min(years_sel_1), "-", max(years_sel_1)))+
  theme_void()
# + scale_fill_gradient(trans = "log")

ggsave(file= paste0(folder_plot, "E0_1_level.png"), units="in", width=5, height=7, dpi=300)

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  xlim(c(-15,20))+
  ylim(c(36, 60))+
  ggtitle(paste0("E0 diapausing eggs, period = ", min(years_sel_2), "-", max(years_sel_2)))+
  theme_void()

ggsave(file= paste0(folder_plot, "E0_2_level.png"), units="in", width=5, height=7, dpi=300)

# with pres-abs

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_3_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = ECDC_Albo %>% filter(Status == "Absent"), fill = "white", alpha = 0.3, color = "black")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  geom_sf(data = obs_GBIF, alpha = 1, colour = "yellow", size=0.3)+
  ggtitle(paste0("E0 diapausing eggs, period = ", min(years_sel_3), "-", max(years_sel_3)))+
  xlim(c(-14,19))+
  ylim(c(37, 59))+
  theme_void()
# + scale_fill_gradient(trans = "log")

ggsave(file= paste0(folder_plot, "E0_3_level+gbif-ecdc.png"), units="in", width=5, height=7, dpi=300)
