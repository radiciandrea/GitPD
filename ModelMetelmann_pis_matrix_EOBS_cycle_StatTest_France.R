# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R

#Franche scale

# to vaildate with data of presence and absence

# presence: data from Johanna Fite (ANSES) 
# Absence: will take probably (??)

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

years_sel_2 = 2015:2023 # 2017:2023 
E0_m_c_sel_2 <- apply(E0_m[which(years %in% years_sel_2),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_2015_2023 = apply(E0_m_c_sel_2, 2,
                     function(x){exp(mean(log(x)))})

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) 

# select France only

domain_years_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_2015_2023 = E0_2015_2023) %>%
  filter(Country == "France")

# load presence data

folder_ANSES = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/ANSES"
data_presence <- read.csv2(paste0(folder_ANSES, "/communes_colonisees_export_2024.08.28.csv"))

folder_COMM = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/Eurostat_2010"
shp_communes <- st_read(paste0(folder_COMM, "/Comuni.shp"))

data_presence_sf <- left_join(shp_communes, data_presence) %>%
  rename(presence = presence.moustique...import.ajout.manuel.depuis.onglet.colonisation.) %>%
  select(c("insee","presence", "geometry"))

ggplot()+
  geom_sf(data = data_presence_sf, aes(fill = presence), color = NA)

data_presence_sf$presence[which(is.na(data_presence_sf$presence))] = 0

# intersect with domain
sf::sf_use_s2(FALSE)
data_presence_sf <- sf::st_make_valid(data_presence_sf)

domain_presence_intersect <- st_intersection(data_presence_sf, domain_sel)

tic()
domain_prs_abs <- domain_presence_intersect %>%
  select(c("region", "presence"))%>%
  group_by(region)%>%
  # dplyr::summarise(presence = 1*(sum(presence)>0)) %>%
  dplyr::summarise(perc_pres = mean(presence)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  mutate(presence_strong = 1*(perc_pres > 0)) %>%
  mutate(presence = 1*(perc_pres == 1)) %>%
  mutate(absence = 1*(perc_pres == 0))
toc() # 20 seconds 


#plot to check

domain_years_sel_prs_abs <- left_join(domain_years_sel, domain_prs_abs) %>%
  mutate(status = case_when(absence == 1 ~ 0,
                            presence == 1 ~ 1,
                            .default = NA))

# there's a huge overestimation... or underestimation, depending
ggplot()+
  geom_sf(data = domain_years_sel_prs_abs, aes(fill = status), )

# ROC AUC
# https://www.youtube.com/watch?v=qcvAqAH60Yw
# x = (1- specificity) = false positive %
# y = (sensitivity) = true positive %

library(pROC)

#strong (overestimation)

category = domain_years_sel_prs_abs$presence_strong
prediction = domain_years_sel_prs_abs$E0_2015_2023

thr = 1
prediction_th = 1*(prediction>thr)
sensitivity_th = sum((prediction_th+category)==2, na.rm = T)/sum(category)
specificity_th = sum((prediction_th+category)==0, na.rm = T)/(sum(category==0))

par(pty = "s")
#roc(obese, glm.fit$fitted.values, plot = TRUE)
roc(category , prediction, plot = TRUE, col = "#377eb8", lwd = 3, print.thres=TRUE)
points(y = sensitivity_th, x = specificity_th , col = "red")
text(y = sensitivity_th, x = specificity_th -0.35, paste0("biological threshold: 1 (",
                                                          round(specificity_th ,3), ",", round(sensitivity_th,3), ")"))

# weak (underestimation)

category = domain_years_sel_prs_abs %>% filter(!is.na(status)) %>% pull(status)
prediction = domain_years_sel_prs_abs %>% filter(!is.na(status)) %>% pull(E0_2015_2023)

thr = 1
prediction_th = 1*(prediction>thr)
sensitivity_th = sum((prediction_th+category)==2, na.rm = T)/sum(category)
specificity_th = sum((prediction_th+category)==0, na.rm = T)/(sum(category==0))


par(pty = "s")
#roc(obese, glm.fit$fitted.values, plot = TRUE)
roc(category , prediction, plot = TRUE, col = "#377eb8", lwd = 3, print.thres=TRUE)
points(y = sensitivity_th, x = specificity_th , col = "red")
text(y = sensitivity_th, x = specificity_th -0.35, paste0("biological threshold: 1 (",
                                                    round(specificity_th ,3), ",", round(sensitivity_th,3), ")"))

# # Load your ROC curve data
# my_curve <- roc(category, prediction)
# 
# # Choose a specific threshold (e.g., 0.5)
# threshold <- 1
# 
# # Calculate corresponding FPR and TPR
# fpr_threshold <- fpr(my_curve, threshold)
# tpr_threshold <- tpr(my_curve, threshold)
# 
# # Plot the threshold point
# plot(my_curve, print.thres = TRUE)
# abline(v = fpr_threshold, h = tpr_threshold, col = "red", lwd = 2)
