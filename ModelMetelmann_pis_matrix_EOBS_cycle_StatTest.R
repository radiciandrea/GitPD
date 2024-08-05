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

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) 

domain_years_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_2006_2014 = E0_2006_2014)%>%
  mutate(E0_2015_2023 = E0_2015_2023)

#2006_2014
obs_Kramer <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/Kramer_2015_Albo_W_EU.shp")
#2015_2023
obs_GBIF <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Albo_W_EU.shp")

# ROC AUC
# https://www.youtube.com/watch?v=qcvAqAH60Yw

library(pROC)

par(pty = "s")
#roc(obese, glm.fit$fitted.values, plot = TRUE)
roc(obese, glm.fit$fitted.values, plot = TRUE, col = "#377eb8", lwd = 3)

# x = (1- specificity) = false positive %
# y = (sensitivity) = true positive %
