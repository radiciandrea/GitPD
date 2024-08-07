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

# observation intersection 

obs_GBIF <- obs_GBIF %>%
  dplyr::select(c("gbifID"))

domain_GBIF <-st_join(obs_GBIF, domain_sel, join = st_within)
#domain_GBIF <-st_join(domain_sel, obs_GBIF, join = st_contains) 

count_intersection <- count(as_tibble(domain_GBIF), region) 

domain_years_sel <- left_join(domain_years_sel, count_intersection) %>%
  mutate(presence = !is.na(n))

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
domain_intersect <- domain_intersect %>%
  mutate(Status_code = 1*(Status != "Absent"))%>%
  select(c("region", "Status_code"))%>%
  group_by(region)%>%
  dplyr::summarise(absence = sum(Status_code)==0) %>%
  ungroup()
toc() # 89.1 seconds 

# ROC AUC
# https://www.youtube.com/watch?v=qcvAqAH60Yw

library(pROC)

par(pty = "s")
#roc(obese, glm.fit$fitted.values, plot = TRUE)
roc(obese, glm.fit$fitted.values, plot = TRUE, col = "#377eb8", lwd = 3)

# x = (1- specificity) = false positive %
# y = (sensitivity) = true positive %
