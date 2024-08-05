# plot geo delle attività stagionali di 

# MM_pis_matrix_EOBS_cycle_consec_geo

#per plottare anni indipendenti


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)

type = "_consec"

folder_out = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim", type)

files = list.files(folder_out)

name = "W_EU"
years = substring(files, nchar(type)+8, nchar(type)+11)

# load domain
domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name,".shp")) %>%
  arrange(region)

people_ha = domain_sel$popkm2/100

#indicatori
n_r = nrow(domain_sel) # numer of regions

# fist appereance (date where M/A > 1)
F0_m = matrix(NA, ncol = n_r, nrow = length(files))

# seasonal lenght (number of days where M/A > 1)
S0_m = matrix(NA, ncol = n_r, nrow = length(files))

# abundance - mean number of adult per person during season
A0_m = matrix(NA, ncol = n_r, nrow = length(files))

for (i in 1:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  A_sim = Sim[,1+(1+n_r*3):(4*n_r)]
  A_sim_pp = A_sim/matrix(rep(people_ha, nrow(A_sim)), nrow = nrow(A_sim), byrow = T)
  
  #remove Inf and NaN: set them as 0
  A_sim_pp[which(is.finite(A_sim_pp)==F)] = 0
  
  F0_m[i,] = apply(A_sim_pp, 2, function(x){which(x>1)[1]})
  S0_m[i,] = apply(A_sim_pp, 2, function(x){sum(x>1)})
  A0_m[i,] = apply(A_sim_pp, 2, function(x){mean(x[which(x>1)])})
}

# mean plots over 2 periods

# we should correct 2 problems: NaN; Inf; 

years_sel_1 = 2006:2016 # # 2006:2016
F0_sel_1 = apply(F0_m[years %in% years_sel_1,], 2, function(x){mean(x, na.rm = T)})
S0_sel_1 = apply(S0_m[years %in% years_sel_1,], 2, function(x){mean(x, na.rm = T)})
A0_sel_1 = apply(A0_m[years %in% years_sel_1,], 2, function(x){mean(x, na.rm = T)})

years_sel_2 = 2017:2023 # 2017:2023
F0_sel_2 = apply(F0_m[years %in% years_sel_2,], 2, function(x){mean(x, na.rm = T)})
S0_sel_2 = apply(S0_m[years %in% years_sel_2,], 2, function(x){mean(x, na.rm = T)})
A0_sel_2 = apply(A0_m[years %in% years_sel_2,], 2, function(x){mean(x, na.rm = T)})

# to plot

if (name == "France") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
} else if (name == "Occitanie") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
  regions_sh <- regions_sh %>%
    filter(Region == "Languedoc-Roussillon et Midi-P")
} else if (name == "W_EU") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/W_EU_s.shp")
}

# beginning
ggplot()+
  geom_sf(data = domain_sel, aes(fill = F0_sel_1), colour = NA)+ #
  ggtitle("beginning seasonal activity (date ratio mosquito/person > 1), period 2006-2016")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = F0_sel_2), colour = NA)+ #
  ggtitle("beginning seasonal activity, period 2017-2023")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = F0_sel_1 - F0_sel_2), colour = NA)+ #
  scale_fill_gradient2(
    low = "#5570DF",
    mid = "white",
    high = "#D04B45",
    midpoint = 0,
    na.value = "grey50")+
  ggtitle("anticipation seasonal activity, period 10's vs 20's ")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")


# length
ggplot()+
  geom_sf(data = domain_sel, aes(fill = S0_sel_1), colour = NA)+ #
  ggtitle("length seasonal activity (#days ratio mosquito/person > 1), period 2006-2016")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = S0_sel_2), colour = NA)+ #
  ggtitle("length seasonal activity, period 2017-2023")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = S0_sel_2 - S0_sel_1), colour = NA)+ #
  scale_fill_gradient2(
    low = "#5570DF",
    mid = "white",
    high = "#D04B45",
    midpoint = 0,
    na.value = "grey50")+ #
  ggtitle("variation length seasonal activity, period 10's vs 20's ")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

# abundance
ggplot()+
  geom_sf(data = domain_sel, aes(fill = A0_sel_1), colour = NA)+ #
  scale_fill_continuous(trans = "log")+
  ggtitle("mosquito abundance per person (during M/A>0), period 2006-2016")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = A0_sel_2), colour = NA)+ #
  scale_fill_continuous(trans = "log")+
  ggtitle("mosquito abundance per person, period 2017-2023")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = A0_sel_2 - A0_sel_1), colour = NA)+ #
  scale_fill_gradient2(
    low = "#5570DF",
    mid = "white",
    high = "#D04B45",
    midpoint = 0,
    na.value = "grey50")+ #
  ggtitle("variation mosquito abundance per person, period 10's vs 20's")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")
