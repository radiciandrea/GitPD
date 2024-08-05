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
  
    rm(Sim)
}

# #Metelamnn, geometric mean. = exp(mean(log))
# years_sel_1 = 2006:2016 # # 2006:2016
# E0_m_c_sel_1 <- apply(E0_m[which(years %in% years_sel_1),], 2,
#                       function(x){x[which(is.na(x))] = exp(mean(log(x[which(is.na(x)==F)]))); return(x)})
# E0_sel_1 = apply(E0_m_c_sel_1, 2,
#                  function(x){exp(mean(log(x)))})
# 
# years_sel_2 = 2017:2023 # 2017:2023 
# E0_m_c_sel_2 <- apply(E0_m[which(years %in% years_sel_2),], 2,
#                       function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
# E0_sel_2 = apply(E0_m_c_sel_2, 2,
#                  function(x){exp(mean(log(x)))})
# 
# E0_diff = (E0_sel_2 - E0_sel_1)/E0_sel_1
# 
# # to plot
# 
# years_sel = years_sel_2
# E0_sel = E0_sel_2
# 
# br = c(0, 10^(-3:3), 10^10)
# br_diff = c(-10^c(2, log(50,10), 0), 0, 10^(0:3), 10^10)
# 
# #green-yellow-red-purple
# col_br= c("#007917", "#65992E", "#8FB338", "#E2CF4D", "#F89061", "#F96970", "#F97ADC", "#A494FB")
# 
# #Metelmann map
# col_br= c("#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3", "#F2CDBB", "#F29878", "#D04B45", "#B00026")
# 
# domain_years_sel <- domain_sel%>%
#   arrange(region) %>%
#   mutate(E0 = E0_sel)%>%
#   mutate(E0_level=cut(E0, breaks=br,
#                       labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
#   mutate(E0_level=factor(as.character(E0_level), levels=rev(levels(E0_level)))) %>%
#   mutate(E0_diff = E0_diff)%>%
#   mutate(E0_diff_level=cut(E0_diff, breaks=br_diff,
#                            labels=sapply(br_diff[-length(br_diff)], function(x){paste0(">", as.character(x))}))) %>%
#   mutate(E0_diff_level=factor(as.character(E0_diff_level), levels=rev(levels(E0_diff_level))))
# 
# if (name == "France") {
#   regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
# } else if (name == "Occitanie") {
#   regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
#   regions_sh <- regions_sh %>%
#     filter(Region == "Languedoc-Roussillon et Midi-P")
# } else if (name == "W_EU") {
#   regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/W_EU_s.shp")
# }
