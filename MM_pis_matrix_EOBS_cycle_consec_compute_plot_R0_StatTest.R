# plot del cycle
# MM_pis_matrix_EOBS_cycle_consec.R

#per plottare anni consecutivi + calcolare R0

rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(lubridate)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec"
folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_elab"

files = list.files(folder_out, pattern = ".RData")

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

#carichiamo 1 per la popolazion
load(paste0(folder_eobs, "/EOBS_sel_", years[1], "_", name, ".RData")) #EOBS W_EU #Occitanie #France

# human hosts
H_v <- W_tot_df %>% 
  distinct(region, .keep_all = TRUE) %>%
  pull("pop")

# R0s Dengue

R0_DG_tot = matrix(NA, ncol = n_r, nrow = n_d) 
R0_DG_m = matrix(NA, ncol = n_r, nrow = length(files))

k = 0
for (i in 1:length(files)){
  file = files[i]
  year = years[i]
  
  load(paste0(folder_out, "/", file))
  n_d_i = nrow(Sim)
  
  #Getting weather from EOBS
  load(paste0(folder_eobs, "/EOBS_sel_", year, "_", name, ".RData")) #EOBS W_EU #Occitanie #France
  temp = matrix(W_tot_df$T_av, nrow = n_d_i)
  
  #bio
  Adults = Sim[, 1+(n_r*3+1):(n_r*4)]*100 #per km2
  H_m =   matrix(rep(H_v , n_d_i), nrow = n_d_i, byrow = T )
  m = Adults/H_m # adult female osquito to human ration
  
  #parameters
  mu_A = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
  mu_A[which(temp<=0)] = -log(0.677 * exp(-0.5*((temp[which(temp<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  #EIP
  EIP_DN_Bnk = 0.11*temp^2 - 7.13*temp +121.17 #Benkimoun 2021
  EIP_DN_Mtl = 1.03*(4*exp(5.15 - 0.123*temp)) #Metelmann 2021

  #choice EIP
  EIP_DG = EIP_DN_Mtl
  
  #Vectorial competence
  B = pmax(0,-0.0043*temp^2 + 0.2593*temp - 3.2705) #Benkimoun 2021
  b_v2H = 0.5 # b Blagrove 2020
  
  #to check this:
  b_H2v_DG_Mtl = 0.31 # beta Mtl 2021
  
  #ch
  b_H2v_DG = b_H2v_DG_Mtl
  
  IIP_DG = 5 #Benkimoun 2021, 

  a = (0.0043*temp + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  
  #host preference
  phi_a_U = 0.9 #human biting preference (urban)
  phi_a_R = 0.5 #human biting preference (rural) #Caminade 2016
  
  #choose
  R_th = 50 #defines density over km2 below which an area is "rural", with little phi
  phi_a = phi_a_U*(H_m>R_th)+phi_a_R*(H_m<=R_th)
  
  # VC = (a*phi_a)^2*m*exp(-mu_A*EIP)/mu_A #Vector capacity as RossMcDonald
  R0_DG = (a*phi_a)^2*m/(mu_A+mu_A^2*EIP_DG)*b_v2H*b_H2v_DG*IIP_DG # as Zanardini et al.
  
  n_d_i = nrow(R0_DG)
  R0_DG_tot[k + 1:n_d_i,]=R0_DG
  k = k + n_d_i
  R0_DG_m[i,] = colSums(R0_DG>1)
}

R0_tot = R0_DG_tot
R0_m = R0_DG_m

years_sel = 2010:2023 # as ECDC
R0_l = colMeans(R0_m[which(years %in% years_sel),], na.rm = T)

# to plot
domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_W_EU.shp")) %>%
  select(region) %>%
  arrange(region) %>%
  mutate(R0_l = R0_l)

# ggplot()+
#   geom_sf(data = domain_sel, aes(fill = log(R0_l)), colour = NA)

#load presence data
#https://www.ecdc.europa.eu/en/all-topics-z/dengue/surveillance-and-disease-data/autochthonous-transmission-dengue-virus-eueea

#and remove not included locations
ECDC_Dengue <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/ECDC/Dengue_from_site_cleaned.shp") %>%
  filter(Infection != "Not inc")

sf::sf_use_s2(FALSE)
ECDC_Dengue <- sf::st_make_valid(ECDC_Dengue)

domain_Dengue <- st_intersection(ECDC_Dengue, domain_sel)%>%
  st_drop_geometry()%>%
  mutate(status = case_when(Infection == "Yes" ~ 1,
                   .default = 0)) %>%
  group_by(region)%>%
  dplyr::summarise(status = 1*sum(status)>0) %>%
  ungroup()

domain_Dengue <- left_join(domain_Dengue, domain_sel)


library(pROC)

category = domain_Dengue$status
prediction = domain_Dengue$R0_l

category = category[which(!is.na(prediction))]
prediction = prediction[which(!is.na(prediction))]

par(pty = "s")

roc(category , prediction, plot = TRUE, col = "#377eb8", lwd = 3, print.thres=TRUE)

# x = (1- specificity) = 1 - true negative %
# y = (sensitivity) = true positive %

thr_v = 0.107

#other values?

thr = 10 # days
prediction_th = 1*(prediction>thr)
sensitivity_th = sum((prediction_th+category)==2, na.rm = T)/sum(category)
specificity_th = sum((prediction_th+category)==0, na.rm = T)/(sum(category==0))

roc(category , prediction, plot = TRUE, col = "#377eb8", lwd = 3, print.thres=TRUE)
points(y = sensitivity_th, x = specificity_th , col = "red")
text(y = sensitivity_th -0.03, x = specificity_th -0.03, paste0(thr, " day(s) (",
                                                                round(specificity_th ,3), ",", round(sensitivity_th,3), ")"))


#plots

ggplot()+
  geom_sf(data = domain_sel, aes(fill = R0_l), colour = NA)+
  geom_sf(data = ECDC_Dengue, alpha = 0, color = "gray90", lwd = 0.1)+ 
  geom_sf(data = ECDC_Dengue %>% filter(Infection == "Yes"), fill = "red", alpha = 0.4, color = NA)+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60))

ggplot()+
  geom_sf(data = domain_sel, aes(fill = R0_l), colour = NA)+
  geom_sf(data = ECDC_Dengue, alpha = 0, color = "gray90", lwd = 0.1)+ 
  geom_sf(data = domain_sel %>% filter(R0_l > thr_v), fill = "orange", alpha = 0.4, color = NA)+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60))
