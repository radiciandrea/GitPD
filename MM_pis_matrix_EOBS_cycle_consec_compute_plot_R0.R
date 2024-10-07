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

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec_01" # EOBS_sim_consec
folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_elab" # "EOBS_elab"

files = list.files(folder_out, pattern = "Sim_EOBS")

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
load(paste0(folder_eobs, "/EOBS_sel_01_", years[1], "_", name, ".RData")) #EOBS W_EU #Occitanie #France

# human hosts
H_v <- W_tot_df %>% 
  distinct(region, .keep_all = TRUE) %>%
  pull("pop")

# R0s Zika & Dengue
R0_ZK_tot = matrix(NA, ncol = n_r, nrow = n_d) 
R0_ZK_m = matrix(NA, ncol = n_r, nrow = length(files))

R0_DG_tot = matrix(NA, ncol = n_r, nrow = n_d) 
R0_DG_m = matrix(NA, ncol = n_r, nrow = length(files))

k = 0
for (i in 1:length(files)){
  file = files[i]
  year = years[i]
  
  load(paste0(folder_out, "/", file))
  n_d_i = nrow(Sim)
  
  #Getting weather from EOBS
  load(paste0(folder_eobs, "/EOBS_sel_01_", year, "_", name, ".RData")) #EOBS W_EU #Occitanie #France
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
  EIP_ZK_Cmn = 1.03*(4*exp(5.15 - 0.123*temp)) #Caminade 2016 ("the Dengue one")
  
  #choice EIP
  EIP_ZK = EIP_ZK_Cmn
  EIP_DG = EIP_DN_Mtl
  
  #Vectorial competence
  B = pmax(0,-0.0043*temp^2 + 0.2593*temp - 3.2705) #Benkimoun 2021
  b_v2H = 0.5 # b Blagrove 2020
  
  #to check this:
  b_H2v_ZK_Blg = 0.0665 # beta Blagrove 2020 - cites study in Europe: preferred
  b_H2v_ZK_Cmn = 0.033 # beta Caminade 2016
  b_H2v_DG_Mtl = 0.31 # beta Mtl 2021
  
  #chose
  b_H2v_ZK = b_H2v_ZK_Blg
  b_H2v_DG = b_H2v_DG_Mtl
  
  IIP_DG = 5 #Benkimoun 2021, 
  IIP_ZK= 7 #Caminade 2016 
  
  a = (0.0043*temp + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  
  #host preference
  phi_a_U = 0.9 #human biting preference (urban)
  phi_a_R = 0.5 #human biting preference (rural) #Caminade 2016
  
  
  #choose
  R_th = 50 #defines density over km2 below which an area is "rural", with little phi
  phi_a = phi_a_U*(H_m>R_th)+phi_a_R*(H_m<=R_th)

  # VC = (a*phi_a)^2*m*exp(-mu_A*EIP)/mu_A #Vector capacity as RossMcDonald
  R0_ZK = (a*phi_a)^2*m/(mu_A+mu_A^2*EIP_ZK)*b_v2H*b_H2v_ZK*IIP_ZK # as Zanardini et al.
  R0_DG = (a*phi_a)^2*m/(mu_A+mu_A^2*EIP_DG)*b_v2H*b_H2v_DG*IIP_DG # as Zanardini et al.
  
  n_d_i = nrow(R0_ZK)
  R0_ZK_tot[k + 1:n_d_i,]=R0_ZK
  R0_DG_tot[k + 1:n_d_i,]=R0_DG
  k = k + n_d_i
  R0_ZK_m[i,] = colSums(R0_ZK>1)
  R0_DG_m[i,] = colSums(R0_DG>1)
}

#Choose here what to plot

disease = "dengue" 

if (disease == "Zika"){
  R0_tot = R0_ZK_tot
  R0_m = R0_ZK_m
} else if (disease == "dengue") {
  R0_tot = R0_DG_tot
  R0_m = R0_DG_m
}

# #########################
# #plot
# 
# R0_m_df = data.frame("variable" = "R0",
#                      "region" = rep(regions, each = max(DOS_sim)),
#                      "t" = rep(DOS_sim, n_r),
#                      "value" = c(R0_tot)) #5 classes
# 
# #210
# 
# id_reg = 1597 #
# 
# #Roma: 1091, 1992
# #Nizza: 1597 e un'altra
# 
# region_x = id_reg #regions[id_reg]
# 
# R0_m_x_df <- R0_m_df %>%
#   filter(region == region_x)
# 
# R0_x_df<- dcast(R0_m_x_df, t ~ variable)
# 
# ggplot(R0_m_x_df, aes(x = t, y = value, color = variable))+
#   geom_line()+
#   scale_y_continuous(trans='log2', limits = c(1, max(R0_m_x_df$value)))+
#   # ylim(1, max(R0_m_x_df$value))+
#   # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
#   labs(color = paste0("Abundances per classes (", region_x, ")")) +
#   theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),

#### Geo plot 

years_sel_1 = 2007:2014 # # 2006:2016
R0_1 = colMeans(R0_m[which(years %in% years_sel_1),], na.rm = T)

years_sel_2 = 2015:2022 # 2017:2023 
R0_2 = colMeans(R0_m[which(years %in% years_sel_2),], na.rm = T)


# to plot
domain <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_01_W_EU.shp")) %>%
  arrange(region)

x = c(61, 21, 7, 0, 0)
x_lab = c("e) >9", "d) 3-8", "c) 1-2", "b) < 1", "a) 0")
col_x <- rev(c("#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724"))


# domain <- domain%>%
#   arrange(region) %>%
#   mutate(R0_1 = R0_sel_1)%>%
#   mutate(R0_1_level=cut(R0_1, breaks=br,
#                         labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
#   mutate(R0_1_level=factor(as.character(R0_1_level), levels=rev(levels(R0_1_level)))) %>%
#   mutate(R0_2 = R0_sel_2)%>%
#   mutate(R0_2_level=cut(R0_2, breaks=br,
#                         labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
#   mutate(R0_2_level=factor(as.character(R0_2_level), levels=rev(levels(R0_2_level))))

R0_1_level <- case_when(R0_1 > x[1] ~ x_lab[1],
                        R0_1 > x[2] ~ x_lab[2],
                        R0_1 > x[3] ~ x_lab[3],
                        R0_1 > x[4] ~ x_lab[4],
                        R0_1 == x[5] ~ x_lab[5])

R0_2_level <- case_when(R0_2 > x[1] ~ x_lab[1],
                        R0_2 > x[2] ~ x_lab[2],
                        R0_2 > x[3] ~ x_lab[3],
                        R0_2 > x[4] ~ x_lab[4],
                        R0_2 == x[5] ~ x_lab[5])

Risk_zone <-  case_when((R0_1 < 1) & (R0_2 < 1) ~ "No p. spread",
                        (R0_1 > 1) & (R0_2 > 1) ~ "Hist. p. spread",
                        (R0_1 < 1) & (R0_2 > 1) ~ "New p. spread",
                        (R0_1 > 1) & (R0_2 < 1) ~ "Prev. p. spread")

### plot for CC RIO conference

folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/R0/"

countries_sh <-  st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/european-countries.shp")

#plot 1

g1 <- ggplot()+
  geom_sf(data = domain, aes(fill = R0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_x))+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60)) +
  ggtitle(paste0("Possible spread of ", disease, " (period 2007-2014)"))+
  theme_minimal()+
  guides(fill=guide_legend(title=bquote(R[0]~gt~1~(weeks))))

ggsave(file= paste0(folder_plot, "R0_", disease,"_1_level_01.png"), plot= g1 , units="in", width=5.5, height=7, dpi=300)

#plot 2

g2 <- ggplot()+
  geom_sf(data = domain, aes(fill = R0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_x))+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60)) +
  ggtitle(paste0("Possible spread of ", disease, " (period 2015-2022)"))+
  theme_minimal() +
  guides(fill=guide_legend(title=bquote(R[0]~gt~1~(weeks))))

ggsave(file= paste0(folder_plot, "R0_", disease,"_2_level_01.png"),  plot= g2 , units="in", width=5.5, height=7, dpi=300)

# plot 3

col_x_sint_FR<- c("#384AB4", "#8EB0FE", "#F29878", "#B00026") 

gvar <- ggplot()+
  geom_sf(data = domain, aes(fill = Risk_zone), colour = NA)+
  scale_fill_manual(values = rev(col_x_sint_FR))+
  geom_sf(data = countries_sh, alpha = 0, colour = "grey90")+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60)) +
  ggtitle(paste0("Variation in the spread pattern of ", disease))+
  theme_minimal() +
  guides(fill=guide_legend(title=bquote(R[0]~variation)))

ggsave(file= paste0(folder_plot, "R0_", disease,"_var_level_01.png"), plot= gvar , units="in", width=5.5, height=7, dpi=300)

# Plot Francia (poster ESOVE)

library(ggspatial)
library(prettymapr)
library(ggrepel)
library(RJSONIO)

folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/ArtiConForm/01_Esove/images/"

regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")

domain_FR <- domain %>%
  filter(!is.na(Country))

reg_FR = domain_FR$region

# plot cities (from)

locateCountry = function(nameCity, codeCountry) {
  cleanCityName = gsub(' ', '%20', nameCity)
  url = paste(
    "http://nominatim.openstreetmap.org/search?city="
    , cleanCityName
    , "&countrycodes="
    , codeCountry
    , "&limit=9&format=json"
    , sep="")
  resOSM = fromJSON(url)
  if(length(resOSM) > 0) {
    return(c(resOSM[[1]]$lon, resOSM[[1]]$lat))
  } else return(rep(NA,2)) 
}

cities_df <- data.frame(name = c("Paris", "Marseille", "Lyon", "Toulouse", "Bordeaux", "Nice", "Dijon",
                                 "Lille", "Montpellier", "Strasbourg", "Rennes", "Nantes", "Ajaccio", "Dijon"))
cities_df$code = "FR"

coord = t(apply(cities_df, 1, function(aRow) as.numeric(locateCountry(aRow[1], aRow[2]))))

cities_df$lon = coord[,1]
cities_df$lat = coord[,2]

points <- lapply(1:nrow(coord), function(i){st_point(coord[i,])})
points_sf <- st_sfc(points, crs = 4326)
cities_sf <- st_sf('city' = cities_df$name, 'geometry' = points_sf)  

#plot: degnue 2020 

R0_2020_FR_v <- R0_2[reg_FR]

x = c(61, 21, 7, 0, 0)
x_lab = c("a) >9", "b) 3-8", "c) 1-2", "d) < 1", "e) 0")

R0_2020_FR_f <- case_when(R0_2020_FR_v > x[1] ~ x_lab[1],
                        R0_2020_FR_v > x[2] ~ x_lab[2],
                        R0_2020_FR_v > x[3] ~ x_lab[3],
                        R0_2020_FR_v > x[4] ~ x_lab[4],
                        R0_2020_FR_v == x[5] ~ x_lab[5])

g_D_2020 <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = R0_2020_FR_f), color = NA)+
  scale_fill_viridis_d()+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Lenght period with R0 gt 1 (dengue)"))+
  guides(fill=guide_legend(title="weeks"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_D_2020.png"), g_D_2020, units="in", height=8, width= 6, dpi=300)

### change

palette_pos2 = c( "gray90", "#F2CDBB", "#F29878", "#D04B45", "#B00026")

R0_2010_FR_v <- R0_1[reg_FR]
R0_diff_FR_v <- R0_2020_FR_v - R0_2010_FR_v

R0_diff_FR_v[which(R0_2020_FR_v<1)] = NA

R0_diff_FR_f <- case_when(R0_diff_FR_v > 63 ~"a) > 9 w",
                                   R0_diff_FR_v > 21 ~"b) 3 to 9 w)",
                                   R0_diff_FR_v > 7 ~"c) 1 to 3 w",
                                   R0_diff_FR_v > 2 ~"d) 2 d to 1 w ",
                                   R0_diff_FR_v > -3 ~"e) -2 to +2 d",
                                   .default = NA)

g_D_change <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = R0_diff_FR_f), color = NA)+
  scale_fill_manual(values = rev(palette_pos2), na.value = "transparent")+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Difference in positive transmission period"))+
  guides(fill=guide_legend(title="Increase"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_D_change.png"), g_D_change, units="in", height=8, width= 6, dpi=300)
