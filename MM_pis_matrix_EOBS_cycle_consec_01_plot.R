# plot del cycle
# MM_pis_matrix_EOBS_cycle_consec_01.R

#per individuare indicatori di attività e di presenza di adulti


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(lubridate)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec_01" # EOBS_sim_consec
folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_elab_01" # "EOBS_elab"

name = "W_EU"

files = list.files(folder_out, pattern = "Sim_EOBS")

name = substring(files[1], 10, 13)
#years = substring(files, 15, 18)

years = 2005:2023

first_day = as.Date(paste0(min(years), "-01-01"))
last_day = as.Date(paste0(max(years), "-12-31"))

date_sim = seq(from = first_day, to = last_day, by = 'day')
DOS_sim = 1:length(date_sim)
n_d = length(DOS_sim)

# #carichiamo 1 per le dimensioni
# load(paste0(folder_out, "/", files[1]))
# 
# n_c = 5 # numero di classi
# n_r = (ncol(Sim)-1)/n_c #numero di regioni
# regions = 1:n_r
# 
# n_d_i = nrow(Sim)
# Sim_tot = matrix(NA, ncol = n_c*n_r+1, nrow = n_d)
# Sim_tot[1:n_d_i,]=Sim
# 
# #same for beta
# beta_approx_tot = matrix(NA, ncol = n_r, nrow = n_d)
# beta_approx_tot[1:n_d_i,]=beta_approx
# 
# k = n_d_i
# for (i in 2:length(files)){
#   file = files[i]
#   load(paste0(folder_out, "/", file))
#   n_d_i = nrow(Sim)
#   Sim_tot[k + 1:n_d_i,]=Sim
#   beta_approx_tot[k + 1:n_d_i,]=beta_approx
#   k = k + n_d_i
# }
# 
# # set E_d_0 consect
# 
# file = files[length(files)]
# load(paste0(folder_out, "/", file))
# 
# E_d_0 = pmax(1, Sim[nrow(Sim), 1+(n_r*4+1):(n_r*5)])
# E_d_0[which(is.na(E_d_0))] =1
# 
# save(E_d_0, file =
#        paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Data_sim/X0_E0_consec_01_", name, ".RData"))
# 
# 
# #########################
# 
# #extract adults and eggs
# index = 1+(1+3*n_r):(4*n_r)
# 
# Adults_m = Sim_tot[, index]
# LE_m = Adults_m*beta_approx_tot
# 
# save(Adults_m, LE_m, file = paste0(folder_out, "/LE_Adults.RData"))
# 
load(paste0(folder_out, "/LE_Adults.RData"))

# Recompute n_c, n_r...

years = 2005:2022

first_day = as.Date(paste0(min(years), "-01-01"))
last_day = as.Date(paste0(max(years), "-12-31"))

date_sim = seq(from = first_day, to = last_day, by = 'day')
DOS_sim = 1:length(date_sim)
n_d = length(DOS_sim)

n_c = 5 # numero di classi
n_r = (ncol(Adults_m)) #numero di regioni
regions = 1:n_r
n_y = length(years)

# correct NAs into 0s; same for negative values or estreme values (i.e. > 10^8)

Adults_m[which(is.na(Adults_m))] = 0
Adults_m[which(Adults_m < 0)] = 0
Adults_m[which(Adults_m > 10^8)] = 0

# Adults_RM_m <- t(sapply(1:n_d,
#                        function(x){return(colMeans(Adults_m[max(1,(x-7)):min(n_d,(x+7)),]))}))

LE_m[which(is.na(LE_m))] = 0
LE_m[which(LE_m < 0)] = 0
LE_m[which(LE_m > 10^8)] = 0

# LE_RM_m <- t(sapply(2:n_d,
#                     function(x){return(colMeans(LE_m[max(1,(x-7)):min(n_d,(x+7)),]))}))

# compute annual averages for years (summer s)
Ab_s_df <- data.frame(region = rep(regions, times = n_y),
                                 year = rep(years, each = n_r),
                                 Adults = NA,
                                 LE = NA)

for (y in years){
  mjjas = seq(as.Date(paste0(y, "-05-01")), as.Date(paste0(y, "-09-30")), 1)
  
  i_mjjas = which(date_sim %in% mjjas)
  
  Ab_s_df$Adults[which(Ab_s_df$year == y)] = colMeans(Adults_m[i_mjjas,])
  Ab_s_df$LE[which(Ab_s_df$year == y)] = colMeans(LE_m[i_mjjas,])
  
}

# compute statistics for 2 periods

LE_av_s_2010_v =  Ab_s_df %>% filter(year < 2015) %>% group_by(region) %>% summarise(LE_av = mean(LE)) %>% ungroup() %>% pull(LE_av)
LE_av_s_2020_v =  Ab_s_df %>% filter(year >= 2015) %>% group_by(region) %>% summarise(LE_av = mean(LE)) %>% ungroup() %>% pull(LE_av)
Adults_av_s_2010_v =  Ab_s_df %>% filter(year < 2015) %>% group_by(region) %>% summarise(Adults_av = mean(Adults)) %>% ungroup() %>% pull(Adults_av)
Adults_av_s_2020_v =  Ab_s_df %>% filter(year >= 2015) %>% group_by(region) %>% summarise(Adults_av = mean(Adults)) %>% ungroup() %>% pull(Adults_av)

#summer "MJJAS" - mean abundances

# load shp to compare
folder_sh = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/"
folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/MM_pis_matrix_EOBS_cycle_consec_01_plot/"

domain <- st_read(paste0(folder_sh, "domain_sel_01_W_EU.shp")) %>%
  arrange(region)

countries_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/european-countries.shp")

# my_breaks = c(10^(c(4:0)), 0)
# 
# LE_av_s_2010_gg <- ggplot()+
#   geom_sf(data = domain, aes(fill = LE_av_s_2010_v), color = NA)+
#   geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
#   coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
#   scale_fill_gradientn(colors = palette1, 
#                        na.value = "transparent", 
#                        transform = "log",
#                        breaks = my_breaks,
#                        labels = my_breaks) +
#   ggtitle(paste0("Average daily laid eggs per ha between May and September"))+
#   guides(fill=guide_legend(title="eggs/ha"))+
#   theme_minimal()
# 
# ggsave(paste0(folder_plot, "LE_av_s_2010_gg.png"), LE_av_s_2010_gg, units="in", width=7, height=5.5, dpi=300)

# abundance in mjjas: # 1/6 times (EGGS 2010)

LE_av_s_2010_f <- case_when(LE_av_s_2010_v  > 10^4 ~ "a) > 10^4",
                            LE_av_s_2010_v > 10^3 ~ "b) > 10^3",
                            LE_av_s_2010_v  > 10^2 ~ "c) > 10^2",
                            LE_av_s_2010_v  >= 10 ~ "d) > 10",
                            LE_av_s_2010_v  < 10 ~ "e) < 10")

LE_av_s_2010_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_av_s_2010_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Average daily laid eggs per ha between May and September"))+
  guides(fill=guide_legend(title="eggs/ha"))+
  theme_minimal()

ggsave(paste0(folder_plot, "LE_av_s_2010_gg.png"), LE_av_s_2010_gg, units="in", width=7, height=5.5, dpi=300)

# abundance in mjjas: # 2/6 times (EGGS 2020)

LE_av_s_2020_f <- case_when(LE_av_s_2020_v  > 10^4 ~ "a) > 10^4",
                            LE_av_s_2020_v > 10^3 ~ "b) > 10^3",
                            LE_av_s_2020_v  > 10^2 ~ "c) > 10^2",
                            LE_av_s_2020_v  >= 10 ~ "d) > 10",
                            LE_av_s_2020_v  < 10 ~ "e) < 10")

LE_av_s_2020_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_av_s_2020_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Average daily laid eggs per ha between May and September"))+
  guides(fill=guide_legend(title="eggs/ha"))+
  theme_minimal()

ggsave(paste0(folder_plot, "LE_av_s_2020_gg.png"), LE_av_s_2020_gg, units="in", width=7, height=5.5, dpi=300)

# abundance in mjjas: # 3/6 times (difference EGGS 2020)

#temporaneo :
palette_simp <- c("#384AB4", "#8EB0FE", "gray80", "#F29878", "#B00026")


LE_av_s_diff_v <- LE_av_s_2020_v - LE_av_s_2010_v

LE_av_s_diff_f <- case_when(LE_av_s_diff_v > 10^3 ~"a) strong increase (>+1000)",
                            LE_av_s_diff_v > 10 ~"b) increase (+10 to 1000)",
                            LE_av_s_diff_v > -10 ~"c) stable (-10 to +10)",
                            LE_av_s_diff_v >= -10^3 ~"d) decrease (-10 to 1000)",
                            LE_av_s_diff_v < -10^3 ~"e) strong decrease (<-1000)",
                            .default = NA)

LE_av_s_diff_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_av_s_diff_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_manual(values = rev(palette_simp))+
  ggtitle(paste0("Difference in eggs density in summer"))+
  guides(fill=guide_legend(title="eggs/ha"))+
  theme_minimal()

ggsave(paste0(folder_plot, "LE_av_s_diff_gg.png"), LE_av_s_diff_gg, units="in", width=7, height=5.5, dpi=300)

# abundance in mjjas: # 4/6 times (Adults 2010)

Adults_av_s_2010_f <- case_when(Adults_av_s_2010_v  > 10^4 ~ "a) > 10^4",
                            Adults_av_s_2010_v > 10^3 ~ "b) > 10^3",
                            Adults_av_s_2010_v  > 10^2 ~ "c) > 10^2",
                            Adults_av_s_2010_v  >= 10 ~ "d) > 10",
                            Adults_av_s_2010_v  < 10 ~ "e) < 10")

Adults_av_s_2010_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = Adults_av_s_2010_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Average Adults per ha between May and September"))+
  guides(fill=guide_legend(title="Adults/ha"))+
  theme_minimal()

ggsave(paste0(folder_plot, "Adults_av_s_2010_gg.png"), Adults_av_s_2010_gg, units="in", width=7, height=5.5, dpi=300)

# abundance in mjjas: # 5/6 times (Adults 2020)

Adults_av_s_2020_f <- case_when(Adults_av_s_2020_v  > 10^4 ~ "a) > 10^4",
                            Adults_av_s_2020_v > 10^3 ~ "b) > 10^3",
                            Adults_av_s_2020_v  > 10^2 ~ "c) > 10^2",
                            Adults_av_s_2020_v  >= 10 ~ "d) > 10",
                            Adults_av_s_2020_v  < 10 ~ "e) < 10")

Adults_av_s_2020_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = Adults_av_s_2020_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Average Adults per ha between May and September"))+
  guides(fill=guide_legend(title="Adults/ha"))+
  theme_minimal()

ggsave(paste0(folder_plot, "Adults_av_s_2020_gg.png"), Adults_av_s_2020_gg, units="in", width=7, height=5.5, dpi=300)

# abundance in mjjas: # 6/6 times (difference Adults 2020)

Adults_av_s_diff_v <- Adults_av_s_2020_v - Adults_av_s_2010_v

Adults_av_s_diff_f <- case_when(Adults_av_s_diff_v > 10^3 ~"a) strong increase (>+1000)",
                            Adults_av_s_diff_v > 10 ~"b) increase (+10 to 1000)",
                            Adults_av_s_diff_v > -10 ~"c) stabAdults (-10 to +10)",
                            Adults_av_s_diff_v >= -10^3 ~"d) decrease (-10 to 1000)",
                            Adults_av_s_diff_v < -10^3 ~"e) strong decrease (<-1000)",
                            .default = NA)

Adults_av_s_diff_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = Adults_av_s_diff_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_manual(values = rev(palette_simp))+
  ggtitle(paste0("Difference in Adults density in summer"))+
  guides(fill=guide_legend(title="Adults/ha"))+
  theme_minimal()

ggsave(paste0(folder_plot, "Adults_av_s_diff_gg.png"), Adults_av_s_diff_gg, units="in", width=7, height=5.5, dpi=300)

#  2 weeks rolling mean: standardyze dynamics over 2 weeks (eggs) 

LE_RM_m <- t(sapply(1:n_d,
                    function(x){return(colMeans(LE_m[max(1,(x-7)):min(n_d,(x+7)),]))}))

thr = 0.001

max_LE_RM = max(LE_RM_m)
thr_LE_RM = thr*max_LE_RM

# assume that season starts and and when there is 1 le per 10 mq

thr_LE_RM = 1000 #about 0.0001* of the max

# compute season lenght per year
Ab_s_df$le_LE = NA

for (y in years){
  
  index_y = which(date_sim %in% seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), 1))
  origin_y =  as.Date(paste0(as.numeric(y)-1, "-12-31"))
  
  LE_RM_m_y = LE_RM_m[index_y,]
  
  LE_b_y = as.Date(apply(LE_RM_m_y, 2, function(x){which(x>thr_LE_RM)[1]}), origin = origin_y) #beginning
  LE_e_y = as.Date(apply(LE_RM_m_y, 2, function(x){max(which(x>thr_LE_RM))}), origin = origin_y) #end
  LE_e_y[which(LE_e_y<0)] = NA
  
  
  Ab_s_df$le_LE[which(Ab_s_df$year == y)] = pmax(LE_e_y - LE_b_y, 0, na.rm = T)
}

LE_le_s_2010_v =  Ab_s_df %>% filter(year < 2015) %>% group_by(region) %>% summarise(le_LE_av = mean(le_LE, na.rm = T)) %>% ungroup() %>% pull(le_LE_av)
LE_le_s_2020_v =  Ab_s_df %>% filter(year >= 2015) %>% group_by(region) %>% summarise(le_LE_av = mean(le_LE, na.rm = T)) %>% ungroup() %>% pull(le_LE_av)

# PLOTS

# season lenght: # 1/3 times (EGGS 2010)
x = c("a) > 14 w", "b) < 14 w", "c) < 7 w", "d) < 3 w", "e) 0 w")

LE_le_s_2010_f <- case_when(LE_le_s_2010_v > 98 ~ x[1],
                      LE_le_s_2010_v > 49 ~ x[2],
                      LE_le_s_2010_v > 21 ~ x[3],
                      LE_le_s_2010_v > 0 ~ x[4],
                      LE_le_s_2010_v == 0 ~ x[5])


LE_le_s_2010_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_le_s_2010_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Lenght of the season based on laid eggs, threshold = ", thr_LE_RM, " laid eggs/(ha*d)"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave(paste0(folder_plot, "LE_le_s_2010_gg.png"), LE_le_s_2010_gg, units="in", width=7, height=5.5, dpi=300)

# season lenght: # 2/3 times (EGGS 2020)

LE_le_s_2020_f <- case_when(LE_le_s_2020_v > 98 ~ x[1],
                            LE_le_s_2020_v > 49 ~ x[2],
                            LE_le_s_2020_v > 21 ~ x[3],
                            LE_le_s_2020_v > 0 ~ x[4],
                            LE_le_s_2020_v == 0 ~ x[5])

LE_le_s_2020_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_le_s_2020_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Lenght of the season based on laid eggs, threshold = ", thr_LE_RM, " laid eggs/(ha*d)"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave(paste0(folder_plot, "LE_le_s_2020_gg.png"), LE_le_s_2020_gg, units="in", width=7, height=5.5, dpi=300)

# season lenght: # 3/3 times (difference EGGS 2020)

LE_le_s_diff_v <- LE_le_s_2020_v - LE_le_s_2010_v

LE_le_s_diff_f <- case_when(LE_le_s_diff_v > 14 ~"a) strong increase (> 2 w)",
                            LE_le_s_diff_v >  0 ~"b) increase (< 2 w)",
                            LE_le_s_diff_v > -1 ~"c) stable ",
                            LE_le_s_diff_v >= -14 ~"d) decrease (< 2 w)",
                            LE_le_s_diff_v < -14 ~"e) strong decrease (> 2 w)",
                            .default = NA)

LE_le_s_diff_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_le_s_diff_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(-9, 17), ylim = c(34, 59)) +
  scale_fill_manual(values = rev(palette_simp))+
  ggtitle(paste0("Difference in the length of th season"))+
  guides(fill=guide_legend(title="days"))+
  theme_minimal()

ggsave(paste0(folder_plot, "LE_le_s_diff_gg.png"), LE_le_s_diff_gg, units="in", width=7, height=5.5, dpi=300)

# Francia (poster ESOVE)

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

cities_df <- data.frame(name = c("Paris", "Marseille", "Lyon", "Toulouse", "Bordeaux", "Nice",
                                 "Lille", "Montpellier", "Strasbourg", "Rennes", "Nantes", "Ajaccio", "Dijon"))
cities_df$code = "FR"

coord = t(apply(cities_df, 1, function(aRow) as.numeric(locateCountry(aRow[1], aRow[2]))))

cities_df$lon = coord[,1]
cities_df$lat = coord[,2]

points <- lapply(1:nrow(coord), function(i){st_point(coord[i,])})
points_sf <- st_sfc(points, crs = 4326)
cities_sf <- st_sf('city' = cities_df$name, 'geometry' = points_sf) 

#plot: Adults 2010 

Adults_av_s_2010_FR_v <- Adults_av_s_2010_v[reg_FR]

Adults_av_s_2010_FR_f <- case_when(Adults_av_s_2010_FR_v  > 10^3 ~ "a) > 10^3",
                                   Adults_av_s_2010_FR_v > 10^2 ~ "b) > 10^2",
                                   Adults_av_s_2010_FR_v  > 10 ~ "c) > 10",
                                   Adults_av_s_2010_FR_v  >= 1 ~ "d) > 1",
                                   Adults_av_s_2010_FR_v  < 1 ~ "e) < 1")

g_Ad_2010 <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = Adults_av_s_2010_FR_f), color = NA)+
  scale_fill_viridis_d()+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Average Adults per ha between May and September"))+
  guides(fill=guide_legend(title="Adults/ha"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_Ad_2010.png"), g_Ad_2010, units="in", height=8, width= 6, dpi=300)

#plot: Adults 2020 

Adults_av_s_2020_FR_v <- Adults_av_s_2020_v[reg_FR]

Adults_av_s_2020_FR_f <- case_when(Adults_av_s_2020_FR_v  > 10^3 ~ "a) > 10^3",
                                Adults_av_s_2020_FR_v > 10^2 ~ "b) > 10^2",
                                Adults_av_s_2020_FR_v  > 10 ~ "c) > 10",
                                Adults_av_s_2020_FR_v  >= 1 ~ "d) > 1",
                                Adults_av_s_2020_FR_v  < 1 ~ "e) < 1")

g_Ad_2020 <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = Adults_av_s_2020_FR_f), color = NA)+
  scale_fill_viridis_d()+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Average Adults per ha between May and September"))+
  guides(fill=guide_legend(title="Adults/ha"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_Ad_2020.png"), g_Ad_2020, units="in", height=8, width= 6, dpi=300)

#plot: varAdults 2020 

#palette_pos = c("#8EB0FE", "gray90", "#F29878", "#D04B45", "#B00026")
palette_pos2 = c( "gray90", "#F2CDBB", "#F29878", "#D04B45", "#B00026")

Adults_av_s_diff_FR_v <- Adults_av_s_diff_v[reg_FR]
Adults_av_s_diff_FR_v[which(Adults_av_s_2020_FR_v<1)] = NA

Adults_av_s_diff_FR_f <- case_when(Adults_av_s_diff_FR_v > 200 ~"a) > +200",
                                   Adults_av_s_diff_FR_v > 80 ~"b) +80 to +200",
                                   Adults_av_s_diff_FR_v > 30 ~"c) +30 to +80",
                                   Adults_av_s_diff_FR_v > 10 ~"d) +10 to +30 ",
                                   Adults_av_s_diff_FR_v > -10 ~"e) -10 to +10",
                                   .default = NA)

g_Ad_change <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = Adults_av_s_diff_FR_f), color = NA)+
  scale_fill_manual(values = rev(palette_pos2), na.value = "transparent")+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Difference in Adults density in summer"))+
  guides(fill=guide_legend(title="Variation"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_Ad_change.png"), g_Ad_change, units="in", height=8, width= 6, dpi=300)

#plot: LE 2010  (ok)

LE_le_s_2010_FR_v <- LE_le_s_2010_v[reg_FR]

x = c("a) > 14 w", "b) < 14 w", "c) < 7 w", "d) < 3 w", "e) 0 w")

LE_le_s_2010_FR_f <- case_when(LE_le_s_2010_FR_v > 98 ~ x[1],
                            LE_le_s_2010_FR_v > 49 ~ x[2],
                            LE_le_s_2010_FR_v > 21 ~ x[3],
                            LE_le_s_2010_FR_v > 0 ~ x[4],
                            LE_le_s_2010_FR_v == 0 ~ x[5])

g_Le_2010 <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = LE_le_s_2010_FR_f), color = NA)+
  scale_fill_viridis_d()+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Lenght of the season based on laid eggs, threshold = ", thr_LE_RM, " laid eggs/(ha*d)"))+
  guides(fill=guide_legend(title="L season"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_Le_2010.png"), g_Le_2010, units="in", height=8, width= 6, dpi=300)

#plot: LE 2020  (ok)

LE_le_s_2020_FR_v <- LE_le_s_2020_v[reg_FR]

LE_le_s_2020_FR_f <- case_when(LE_le_s_2020_FR_v > 98 ~ x[1],
                               LE_le_s_2020_FR_v > 49 ~ x[2],
                               LE_le_s_2020_FR_v > 21 ~ x[3],
                               LE_le_s_2020_FR_v > 0 ~ x[4],
                               LE_le_s_2020_FR_v == 0 ~ x[5])

g_Le_2020 <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = LE_le_s_2020_FR_f), color = NA)+
  scale_fill_viridis_d()+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Lenght of the season based on laid eggs, threshold = ", thr_LE_RM, " laid eggs/(ha*d)"))+
  guides(fill=guide_legend(title="L season"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_Le_2020.png"), g_Le_2020, units="in", height=8, width= 6, dpi=300)


#plot: var LE 2020 

palette_pos2 = c( "gray90", "#F2CDBB", "#F29878", "#D04B45", "#B00026")

LE_le_s_diff_FR_v <- LE_le_s_diff_v[reg_FR]

LE_le_s_diff_FR_v[which(LE_le_s_2020_f[reg_FR]=="e) 0 w")] <- NA

LE_le_s_diff_FR_f <- case_when(LE_le_s_diff_FR_v > 7*7 ~"a) > 7 w",
                            LE_le_s_diff_FR_v >  7*3 ~"b) > 3 w",
                            LE_le_s_diff_FR_v > 7 ~"c) > 1 w ",
                            LE_le_s_diff_FR_v >= 1  ~"d) > 1 d ",
                            LE_le_s_diff_FR_v > -1 ~"e) stable ",
                            .default = NA)

g_Le_change <- ggplot()+
  geom_sf(data = domain_FR, aes(fill = LE_le_s_diff_FR_f), color = NA)+
  scale_fill_manual(values = rev(palette_pos2), na.value = "transparent")+
  geom_sf(data = regions_sh, alpha = 0, colour = "gray70")+
  geom_sf(data = points_sf)+
  ggtitle(paste0("Difference in season lenght"))+
  guides(fill=guide_legend(title="Increase"))+
  theme(panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.1, size = 4)

ggsave(paste0(folder_plot, "g_Le_change.png"), g_Le_change, units="in", height=8, width= 6, dpi=300)

# +2.3 giorni di attività all'anno, contro una media in europa occidentale di +1.9