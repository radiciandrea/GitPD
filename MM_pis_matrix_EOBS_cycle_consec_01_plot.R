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

# name = "W_EU"
# year = 2005

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
# 
# #########################
# #plot pop
# Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
#                       "region" = rep(rep(regions, each = max(DOS_sim)), n_c),
#                       "t" = rep(DOS_sim, n_r*n_c),
#                       "value" = c(Sim_tot[, 2:(1+n_c*n_r)])) #5 classes
# 
# beta_approx_m_df = data.frame("variable" = "beta",
#                               "region" = rep(regions, each = max(DOS_sim)),
#                               "t" = rep(DOS_sim, n_r),
#                               "value" = c(beta_approx_tot)) #5 classes
# 
# 
# 
# 
# #extract adults and eggs
# index = 1+(1+3*n_r):(4*n_r)
# 
# Adult_m = Sim_tot[, index]
# LE_m = Adult_m*beta_approx_tot
# 
# save(Adult_m, LE_m, file = paste0(folder_out, "/LE_Adults.RData"))

load(paste0(folder_out, "/LE_Adults.RData"))

# rolling means over 14 days +correct NAs into 0s

Adult_m[which(is.na(Adult_m))] =0

Adult_RM_m <- t(sapply(1:n_d,
                       function(x){return(colMeans(Adult_m[max(1,(x-7)):min(365,(x+7)),]))}))

LE_m[which(is.na(LE_m))] =0

LE_RM_m <- t(sapply(1:n_d,
                    function(x){return(colMeans(LE_m[max(1,(x-7)):min(365,(x+7)),]))}))

#summer "MJJAS" - mean abundances
mjjas = 31+28+31+30+1:(31+30+31+31+30-1)

# LE_av_ssn_v = sapply(1:n_r, function(x){mean(LE_RM_m[which(LE_st_m[,x]>thr),x])}) #mean density in summer
LE_av_ssn_v = colMeans(LE_RM_m[mjjas,]) #mean density in MJJAS

# Adult_av_ssn_v = sapply(1:n_r, function(x){mean(Adult_RM_m[which(Adult_st_m[,x]>thr),x])}) #mean density during activity
Adult_av_ssn_v = colMeans(Adult_RM_m[mjjas,]) #mean density in MJJAS

# load shp to compare
folder_sh = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/"

domain <- st_read(paste0(folder_sh, "domain_sel_DaRe.shp")) %>%
  arrange(region)

grid <- st_read(paste0(folder_sh, "grid_DaRe+pop.shp"))

countries_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/european-countries.shp")

# abundance in mjjas (EGGS)

LE_av_ssn_f <- case_when(LE_av_ssn_v  > 10^4 ~ "a) > 10.000",
                         LE_av_ssn_v > 10^3 ~ "b) > 1.000",
                         LE_av_ssn_v  > 10^2 ~ "c) > 100",
                         LE_av_ssn_v  > 10 ~ "d) > 10",
                         LE_av_ssn_v  < 10 ~ "e) < 10")

LE_av_ssn_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_av_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Average daily laid eggs per ha between May and September"))+
  guides(fill=guide_legend(title="eggs/ha"))+
  theme_minimal()

ggsave("LE_av_ssn_gg.png", LE_av_ssn_gg, units="in", width=7, height=5.5, dpi=300)

# abundance in mjjas (Adults)

Adult_av_ssn_f <- case_when(Adult_av_ssn_v  > 10^3 ~ "a) > 1000",
                            Adult_av_ssn_v > 10^2 ~ "b) > 100",
                            Adult_av_ssn_v  > 10 ~ "c) > 10",
                            Adult_av_ssn_v  > 1 ~ "d) > 1",
                            Adult_av_ssn_v  < 1 ~ "e) < 1")

Adult_av_ssn <- ggplot()+
  geom_sf(data = domain, aes(fill = Adult_av_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Average adult per ha between May and September"))+
  guides(fill=guide_legend(title="adult/ha"))+
  theme_minimal()

ggsave("Adult_av_ssn.png", Adult_av_ssn, units="in", width=7, height=5.5, dpi=300)

#  standardyze dynamics over 2 weeks
thr = 0.05

max_Adult_RM = max(Adult_RM_m)
thr_Adult_RM = thr*max_Adult_RM

Adult_st_m = Adult_RM_m/max_Adult_RM #normalizd
Adult_ssn_v = colSums(Adult_st_m>thr) #Adultnght
Adult_b_ssn_v = as.Date(apply(Adult_st_m, 2, function(x){which(x>thr)[1]}), origin = "2022-12-31") #beginning
Adult_e_ssn_v = as.Date(apply(Adult_st_m, 2, function(x){max(which(x>thr))}), origin = "2022-12-31") #end
Adult_e_ssn_v[which(Adult_e_ssn_v<0)] = NA

max_LE_RM = max(LE_RM_m)
thr_LE_RM = thr*max_LE_RM

LE_st_m = LE_RM_m/max_LE_RM #normalized
LE_ssn_v = colSums(LE_st_m>thr) #lenght
LE_b_ssn_v = as.Date(apply(LE_st_m, 2, function(x){which(x>thr)[1]}), origin = "2022-12-31") #beginning
LE_e_ssn_v = as.Date(apply(LE_st_m, 2, function(x){max(which(x>thr))}), origin = "2022-12-31") #end
LE_e_ssn_v[which(LE_e_ssn_v<0)] = NA

# plots of interests 
palette_x <- c("#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724")

# length of the season (EGGS)

x = c("a) > 12 w", "b) < 12 w", "c) < 7 w", "d) < 3 w", "e) - 0 w")

LE_ssn_f <- case_when(LE_ssn_v > 84 ~ x[1],
                      LE_ssn_v > 49 ~ x[2],
                      LE_ssn_v > 21 ~ x[3],
                      LE_ssn_v > 0 ~ x[4],
                      LE_ssn_v == 0 ~ x[5])

LE_ssn_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Lenght of the season based on laid eggs, threshold = ", thr,
                 ",\nequivalent to ", round(thr_LE_RM), " daily laid eggs per ha"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave("LE_ssn_gg.png", LE_ssn_gg, units="in", width=7, height=5.5, dpi=300)

# length of the season (Adults)

Adult_ssn_f <- case_when(Adult_ssn_v > 84 ~ x[1],
                         Adult_ssn_v > 49 ~ x[2],
                         Adult_ssn_v > 21 ~ x[3],
                         Adult_ssn_v > 0 ~ x[4],
                         Adult_ssn_v == 0 ~ x[5])

Adult_ssn_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = Adult_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("Lenght of the season based on adult density, threshold = ", thr,
                 ",\nequivalent to ", round(thr_Adult_RM), " adults per ha"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave("Adult_ssn_gg.png", Adult_ssn_gg,  units="in", width=7, height=5.5, dpi=300)

# beginning of the season (EGGS)
x <- c("a) April or earlier", "b) May", "c) June","d) July", "e) August or later")

LE_b_ssn_f <- case_when(LE_b_ssn_v < "2023-05-01"~ x[1],
                        LE_b_ssn_v < "2023-06-01"~ x[2],
                        LE_b_ssn_v < "2023-07-01"~ x[3],
                        LE_b_ssn_v < "2023-08-01"~ x[4],
                        LE_b_ssn_v >= "2023-08-01"~ x[5])

LE_b_ssn_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_b_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_manual(breaks = x, values = palette_x)+
  ggtitle(paste0("Beginning of the season based on laid eggs, threshold = ", thr,
                 ",\nequivalent to ", round(thr_LE_RM), " daily laid eggs per ha"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave("LE_b_ssn_gg.png", LE_b_ssn_gg, units="in", width=7, height=5.5, dpi=300)

# beginning of the season (Adults)

Adult_b_ssn_f <- case_when(Adult_b_ssn_v < "2023-05-01"~ x[1],
                           Adult_b_ssn_v < "2023-06-01"~ x[2],
                           Adult_b_ssn_v < "2023-07-01"~ x[3],
                           Adult_b_ssn_v < "2023-08-01"~ x[4],
                           Adult_b_ssn_v >= "2023-08-01"~ x[5])

Adult_b_ssn_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = Adult_b_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_manual(breaks = x, values = palette_x)+
  ggtitle(paste0("Beginning of the season based on adult density, threshold = ", thr,
                 ",\nequivalent to ", round(thr_Adult_RM), " adults per ha"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave("Adult_b_ssn_gg.png", Adult_b_ssn_gg, units="in", width=7, height=5.5, dpi=300)

# end of the season (EGGS)

x <- c("a) August or earlier", "b) September", "c) October", "d) November", "e) December")

LE_e_ssn_f <- case_when(LE_e_ssn_v < "2023-09-01"~ x[1],
                        LE_e_ssn_v < "2023-10-01"~ x[2],
                        LE_e_ssn_v < "2023-11-01"~ x[3],
                        LE_e_ssn_v < "2023-12-01"~ x[4],
                        LE_e_ssn_v >= "2023-12-01"~ x[5])

LE_e_ssn_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_e_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("End of the season based on laid eggs, threshold = ", thr,
                 ",\nequivalent to ", round(thr_LE_RM), " daily laid eggs per ha"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave("LE_e_ssn_gg.png", LE_e_ssn_gg, units="in", width=7, height=5.5, dpi=300)

# end of the season (Adults)

Adult_e_ssn_f <- case_when(Adult_e_ssn_v < "2023-09-01"~ x[1],
                           Adult_e_ssn_v < "2023-10-01"~ x[2],
                           Adult_e_ssn_v < "2023-11-01"~ x[3],
                           Adult_e_ssn_v < "2023-12-01"~ x[4],
                           Adult_e_ssn_v >= "2023-12-01"~ x[5])

Adult_e_ssn_gg <- ggplot()+
  geom_sf(data = domain, aes(fill = Adult_e_ssn_f), color = NA)+
  geom_sf(data = countries_sh, alpha = 0, colour = "gray30")+
  coord_sf(xlim = c(1.5, 19.5), ylim = c(40.5, 48.5)) +
  scale_fill_viridis_d()+
  ggtitle(paste0("End of the season based on adult density, threshold = ", thr,
                 ",\nequivalent to ", round(thr_Adult_RM), " adults per ha"))+
  guides(fill=guide_legend(title="L season"))+
  theme_minimal()

ggsave("Adult_e_ssn_gg.png", Adult_e_ssn_gg, units="in", width=7, height=5.5, dpi=300)

# 
# ggplot()+
#   geom_sf(data = domain, aes(fill = Adult_ssn), color = NA)
# sd(Adult_ssn_v)/mean(Adult_ssn_v)
# 
# ggplot()+
#   geom_sf(data = domain, aes(fill = LE_ssn), color = NA)
# sd(LE_ssn_v)/mean(LE_ssn_v)

# I follow code temp_ncdf

library(ncdf4)

# geo_file

grid_centr = st_centroid(grid)

grid_centr <- grid_centr %>%
  mutate(lats = round(st_coordinates(grid_centr)[,2],1))%>%
  mutate(lons = round(st_coordinates(grid_centr)[,1],1))%>%
  select(c("FID", "lons", "lats"))%>%
  st_drop_geometry()

FIDs = grid_centr$FID
n_FIDs = length(FIDs)

# time is DOS_sim

#arrange adult matrices over FID 

#create "fake" matric of adults with NAs
Adult_m_FID = matrix(NA, nrow = 365, ncol = n_FIDs)
fid_index = domain %>% arrange(region) %>% pull(FID)
Adult_m_FID[,fid_index] = Adult_m

Adult_df = data.frame(FID = rep(FIDs, each = n_d),
                      time = rep(DOS_sim, times = n_FIDs),
                      values = c(Adult_m_FID))

# Adults_dfd <- dcast(Adults_df, time ~ FID)
# 
# Adults_dfdm = data.matrix(Adults_dfd[, 1+1:n_FIDs])

# the same with LE
#create "fake" matric of adults with NAs
LE_m_FID = matrix(NA, nrow = 365, ncol = n_FIDs)
LE_m_FID[,fid_index] = LE_m

LE_df = data.frame(FID = rep(FIDs, each = n_d),
                   time = rep(DOS_sim, times = n_FIDs),
                   values = c(LE_m_FID))


#join and sort in preferred order

Adult_j_df = left_join(Adult_df, grid_centr) %>%
  select(-c("FID")) %>%
  arrange(lons)%>%
  arrange(lats)%>%
  arrange(time)

LE_j_df = left_join(LE_df, grid_centr) %>%
  select(-c("FID")) %>%
  arrange(lons)%>%
  arrange(lats)%>%
  arrange(time)

# convert into array

tic()
array_Adult <- xtabs(values ~ lons + lats + time, data = Adult_j_df)
array_LE <- xtabs(values ~ lons + lats + time, data = LE_j_df)
toc()

#https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-netcdf-file

lon <- unique(LE_j_df$lons)
lat <- unique(LE_j_df$lats)
time <- unique(LE_j_df$time)

nlon<-length(lon)
nlat<-length(lat)
nt<-length(time)

# and chatchpt

# Define the dimensions in the NetCDF file
dim_lat <- ncdim_def(name = "latitude", units = "degrees_north", vals = lat)
dim_lon <- ncdim_def(name = "longitude", units = "degrees_east", vals = lon)
dim_timedummy <- ncdim_def(name = "time", units = "time", vals = time)

# Define the variable to store in the NetCDF file
var_value <- ncvar_def(name = "value", units = "units", 
                       dim = list(dim_lon, dim_lat, dim_timedummy), 
                       missval = NA)

# Create the NetCDF file - write - and close to save it
nc_LE <- nc_create("LE.nc", var_value)
ncvar_put(nc_LE, var_value, array_LE)
nc_close(nc_LE)

nc_Adult <- nc_create("Adult.nc", var_value)
ncvar_put(nc_Adult, var_value, array_Adult)
nc_close(nc_Adult)