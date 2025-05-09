# carico gli stessi input di ModelMetelmann_pis_matrix_EOBS_cycle_StatTest_France_year

# per stimare la velocità di colonizzazione seconoo i riferimenti:
# Tisseuil, 2016; Kraemer, 2019

library(sf)
library(dplyr)
library(ggplot2)
library(xlsx)
library(pracma)

#### PRESENCE DATA ----
# load presence data

folder_CANNET = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DGS_Cannet"

# integrate presence data with 2024
data_presence_mid2024 <- read.csv2(paste0(folder_CANNET, "/IRD communes années mod.csv")) # lacks of 2024
data_presence_2024 <- read.xlsx(paste0(folder_CANNET, "/IRD communes années 2024.xlsx"), sheetName = "2024") # lacks of code insee

# clean data_presence_mid2024 from 2024 data and add colmn "name_common_dep"
data_presence_pre2024 <- data_presence_mid2024 %>%
  select(-c("Nb.Présence.vecteur.Oui")) %>%
  mutate(Année.colonisation.commune = case_when(Année.colonisation.commune == "2024" ~ NA,
                                                .default = Année.colonisation.commune)) %>%
  rename(Année.colonisation.commune_before2024 = Année.colonisation.commune) %>%
  mutate(com_dep = paste0(Communes, "_", substring(Code.Insee, 1, 2))) 

# same to 2024
data_presence_2024<- data_presence_2024 %>%
  mutate(com_dep = paste0(Communes, "_", Code.Département))%>%
  rename(Année.colonisation.commune_2024 = Année.colonisation.commune) %>%
  select(c("com_dep", "Année.colonisation.commune_2024"))

# join to obtain global db
data_presence <- left_join(data_presence_pre2024, data_presence_2024, by = "com_dep") %>%
  mutate(Année.colonisation.commune = pmin(Année.colonisation.commune_before2024, Année.colonisation.commune_2024, na.rm = T)) %>%
  select(c("Communes", "Code.Insee", "Année.colonisation.commune"))

# simplidfied shp (may be too much) with QGIS
folder_COMM = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/communes-20220101-shp"
shp_communes <- st_read(paste0(folder_COMM, "/communes-20220101_metr_simp.shp"))

data_presence_sf <- left_join(shp_communes, data_presence, by = c("insee" = "Code.Insee")) %>%
  rename(year_col = Année.colonisation.commune) %>%
  mutate(presence = !is.na(year_col)) %>%
  select(c("insee","presence", "year_col", "geometry"))

ggplot()+
  geom_sf(data = data_presence_sf, aes(fill = year_col), color = NA)

# extract centroids - only of observed values?

data_presence_sf_sel <-data_presence_sf%>%
  filter(presence == 1)

data_presence_sf_c <- st_centroid(data_presence_sf_sel)

# project them to EPSG 3035
#https://epsg.io/3035

data_presence_sf_cp <- st_transform(data_presence_sf_c, crs = "EPSG:3035") #meters

### Fitting and prediciton ----

#use 'fields' to estimate velocity
#https://www.image.ucar.edu/GSP/Software/Fields/Help/Tps.html
library(fields)

# very long: 285 secs
tic()
tps_model <- Tps(x = st_coordinates(data_presence_sf_cp), Y = data_presence_sf_cp$year_col)
toc()

# load grid and other datas
grid_W_EU <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/grid_eobs_01_W_EU.shp")

grid_W_EU_c <- st_centroid(grid_W_EU)
grid_W_EU_cp <- st_transform(grid_W_EU_c, crs = "EPSG:3035")

# predict over a grid
grid_W_EU_cp$year_pred = predict(tps_model, st_coordinates(grid_W_EU_cp))

### Gradient computation and smoothing ----

# make a matrix out of it

# #if projected
# dx = mean(grid_W_EU_m_x[,2:291]-grid_W_EU_m_x[,1:290]) # 1:131
# dy = mean(grid_W_EU_m_y[1:239,]-grid_W_EU_m_y[2:240,]) # #1:88
# 
# # compute velocity
# library(pracma)
# 
# gradient_xy <- gradient(grid_W_EU_m_year, dx, dy)
# gradient_x = gradient_xy[[1]]
# gradient_y = gradient_xy[[2]]

x = st_coordinates(grid_W_EU_cp)[,1]
y = st_coordinates(grid_W_EU_cp)[,2]
z = grid_W_EU_cp$year_pred

x_m = matrix(x, nrow = 240, byrow = F)
y_m = matrix(y, nrow = 240, byrow = F)
z_m = matrix(z, nrow = 240, byrow = F)

#approximate gradient

#g = (f(i+1)-f(i-1))/(x(i+1)-x(i-1))
# actually this is consistent with Kraemer 2019 ("we took a 3*3 windows")

g_x = matrix(NA, nrow = 240, ncol = 291)
g_y = matrix(NA, nrow = 240, ncol = 291)

for(i in 2:239){
  for(j in 2:290){
    g_x[i,j] = (z_m[i,j+1]-z_m[i,j-1])/(x_m[i,j+1]-x_m[i,j-1])
    g_y[i,j] = (z_m[i+1,j]-z_m[i-1,j])/(y_m[i+1,j]-y_m[i-1,j])
  }
}

# velocity (is friction in reality velocity^-1)
friction <- sqrt(g_x^2 + g_y^2)

#considerr a 11*11 windows: Kraemer 2019
# the resulting friction surface (time/distance) was smoothed using an average 11 × 11
# cell filter to prevent local null frictions values
friction_smoothed <- friction

d_smooth = 5

for(j in 1:ncol(friction)){
  for(i in 1:nrow(friction)){
    friction_smoothed[i,j] <- mean(friction[max(1,(i-d_smooth)):min((i+d_smooth),nrow(friction))
                                                     ,max(1,(j-d_smooth)):min((j+d_smooth),ncol(friction))])
  }
}

velocity = 1/friction_smoothed*0.001 # (to express as km/y)

grid_W_EU_cp$velocity = as.vector(velocity)

# #Europe
# ggplot(grid_W_EU_cp, aes(color = velocity)) +
#   geom_sf() +
#   scale_color_viridis_c() + 
#   labs(title = "Spread Velocity", x = "X Coordinate", y = "Y Coordinate", color = "Velocity") +
#   theme_minimal()

### Mapping in France ----
type = "_01"

folder_out = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim", type)

files = list.files(folder_out)

name = substring(files[1], nchar(type)+10, nchar(type)+13)
years = substring(files, nchar(type)+15, nchar(type)+18) #CORREGGGERE QUI

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) %>%
  arrange(region) %>%
  filter(!(is.na(Country))) 

domain_sel_v <- left_join(domain_sel, st_drop_geometry(grid_W_EU_cp))

# ggplot(domain_sel_v, aes(fill = velocity), color = NULL) +
#   geom_sf() +
#   scale_fill_viridis_c(na.value = "transparent") +
#   labs(title = "Spread Velocity", fill = "km/y") +
#   theme_minimal()
# 
# ggplot(domain_sel_v, aes(fill = year_pred), color = NULL) +
#   geom_sf() +
#   scale_fill_viridis_c(na.value = "transparent") + 
#   labs(title = "Year of colonization", fill = "year") +
#   theme_minimal()

#### INTERSECT GRID to get presence data at the same resolution

sf::sf_use_s2(FALSE)
data_presence_sf <- sf::st_make_valid(data_presence_sf)

tic()
domain_presence_intersect <- st_intersection(data_presence_sf, domain_sel)
toc()

domain_presence_merged <-domain_presence_intersect %>%
  filter(presence == 1) %>%
  select(c("region", "year_col")) %>%
  st_drop_geometry() %>%
  group_by(region) %>%
  summarise(year_col = min(year_col, na.rm = T))%>%
  ungroup()

domain_sel_v  <- left_join(domain_sel_v, (domain_presence_merged))

#### MODEL SPREAD VELOCITY FROM E0 ---- 

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

years_eval = 2005:2024
delay = 7 #1:8

domain_sel_E0 <- domain_sel
domain_sel_E0$year_col <- NA

#select only E0 in France
E0_m_FR = E0_m[,domain_sel$region]

for (year in years_eval){
  years_sel = max(min(years), (year-delay+1)):min(max(years), (year))
  E0_m_FR_c_sel <- apply(E0_m_FR[which(years %in% years_sel),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
  
  E0_sel = apply(E0_m_FR_c_sel, 2,
                 function(x){exp(mean(log(x)))})
  
  id_reg_E0 = which(E0_sel>1)
  id_reg_noy = which(is.na(domain_sel_E0$year_col))
  id_new_invasions = intersect(id_reg_noy, id_reg_E0)
  # if gretare than 1 AND no values, then year_colonization = year
  
  if(length(id_new_invasions>1)){
    domain_sel_E0$year_col[id_new_invasions] = year
  }
}

#
domain_sel_E0_sel <-domain_sel_E0%>%
  filter(!is.na(year_col))

domain_sel_E0_sel_df <- domain_sel_E0_sel%>%
  st_drop_geometry()%>%
  select(c("region", "year_col"))

domain_sel_E0_sel_c <- st_centroid(domain_sel_E0_sel)

# project them to EPSG 3035
#https://epsg.io/3035

domain_sel_E0_sel_cp <- st_transform(domain_sel_E0_sel_c, crs = "EPSG:3035") #meters

### Fitting and prediciton ----

tps_model_E0 <- Tps(x = st_coordinates(domain_sel_E0_sel_cp), Y = domain_sel_E0_sel_cp$year_col)

# predict over a grid
grid_W_EU_cp$year_pred_E0 = predict(tps_model_E0, st_coordinates(grid_W_EU_cp))

# gradient with more accurated formula (as before: g = (f(i+1)-f(i-1))/(x(i+1)-x(i-1)))
# actually this is consistent with Kraemer 2019 ("we took a 3*3 windows")

### Gradient computation and smoothing ----

# x = st_coordinates(grid_W_EU_cp)[,1]
# y = st_coordinates(grid_W_EU_cp)[,2]
z_E0 = grid_W_EU_cp$year_pred_E0

# x_m = matrix(x, nrow = 240, byrow = F)
# y_m = matrix(y, nrow = 240, byrow = F)
z_E0_m = matrix(z_E0, nrow = 240, byrow = F)

g_x_E0 = matrix(NA, nrow = 240, ncol = 291)
g_y_E0 = matrix(NA, nrow = 240, ncol = 291)

for(i in 2:239){
  for(j in 2:290){
    g_x_E0[i,j] = (z_E0_m[i,j+1]-z_E0_m[i,j-1])/(x_m[i,j+1]-x_m[i,j-1])
    g_y_E0[i,j] = (z_E0_m[i+1,j]-z_E0_m[i-1,j])/(y_m[i+1,j]-y_m[i-1,j])
  }
}

# velocity (is friction in reality velocity^-1)
friction_E0 <- sqrt(g_x_E0^2 + g_y_E0^2)

#considerr a 11*11 windows: Kraemer 2019
# the resulting friction surface (time/distance) was smoothed using an average 11 × 11
# cell filter to prevent local null frictions values
friction_smoothed_E0 <- friction_E0

d_smooth = 5

for(j in 1:ncol(friction_E0)){
  for(i in 1:nrow(friction_E0)){
    friction_smoothed_E0[i,j] <- mean(friction_E0[max(1,(i-d_smooth)):min((i+d_smooth),nrow(friction_E0))
                                            ,max(1,(j-d_smooth)):min((j+d_smooth),ncol(friction_E0))])
  }
}

velocity_E0 = 1/friction_smoothed_E0*0.001 # (to express as km/y)

grid_W_EU_cp$velocity_E0 = as.vector(velocity_E0)

# #Europe
# ggplot(grid_W_EU_cp, aes(color = velocity_E0)) +
#   geom_sf() +
#   scale_color_viridis_c() + 
#   labs(title = "Spread Velocity", x = "X Coordinate", y = "Y Coordinate", color = "Velocity") +
#   theme_minimal()

domain_sel_v_E0 <- left_join(domain_sel, st_drop_geometry(grid_W_EU_cp))

domain_sel_v_E0 <- left_join(domain_sel_v_E0, (domain_sel_E0_sel_df))

# ggplot(domain_sel_v_E0, aes(fill = velocity_E0), color = NULL) +
#   geom_sf() +
#   scale_fill_viridis_c(na.value = "transparent") + 
#   labs(title = "Spread Velocity", fill = "Velocity") +
#   theme_minimal()

### Plots ----

folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Spread France"

ggplot(domain_sel_v, aes(fill = year_col)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "transparent") + 
  labs(title = "Colonization - obs", fill = "Year") +
  theme_minimal()

ggplot(domain_sel_v %>% filter(!is.na(year_col)), aes(fill = velocity)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "transparent") + 
  labs(title = "Spread Velocity - obs", fill = "(km/y)") +
  theme_minimal()

ggplot(domain_sel_v, aes(fill = year_pred)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "transparent") + 
  labs(title = "Pred colonization - obs", fill = "Year") +
  theme_minimal()

#
ggplot(domain_sel_v_E0, aes(fill = year_col)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "transparent") + 
  labs(title = "Colonization - E0", fill = "Year") +
  theme_minimal()

ggplot(domain_sel_v_E0 %>% filter(!is.na(year_col)), aes(fill = velocity_E0)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "transparent") + 
  labs(title = "Spread Velocity - E0", fill = "(km/y)") +
  theme_minimal()

ggplot(domain_sel_v_E0, aes(fill = year_pred_E0)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(na.value = "transparent") + 
  labs(title = "Pred colonization - E0", fill = "Year") +
  theme_minimal()

# plot velocity global
velocity_df <- domain_sel_v %>%
  filter(!is.na(year_col)) %>%
  select(c("year_col", "velocity")) %>%
  mutate(source = "obs")

velocity_E0_df <- domain_sel_v_E0 %>%
  filter(!is.na(year_col)) %>%
  mutate(velocity = velocity_E0) %>%
  select(c("year_col", "velocity")) %>%
  mutate(source = "E0")

#Obs
ggplot(domain_sel_v %>% filter(!is.na(year_col)),
       aes(x = as.factor(year_col), y  = velocity_E0)) +
  ylim(c(0, 40))+
  geom_boxplot(color = "red", outliers = F)+
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.1)+
  labs(title = "Spread velocity by year - obs") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='transparent',color=NA), 
        panel.background = element_rect(fill='transparent'),
        axis.line = element_line(colour = "black"),
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel

# E0
ggplot(domain_sel_v_E0 %>% filter(!is.na(year_col)),
       aes(x = as.factor(year_col), y  = velocity_E0)) +
  geom_boxplot(color = "red", outliers = F)+
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.1)+
  labs(title = "Spread velocity by year - E0") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='transparent',color=NA), 
        panel.background = element_rect(fill='transparent'),
        axis.line = element_line(colour = "black"),
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel


velocity_all_df = rbind(velocity_df, velocity_E0_df )

vel <- ggplot(velocity_all_df,
       aes(x = as.factor(year_col), y  = velocity, color = source)) +
  geom_boxplot(outliers = F)+
  ylim(c(0, 40))+
  scale_x_discrete(labels=c("", "2005", "", "", "", "", "2010", "", "", "", "",
         "2015","", "", "", "", "2020", "", "", "", ""))+
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.1)+
  scale_color_manual(values=c("#B0986CFF", "#009474FF"))+
  labs(title = "Spread velocity by year - obs vs E0") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill='transparent',color=NA), 
        panel.background = element_rect(fill='transparent'),
        axis.line = element_line(colour = "black"),
        legend.position = "none") #transparent legend panel

ggsave(file= paste0(folder_plot, "/Velocity.png"), units="in", width=4, height=3, dpi=300)

# #### alternatives code
# #### 1
# 
# bb_france <- st_bbox(st_transform(domain_sel, crs = "EPSG:3035"))# km
# 
# # bb_france <- st_bbox(data_presence_sf_cp) # alternatively
# 
# dxy = 5000
# 
# x_range <- seq(bb_france[1], bb_france[3], by = dxy)
# y_range <- seq(bb_france[2], bb_france[4], by = dxy)
# grid <- expand.grid(X = x_range, Y = y_range)
# 
# grid$year_pred <- predict(tps_model, grid)
# 
# z_matrix <- matrix(grid$year_pred, ncol = length(x_range), nrow = length(y_range), byrow = F)
# 
# library(pracma)
# 
# gradient_xy <- gradient(z_matrix, x_range, y_range)
# gradient_x = gradient_xy[[1]]
# gradient_y = gradient_xy[[2]]
# 
# # velocity (is friction in reality velocity)
# friction <- sqrt(gradient_x^2 + gradient_y^2)
# 
# #considerr a 11*11 windows: Kraemer
# # the resulting friction surface (time/distance) was smoothed using an average 11 × 11
# # cell filter to prevent local null frictions values
# friction_smoothed <- friction
# 
# for(j in 1:ncol(friction)){
#   for(i in 1:nrow(friction)){
#     friction_smoothed[i,j] <- mean(friction[max(1,(i-5)):min((i+5),nrow(friction))
#                                             ,max(1,(j-5)):min((j+5),ncol(friction))])
#   }
# }
# 
# velocity = 1/friction_smoothed*0.001 # (to express as km/y)
# 
# grid$velocity = as.vector(t(velocity))
# 
# ggplot(grid, aes(x = X, y = Y, fill = velocity), colo = NULL) +
#   geom_tile() +
#   scale_fill_viridis_c() + 
#   labs(title = "Spread Velocity", x = "X Coordinate", y = "Y Coordinate", fill = "Velocity") +
#   theme_minimal()
# 
# #### 2
# compute_gradients <- function(x, y, z) {
#   dx <- base::diff(x)
#   dy <- base::diff(y)
#   
#   # Gradients along x (∂z/∂x)
#   grad_x <- c((z[-1] - z[-length(z)]) / dx, NA)  # Add NA for the last value (boundary)
#   
#   # Gradients along y (∂z/∂y)
#   grad_y <- c((z[-1] - z[-length(z)]) / dy, NA)  # Add NA for the last value (boundary)
#   
#   return(list(grad_x = grad_x, grad_y = grad_y))
# }
# 
# # Assuming grid$year_pred contains the predicted year values
# gradients_cgpt <- compute_gradients(st_coordinates(grid_W_EU_cp)[,1], st_coordinates(grid_W_EU_cp)[,2], 
#                                     grid_W_EU_cp$year_pred) #_E0
# 
# gradient_x = gradients_cgpt[[1]]
# gradient_y = gradients_cgpt[[2]]
# 
# # velocity (is friction in reality velocity)
# friction <- sqrt(gradient_x^2 + gradient_y^2)
# 
# #as matrix
# friction <- matrix(friction, nrow = 240, byrow = F)
# 
# #considerr a 11*11 windows: Kraemer
# # the resulting friction surface (time/distance) was smoothed using an average 11 × 11
# # cell filter to prevent local null frictions values
# friction_smoothed <- friction
# 
# d_smooth = 5
# 
# for(j in 1:ncol(friction)){
#   for(i in 1:nrow(friction)){
#     friction_smoothed[i,j] <- mean(friction[max(1,(i-d_smooth)):min((i+d_smooth),nrow(friction))
#                                             ,max(1,(j-d_smooth)):min((j+d_smooth),ncol(friction))])
#   }
# }
# 
# velocity_cgpt = 1/friction_smoothed*0.001 # (to express as km/y)
# 
# grid_W_EU_cp$velocity_cgpt= c(velocity_cgpt)
# 
# ggplot(grid_W_EU_cp, aes(color = velocity_cgpt)) +
#   geom_sf() +
#   scale_color_viridis_c() + 
#   labs(title = "Spread Velocity", x = "X Coordinate", y = "Y Coordinate", color = "Velocity") +
#   theme_minimal()
# 
# # plot over French grid
# domain_sel_2 <- left_join(domain_sel, st_drop_geometry(grid_W_EU_cp))
# 
# ggplot(domain_sel_2, aes(fill = velocity_cgpt), color = NULL) +
#   geom_sf() +
#   scale_fill_viridis_c(na.value = "transparent") + 
#   labs(title = "Spread Velocity", fill = "km/y") +
#   theme_minimal()
# 
# ggplot(domain_sel_2, aes(fill = year_pred_E0), color = NULL) +
#   geom_sf() +
#   scale_fill_viridis_c(na.value = "transparent") + 
#   labs(title = "Colonization E0", fill = "year") +
#   theme_minimal()
# 
