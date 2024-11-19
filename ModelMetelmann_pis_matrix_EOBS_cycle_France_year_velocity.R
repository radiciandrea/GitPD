# carico gli stessi input di ModelMetelmann_pis_matrix_EOBS_cycle_StatTest_France_year

# per stimare la velocità di colonizzazione seconoo i riferimenti:
# Tisseuil, 2016; Kraemer, 2019

library(sf)
library(dplyr)
library(ggplot2)

# load presence data

folder_CANNET = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DGS_Cannet"
data_presence <- read.csv2(paste0(folder_CANNET, "/IRD communes années mod.csv"))

# simplidfied shp (may be too much) with QGIS
folder_COMM = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/communes-20220101-shp"
shp_communes <- st_read(paste0(folder_COMM, "/communes-20220101_metr_simp.shp"))

data_presence_sf <- left_join(shp_communes, data_presence, by = c("insee" = "Code.Insee")) %>%
  rename(presence = Nb.Présence.vecteur.Oui) %>%
  rename(year_col = Année.colonisation.commune) %>%
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

#use 'fields' to estimate velocity
#https://www.image.ucar.edu/GSP/Software/Fields/Help/Tps.html
library(fields)

tps_model <- Tps(x = st_coordinates(data_presence_sf_cp), Y = data_presence_sf_cp$year_col)

# load grid and other datas
grid_W_EU <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/grid_eobs_01_W_EU.shp")

grid_W_EU_c <- st_centroid(grid_W_EU)
grid_W_EU_cp <- st_transform(grid_W_EU_c, crs = "EPSG:3035")

# predict over a grid
grid_W_EU_cp$year_pred = predict(tps_model, st_coordinates(grid_W_EU_cp))

# make a matrix out of it
grid_W_EU_m_year = matrix(grid_W_EU_cp$year_pred, nrow = 240, byrow = F) #88
grid_W_EU_m_x = matrix(st_coordinates(grid_W_EU_cp)[,1], nrow = 240, byrow = F) #88
grid_W_EU_m_y = matrix(st_coordinates(grid_W_EU_cp)[,2], nrow = 240, byrow = F) #88

grid_W_EU_v_x = grid_W_EU_m_x[1,]
grid_W_EU_v_y = grid_W_EU_m_y[,1]

#if projected
dx = mean(grid_W_EU_m_x[,2:291]-grid_W_EU_m_x[,1:290]) # 1:131
dy = mean(grid_W_EU_m_y[1:239,]-grid_W_EU_m_y[2:240,]) # #1:88

# compute velocity
library(pracma)

gradient_xy <- gradient(grid_W_EU_m_year, dx, dy)
gradient_x = gradient_xy[[1]]
gradient_y = gradient_xy[[2]]

# velocity (is friction in reality velocity)
friction <- sqrt(gradient_x^2 + gradient_y^2)

#considerr a 11*11 windows: Kraemer
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

ggplot(grid_W_EU_cp, aes(color = velocity)) +
  geom_sf() +
  scale_color_viridis_c() + 
  labs(title = "Spread Velocity", x = "X Coordinate", y = "Y Coordinate", color = "Velocity") +
  theme_minimal()

# LATER
type = "_01"

folder_out = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim", type)

files = list.files(folder_out)

name = substring(files[1], nchar(type)+10, nchar(type)+13)
years = substring(files, nchar(type)+15, nchar(type)+18) #CORREGGGERE QUI

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) %>%
  arrange(region) %>%
  filter(!(is.na(Country))) 

domain_sel_v <- left_join(domain_sel, st_drop_geometry(grid_W_EU_cp))

ggplot(domain_sel_v, aes(fill = velocity), color = NULL) +
  geom_sf() +
  scale_fill_viridis_c(na.value = "transparent") + 
  labs(title = "Spread Velocity", fill = "Velocity") +
  theme_minimal()


#### alternative code

bb_france <- st_bbox(st_transform(domain_sel, crs = "EPSG:3035"))# km

# bb_france <- st_bbox(data_presence_sf_cp) # alternatively

dxy = 5000

x_range <- seq(bb_france[1], bb_france[3], by = dxy)
y_range <- seq(bb_france[2], bb_france[4], by = dxy)
grid <- expand.grid(X = x_range, Y = y_range)

grid$year_pred <- predict(tps_model, grid)

z_matrix <- matrix(grid$year_pred, ncol = length(x_range), nrow = length(y_range), byrow = F)

library(pracma)

gradient_xy <- gradient(z_matrix, x_range, y_range)
gradient_x = gradient_xy[[1]]
gradient_y = gradient_xy[[2]]

# velocity (is friction in reality velocity)
friction <- sqrt(gradient_x^2 + gradient_y^2)

#considerr a 11*11 windows: Kraemer
# the resulting friction surface (time/distance) was smoothed using an average 11 × 11
# cell filter to prevent local null frictions values
friction_smoothed <- friction

for(j in 1:ncol(friction)){
  for(i in 1:nrow(friction)){
    friction_smoothed[i,j] <- mean(friction[max(1,(i-5)):min((i+5),nrow(friction))
                                            ,max(1,(j-5)):min((j+5),ncol(friction))])
  }
}

velocity = 1/friction_smoothed*0.001 # (to express as km/y)

grid$velocity = as.vector(t(velocity))

ggplot(grid, aes(x = X, y = Y, fill = velocity), colo = NULL) +
  geom_tile() +
  scale_fill_viridis_c() + 
  labs(title = "Spread Velocity", x = "X Coordinate", y = "Y Coordinate", fill = "Velocity") +
  theme_minimal()
