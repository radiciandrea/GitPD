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
grid_France <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/grid_eobs_01_France.shp")

grid_France_c <- st_centroid(grid_France)
grid_France_cp <- st_transform(grid_France_c, crs = "EPSG:3035")

# predict over a grid
grid_France_cp$year_pred = predict(tps_model, st_coordinates(grid_France_cp))

# make a matrix out of it
grid_France_m_year = matrix(grid_France_cp$year_pred, nrow = 88, byrow = F)
grid_France_m_x = matrix(st_coordinates(grid_France_cp)[,1], nrow = 88, byrow = F)
grid_France_m_y = matrix(st_coordinates(grid_France_cp)[,2], nrow = 88, byrow = F)

grid_France_v_x = grid_France_m_x[1,]
grid_France_v_y = grid_France_m_y[,1]

# compute velocity
library(pracma)

gradient_xy <- gradient(grid_France_m_year, grid_France_v_x, grid_France_v_y)
gradient_x = gradient_xy[[1]]
gradient_y = gradient_xy[[2]]

velocity <- sqrt(gradient_x^2 + gradient_y^2)

grid_France_cp$velocity = as.vector(velocity)

ggplot(grid_France_cp, aes(color = velocity)) +
  geom_sf() +
  scale_fill_viridis_c() + 
  labs(title = "Spread Velocity", x = "X Coordinate", y = "Y Coordinate", fill = "Velocity") +
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

domain_sel_v <- left_join(domain_sel, st_drop_geometry(grid_France_cp))

ggplot(domain_sel_v, aes(fill = velocity), color = NULL) +
  geom_sf() +
  scale_fill_viridis_c() + 
  labs(title = "Spread Velocity", fill = "Velocity") +
  theme_minimal()
