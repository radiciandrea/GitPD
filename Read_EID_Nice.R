# file to read data in "Donnees_albo_2008-2023_Nice"

rm(list = ls())

# library(readxl)
library(xlsx)
library(dplyr)

folder_data = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EID_Nice"

name_file = "Donnees_albo_2008-2023_Nice.xls"

# 2008

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2008",
                        startRow = 13,
                        endRow = 42,
                        colIndex = 2:76,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- data_read[,1:7]
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-7)/4

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                        id_piede = rep(data_reg$id_piege, n_dates),
                        lat = rep(data_reg$lat, n_dates),
                        lon = rep(data_reg$lon, n_dates),
                        date_detection = as.Date(rep(NA, n_sites*n_dates)),
                        observed_eggs = (rep(NA, n_sites*n_dates)),
                        trapping_days = (rep(NA, n_sites*n_dates)),
                        eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,8+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  #should elaborate this:
  # N means "negatif" = 0; 
  # 0 means "absence donnée (piège ou polystyrène manquant"
  # AP means "piège anciennement positifs"
  # NR means "non elevé
  
  # RAS = ok
  # PD = piège disparu
  # SPA = seau pas accessible
  # PP = polystyrène perdu
  # S = seau sec
  # R = seau renversé
  
  eggs_brut <- data_read[,9+4*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "0" ~ NA,
                        eggs_brut == "N" ~ 0,
                        eggs_brut == "AP" ~ 0,
                        eggs_brut == "NR" ~ 0,
                        .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2008 = data
