# file to read data in "Donnees_albo_2008-2023_Nice"

rm(list = ls())

# library(readxl)
library(xlsx)
library(dplyr)

folder_data = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EID_Nice"

name_file = "Donnees_albo_2008-2023_Nice.xls"

#####
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
                        id_piege = rep(data_reg$id_piege, n_dates),
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

#####
####  2009


data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2009",
                        startRow = 13,
                        endRow = 62,
                        colIndex = 2:35,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- data_read[,1:7]
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-7)/3

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,8+3*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,9+3*(i-1)]
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

data_2009 = data

#####
#2010

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2010 -incomplet",
                        startRow = 3,
                        endRow = 52,
                        colIndex = 1:32,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("06", data_read[,1:6])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = 7

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

# first data is different
i = 1
date_detection = as.Date(as.numeric(data_read[,7]), origin = "1899-12-30")
data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection

eggs_brut <- data_read[,8]
eggs_brut_elab<- case_when(eggs_brut == "0" ~ NA,
                           eggs_brut == "N" ~ 0,
                           eggs_brut == "AP" ~ 0,
                           eggs_brut == "NR" ~ 0,
                           .default = as.numeric(eggs_brut))

data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)

date_pose = date_detection


for(i in 2:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,5+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,6+4*(i-1)]
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

data_2010 = data

#####
####  2011


data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2011",
                        startRow = 11,
                        endRow = 60,
                        colIndex = 3:56,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("06", data_read[,1:6])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-6)/3

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,7+3*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,8+3*(i-1)]
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

data_2011 = data

#####
####  2012

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2012",
                        startRow = 11,
                        endRow = 60,
                        colIndex = 3:67,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("06", data_read[,1:6])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = 15

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose


# as in 2010: the first is different from the others

i = 1

date_detection = as.Date(as.numeric(data_read[,7+3*(i-1)]), origin = "1899-12-30")
data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection

eggs_brut <- data_read[,8+3*(i-1)]
eggs_brut_elab<- case_when(eggs_brut == "0" ~ NA,
                           eggs_brut == "N" ~ 0,
                           eggs_brut == "AP" ~ 0,
                           eggs_brut == "NR" ~ 0,
                           .default = as.numeric(eggs_brut))

data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)

date_pose = date_detection

for(i in 2:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,6+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,7+4*(i-1)]
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

data_2012 = data

#####
# 2013

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2013",
                        startRow = 11,
                        endRow = 60,
                        colIndex = 3:91,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("06", data_read[,1:6])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = 21

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose


# as in 2010: the first is different from the others

i = 1

date_detection = as.Date(as.numeric(data_read[,7+3*(i-1)]), origin = "1899-12-30")
data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection

eggs_brut <- data_read[,8+3*(i-1)]
eggs_brut_elab<- case_when(eggs_brut == "0" ~ NA,
                           eggs_brut == "N" ~ 0,
                           eggs_brut == "AP" ~ 0,
                           eggs_brut == "NR" ~ 0,
                           .default = as.numeric(eggs_brut))

data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)

date_pose = date_detection

for(i in 2:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,6+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,7+4*(i-1)]
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

data_2013 = data

#####
# 2014

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2014",
                        startRow = 3,
                        endRow = 52,
                        colIndex = 1:54,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-2)/4

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,3+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  # starting from this year, the legend changes:
  #""" = NA
  # 0 = 0
  
  eggs_brut <- data_read[,5+4*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2014 = data

#####
# 2015

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2015",
                        startRow = 3,
                        endRow = 52,
                        colIndex = 1:62,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-2)/4

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,3+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,5+4*(i-1)]
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

data_2015 = data

#####

# 2016

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2016",
                        startRow = 3,
                        endRow = 52,
                        colIndex = 1:50,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-2)/4

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,3+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,5+4*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2016 = data

#####

# 2017

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2017",
                        startRow = 2,
                        endRow = 51,
                        colIndex = 1:54,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-2)/4

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,3+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,5+4*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2017 = data

#####

# 2018

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2018",
                        startRow = 2,
                        endRow = 51,
                        colIndex = 1:58,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-2)/4

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,3+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,5+4*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2018 = data

#####

# 2019

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2019",
                        startRow = 2,
                        endRow = 51,
                        colIndex = 1:66,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-2)/4

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,3+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,5+4*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2019 = data

#####

# 2020

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2020",
                        startRow = 2,
                        endRow = 51,
                        colIndex = 2:38,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")

# date_first_pose missing: we assume "date_first_pose - 14"
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")-14

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-1)/3

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,2+3*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,4+3*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2020 = data

#####

# 2021

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2021",
                        startRow = 2,
                        endRow = 54,
                        colIndex = 2:59,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")

# date_first_pose missing, again: we assume "date_first_pose - 14"
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")-14

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-1)/3

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,2+3*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,4+3*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2021 = data

#####

# 2022

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2022",
                        startRow = 2,
                        endRow = 51,
                        colIndex = 2:53,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")

# date_first_pose missing, again: we assume "date_first_pose - 14"
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")-14

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-1)/3

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,2+3*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,4+3*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2022 = data

#####

# 2023

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2023",
                        startRow = 2,
                        endRow = 51,
                        colIndex = 2:71,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- c("06", "Nice", NA, NA, NA, data_read[,1:2])
names(data_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")

# date_first_pose is indicated to be 1/03
data_reg$date_first_pose = as.Date("2023-03-01")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-1)/3

#create a new database

data <- data.frame(commune = rep(data_reg$commune, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   lat = rep(data_reg$lat, n_dates),
                   lon = rep(data_reg$lon, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,2+3*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,4+3*(i-1)]
  eggs_brut_elab<- case_when(eggs_brut == "" ~ NA,
                             .default = as.numeric(eggs_brut))
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_2023 = data

#####

library(ggplot2)

# Aggregate everything

data_all <- rbind(data_2008,
                  data_2009,
                  data_2010,
                  data_2011,
                  data_2012,
                  data_2013,
                  data_2014,
                  data_2015,
                  data_2016,
                  data_2017,
                  data_2018,
                  data_2019,
                  data_2020,
                  data_2021,
                  data_2022,
                  data_2023)

data_all_summ <- data_all %>%
  group_by(date_detection, commune)%>%
  dplyr::summarise(av_eggs_per_day = mean(eggs_per_day, na.rm = T),
                   me_eggs_per_day = median(eggs_per_day, na.rm = T)) %>% 
  ungroup()

ggplot(data_all_summ )+
  geom_point(aes(x = date_detection, y = me_eggs_per_day))
