# file to read data in "Donnees_albo_2008-2023_Nice"

rm(list = ls())

# library(readxl)
library(xlsx)


folder_data = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EID_Nice"

name_file = "Donnees_albo_2008-2023_Nice.xls"

# 2008

data_2008_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "2008",
                        startRow = 13,
                        endRow = 42,
                        colIndex = 2:76,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_2008_reg <- data_2008_red[,1:7]
data_2008_reg$date_first_pose = as.Date(as.numeric(data_2008_reg$date_first_pose), origin = "1899-12-30")
names(data_2008_reg) = c("dep", "commune", "station", "lat", "lon", "id_piege", "date_first_pose")

#by observing the database
n_sites = ncol(data_2008)
n_dates = (-7)/4

#create a new database

data_2008 <- data.frame(commune = rep(data_2008_reg$commune, n_dates),
                        id_piede = rep(data_2008_reg$id_piege, n_dates),
                        lat = rep(data_2008_reg$lat, n_dates),
                        lon = rep(data_2008_reg$lon, n_dates),
                        date_detection = NA,
                        observed_eggs = NA,
                        trapping_days = NA,
                        eggs_per_day= NA)

