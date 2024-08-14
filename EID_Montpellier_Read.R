# file to read data in "EID_Montpellier"

rm(list = ls())

# library(readxl)
library(xlsx)
library(dplyr)

folder_data = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EID_Montpellier"

# we're interested in series:
# Chevalerie 2016-(2017) [Castelnau]
# La Volhe  2016-(2017) [Castelnau]
# Vert-bois (VB) 2016-2017 [Montpellier]
# Maurin 2016-2017 [Lattes]
# Volhe & Chevalerie (VC) -(2017) [Castelnau]

#####
# Maurin 2016

name_file = "DataBrut_EID_PP_morin_sept_2016_Andrea.xlsx"

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "Feuil3",
                        startRow = 2,
                        endRow = 24,
                        colIndex = 1:14,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("Maurin", data_read[,c(2, 4)])
names(data_reg) = c("quartier", "id_piege", "date_first_pose")

# we don't know it, we assume 7 days for consistency
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30") - 7

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-2)/4

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,4+4*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,5+4*(i-1)]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_Maurin_2016 = data

#####
# Chevalerie 2016

name_file = "DataBrut_EID_PP_2016_Andrea.xlsx"

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "C - Chevalerie",
                        startRow = 3,
                        endRow = 12,
                        colIndex = 1:154,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- data_read[,c(2, 4, 8)]
names(data_reg) = c("quartier", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-9)/5

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,11+5*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,12+5*(i-1)]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_Chevalerie_2016 = data

#####
# Vert Bois 2016

name_file = "DataBrut_EID_PP_2016_Andrea.xlsx"

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "E - Vert-Bois",
                        startRow = 3,
                        endRow = 12,
                        colIndex = 1:154,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- data_read[,c(2, 4, 8)]
names(data_reg) = c("quartier", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-9)/5

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,11+5*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,12+5*(i-1)]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_VertBois_2016 = data

#####
# La Volhe 2016

name_file = "DataBrut_EID_PP_2016_Andrea.xlsx"

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "V - La Volhe",
                        startRow = 3,
                        endRow = 12,
                        colIndex = 1:154,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- data_read[,c(2, 4, 8)]
names(data_reg) = c("quartier", "id_piege", "date_first_pose")
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")

#by observing the database
n_sites = nrow(data_read)
n_dates = (ncol(data_read)-9)/5

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(data_read[,11+5*(i-1)]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,12+5*(i-1)]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_LaVolhe_2016 = data

#####
# Vert-Bois 2017

# this file is organized with dates as first row

name_file = "DataBrut_EID_PP_2017_Andrea.xlsx"

dates_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                         sheetName = "Vert-Bois",
                         startRow = 1,
                         endRow = 1,
                         colIndex = 2:21,
                         header = F,
                         as.data.frame = T)

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "Vert-Bois",
                        startRow = 3,
                        endRow = 23,
                        colIndex = 1:21,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("Vert-Bois", data_read[,1], dates_read[1])
names(data_reg) = c("quartier", "id_piege", "date_first_pose")

#as specified later: each reading is 7 days
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")-7

#by observing the database
n_sites = nrow(data_read)
n_dates = ncol(dates_read)

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(dates_read[,i]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,1+i]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_VertBois_2017 = data

#####
# LaVolhe 2017

name_file = "DataBrut_EID_PP_2017_Andrea.xlsx"

dates_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                         sheetName = "Volhe&ChevaleriePP",
                         startRow = 1,
                         endRow = 1,
                         colIndex = 2:27,
                         header = F,
                         as.data.frame = T)

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                         sheetName = "Volhe&ChevaleriePP",
                         startRow = 11,
                         endRow = 21,
                         colIndex = 1:27,
                         header = F,
                         as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("La Volhe", data_read[,1], dates_read[1])
names(data_reg) = c("quartier", "id_piege", "date_first_pose")

#as specified later: each reading is 7 days
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")-7

#by observing the database
n_sites = nrow(data_read)
n_dates = ncol(dates_read)

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(dates_read[,i]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,1+i]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_LaVolhe_2017 = data


#####
# Chevalerie 2017

name_file = "DataBrut_EID_PP_2017_Andrea.xlsx"

dates_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                         sheetName = "Volhe&ChevaleriePP",
                         startRow = 1,
                         endRow = 1,
                         colIndex = 2:27,
                         header = F,
                         as.data.frame = T)

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "Volhe&ChevaleriePP",
                        startRow = 3,
                        endRow = 10,
                        colIndex = 1:27,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("Chevalerie", data_read[,1], dates_read[1])
names(data_reg) = c("quartier", "id_piege", "date_first_pose")

#as specified later: each reading is 7 days
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")-7

#by observing the database
n_sites = nrow(data_read)
n_dates = ncol(dates_read)

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(dates_read[,i]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,1+i]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_Chevalerie_2017 = data

#####
# Maurin 2017

name_file = "DataBrut_EID_PP_2017_Andrea.xlsx"

dates_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                         sheetName = "MaurinPP",
                         startRow = 1,
                         endRow = 1,
                         colIndex = 2:18,
                         header = F,
                         as.data.frame = T)

data_read <- read.xlsx2(file = paste0(folder_data, "/", name_file),
                        sheetName = "Volhe&ChevaleriePP",
                        startRow = 3,
                        endRow = 18,
                        colIndex = 1:18,
                        header = F,
                        as.data.frame = T)

# divide data into fix (names, locations) and variable

data_reg <- cbind("Maurin", data_read[,1], dates_read[1])
names(data_reg) = c("quartier", "id_piege", "date_first_pose")

#as specified later: each reading is 7 days
data_reg$date_first_pose = as.Date(as.numeric(data_reg$date_first_pose), origin = "1899-12-30")-7

#by observing the database
n_sites = nrow(data_read)
n_dates = ncol(dates_read)

#create a new database

data <- data.frame(quartier = rep(data_reg$quartier, n_dates),
                   id_piege = rep(data_reg$id_piege, n_dates),
                   date_detection = as.Date(rep(NA, n_sites*n_dates)),
                   observed_eggs = (rep(NA, n_sites*n_dates)),
                   trapping_days = (rep(NA, n_sites*n_dates)),
                   eggs_per_day = (rep(NA, n_sites*n_dates)))

date_pose = data_reg$date_first_pose

for(i in 1:n_dates){
  
  date_detection = as.Date(as.numeric(dates_read[,i]), origin = "1899-12-30")
  data$date_detection[((i-1)*n_sites+1) : (i*n_sites)] = date_detection
  
  eggs_brut <- data_read[,1+i]
  eggs_brut_elab<- as.numeric(eggs_brut)
  
  data$observed_eggs[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab
  data$trapping_days[((i-1)*n_sites+1) : (i*n_sites)] = date_detection - date_pose
  data$eggs_per_day[((i-1)*n_sites+1) : (i*n_sites)] = eggs_brut_elab/as.numeric(date_detection - date_pose)
  
  date_pose = date_detection
}

data_Maurin_2017 = data

# Aggregate everything

data_all <- rbind(data_Maurin_2016,
                  data_Maurin_2017,
                  data_LaVolhe_2016,
                  data_LaVolhe_2017,
                  data_Chevalerie_2016,
                  data_Chevalerie_2017,
                  data_VertBois_2016,
                  data_VertBois_2017)

data_all_summ <- data_all %>%
  group_by(date_detection, quartier)%>%
  dplyr::summarise(av_eggs_per_day = mean(eggs_per_day, na.rm = T),
                   me_eggs_per_day = median(eggs_per_day, na.rm = T)) %>% 
  ungroup()

library(ggplot2)

ggplot(data_all_summ )+
  geom_point(aes(x = date_detection, y = me_eggs_per_day, color = quartier))

## save everything

library(lubridate)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/"

Eggs_tot_df <- data_all_summ %>%
  rename(eggs = av_eggs_per_day)%>%
  mutate(type = "observed") %>%
  mutate(date = as.Date(date_detection)) %>%
  mutate(region = "194") %>% #in France!
  mutate(DOY = yday(date)) %>%
  mutate(DOS = julian(as.Date(date), origin = as.Date(paste0('2015-12-31')))) %>%
  ungroup()%>%
  select(c("region", "DOS", "eggs","DOY", "date"))

save(Eggs_tot_df, file = paste0(folder_out, "EID_Montpellier_2016_2017.RData"))


