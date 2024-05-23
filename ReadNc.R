library(ncdf4)
library(raster)
library(dplyr)

rm(list = ls())

data_ER_nc <- nc_open("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/AlbopictusShared/Eggcount_2w_23052011_25102021_EmiliaRomagna_ztigreonline.nc")
print(data_ER_nc)

# print(our_nc_data)

#number_ovitrap <- ncvar_get(data_ER_nc, "number_ovitrap")

attributes(data_ER_nc$var)
number_ovitrap <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[1])
number_eggs <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[2])

# città dovrebbero essere nell'ordine alfabetico: Bologna, Ferrara, Forli Cesena, Modena, Parma, Piacenza, Ravenna, Reggio Emilia, Rimini
region_names <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[6])
week <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[7])
time_start <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[8])
time_end <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[9])

# consider Bologna 2011

Eggs_Bologna_2011_df = data.frame(DOY = time_start[1:(which(week==1)[1]-1)] -  (time_start[1]-week[1]*7),
                              Eggs = number_eggs[1:(which(week==1)[1]-1)]) 

# Meteo da ARPAE https://dati.arpae.it/dataset/erg5-eraclito-91
# es Bologna Cella 01421

W <- read.csv("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/ArpAE/01421_2011/01421_2011_d.csv")

#Create df Bologna 2011

obs_Bologna_2011_df <- data.frame(city = "Bologna",
                                  year = "2011",
                                  DOY = 1:365,
                                  P = W$DAILY_PREC,
                                  T_av = (W$DAILY_TMIN+W$DAILY_TMAX)/2) # is this the best?)
obs_Bologna_2011_df <- right_join(Eggs_Bologna_2011_df, obs_Bologna_2011_df)
