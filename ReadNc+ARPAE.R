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
number_eggs <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[2]) # it is already Average number of eggs

# città dovrebbero essere nell'ordine alfabetico: Bologna, Ferrara, Forli Cesena, Modena, Parma, Piacenza, Ravenna, Reggio Emilia, Rimini
region_names <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[6])
week <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[7])
time_start <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[8])
time_end <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[9])

# day 0: 1/1/2000. 4018 days to 1/1/2011; 4383 to 1/1/2012...

# consider Bologna 2012

# Eggs_Bologna_2011_df = data.frame(DOY = time_start[1:(which(week==1)[1]-1)] -  (time_start[1]-week[1]*7),
#                               Eggs = number_eggs[1:(which(week==1)[1]-1),1]) 

Eggs_Bologna_2012_df = data.frame(DOY = time_start[which(week==1)[1]:(which(week==1)[2]-1)] - 4383.5,
                                  Eggs = number_eggs[which(week==1)[1]:(which(week==1)[2]-1), 1]) 

Eggs_Bologna_2012_df$type = "observed"
# Meteo da ARPAE https://dati.arpae.it/dataset/erg5-interpolazione-su-griglia-di-dati-meteo (non più https://dati.arpae.it/dataset/erg5-eraclito-91)
# es Bologna Cella 01421
# Ferrara 01573
# Forli Cesena 01948
# Modena 01138
# Parma 00774
# Piacenza 00369
# Ravenna 01983
# Reggio Emilia 00977
# Rimini 02191

W <- read.csv(unz("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/ArpAE/01421_2012 ERG5.zip", "01421_2012_d.csv"))              

obs_Bologna_2012_df <- data.frame(city = "Bologna",
                                  year = "2012",
                                  DOY = 1:366,
                                  P = W$DAILY_PREC,
                                  T_av = W$DAILY_TAVG) # is this the best?)

save(Eggs_Bologna_2012_df, obs_Bologna_2012_df, file =  "C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Bologna_2012.RData")

