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

# consider ALL REGIONS IN THE SAME DATAFRAME

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

region_codes = c("01421", "01573", "01948", "01138", "00774", "00369", "01983", "00977", "02191")
years = 2011:2021

W_tot_df <- as.data.frame(matrix(ncol = 6, nrow = 4018*9)) # weather from all the cities
names(W_tot_df) <- c("region", "year", "DOY", "date", "P",  "T_av")

k1 = 0 # counter

for(region in region_codes) {
  for(year in years){
    W <- read.csv(unz(paste0("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/ArpAE/",region,"_",year,".zip"),
                              paste0(region, "_", year, "_d.csv")))              
    
    W_df <- data.frame(region = region_names[which(region_codes == region)],
                       year = year,
                       DOY = nrow(W),
                       date = W$PragaDate,
                       P = W$DAILY_PREC,
                       T_av = W$DAILY_TAVG)
    
    k2 = k1 + nrow(W)
    W_tot_df[(k1+1):k2, ] = W_df
    k1 = k2
  }
}




save(Eggs_Bologna_2012_df, W_tot_df, file =  "C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Bologna_2012.RData")

