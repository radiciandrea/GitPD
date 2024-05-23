library(ncdf4)
library(raster)

rm(list = ls())

data_ER_nc <- nc_open("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/AlbopictusShared/Eggcount_2w_23052011_25102021_EmiliaRomagna_ztigreonline.nc")
print(data_ER_nc)

# print(our_nc_data)

#number_ovitrap <- ncvar_get(data_ER_nc, "number_ovitrap")

attributes(data_ER_nc$var)
number_ovitrap <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[1])
number_eggs <- ncvar_get(data_ER_nc, attributes(data_ER_nc$var)$names[2])

# città dovrebbero essere nell'ordine alfabetico: Bologna, Ferrara, Forli Cesena, Modena, Parma, Piacenza, Ravenna, Reggio Emilia, Rimini