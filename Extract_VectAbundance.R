# code to extract abundances from VectAbundance (Da Re et al)

#https://ecoevorxiv.org/repository/view/6444/

rm(list = ls())

library(readxl)
library(dplyr)


folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/VectAbundance/"

#read excel
data <- read_excel(path = paste0(folder_eobs, "Vectabundace_v015.xlsx"))


