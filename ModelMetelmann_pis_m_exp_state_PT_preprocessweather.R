# extract weather for specific places and run 
# with ModelMetelmann_pis_m_exp_state_PT


rm(list = ls())
gc()

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)
library(data.table)

# place
# Montpellier 1524
# Paris 3163
# Paris suburbs sud # 3082

df_cities = data.frame(city = c("Montpellier", "Paris", "Paris_S", "Madrid", "Rome_E", 
                                "London_N", "Berlin", "Lisbon", "Lyon", "Barcelona", 
                                "Milan", "Zurich", "Munich", "Bruxelles", "Amsterdam", 
                                "Sicily (Catania)", "Vienna", "Copenhagen", "Praga", 
                                "Ljubjiana", "Zagreb", "Stockhom", "Oslo", "Dublin"),
                       code = c("1524", "3163", "3082", "726", "1092", "4032", "4447", 
                                "396", "2140", "965", "2007", "2651", "2936", "3789", 
                                "4320", "275", "2955", "5084", "3588", "2259", "2184",
                                "5649", "5723", "4651"))


df_cities = data.frame(city = c("Montpellier", "Paris", "Madrid", "London_N", "Viterbo",
                                "Coimbra", "Firenze", "Lugano", "Frankfurt aum Main", "Innsbruck",
                                "Antwerp"),
                       code = c("1524", "3163",  "726","4032", "1221","657", "1602",
                                "2236", "3565", "2662", "3876"))

df_cities = data.frame(city = c("Montpellier", "Bilbao", "Augsburg", "Paris-centre", "Paris-suburbs", "Paris-region"),
                       code = c("1524", "1387", "3026", "3163", "3082", "3081"))

df_cities = data.frame(city = c("Montpellier", "Rennes", "Strasbourg"),
                       code = c("1524", "2883", "3104"))

for (city_x in df_cities$city){

region_x = df_cities$code[df_cities$city == city_x] # region of Montpellier

folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_elab" # "EOBS_elab"
name = "W_EU"


years = 2000:2023



W_df_l <- vector(mode = "list", length = length(years))

for (y in years){
  load(paste0(folder_eobs, "/EOBS_sel_", y, "_", name, ".RData"))

  W_df_l[[which(years == y)]] <- W_tot_df %>% filter(region ==region_x)
}

W_tot_df <-rbindlist(W_df_l)

save(W_tot_df, file = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_EOBS_",
     city_x, "_", min(years), "_", max(years), ".RData")) #Nizza

}

