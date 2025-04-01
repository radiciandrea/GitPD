# code to extract available meteo data from given meteo stations in France

rm(list = ls())
gc()

library(dplyr)
library(pracma)
library(data.table)
library(vroom)
library(httr)
library(furrr)
library(readr)
library(lubridate)
library(stringr)

folder_admin = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Admin_France"
path = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance"

# modify the following. weather_station may be != cities: look here
cities = c("Marseille", "Avignon", "Gap") 
#weather_station = str_to_upper(sort(cities)) 
weather_station = str_to_upper(sort(cities)) 
years = 2000:2023

# automatically changed
insee_code <- vroom(paste0(folder_admin, "/meta_base_cc_comparateur.csv")) %>%
  filter(LIB_MOD %in% cities) %>%
  select(c("LIB_MOD", "COD_MOD")) %>%
  rename(city = LIB_MOD, insee = COD_MOD)

admin_data <- vroom(paste0(folder_admin, "/base_cc_comparateur.csv")) %>%
  filter(CODGEO %in% insee_code$insee) %>%
  select(c("CODGEO", "P21_POP", "SUPERF"))%>%
  rename(insee = CODGEO, population = P21_POP, surface = SUPERF)


df_cities <- left_join(insee_code, admin_data) %>%
  mutate(dep = substr(insee, 1, 2)) %>%
  arrange(city) %>%
  mutate(weather_station = weather_station)

list_departements <- unique(df_cities$dep)

# for(i in 1:length(list_departements)){
#   # données 2024-2025
#   httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",list_departements[i],"_latest-2024-2025_RR-T-Vent.csv.gz"),httr::write_disk(file.path(path,paste0("dpt_",list_departements[i],"_2024_2025_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
# 
#   # donnees historiques
#   httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",list_departements[i],"_previous-1950-2023_RR-T-Vent.csv.gz"),httr::write_disk(file.path(path,paste0("dpt_",list_departements[i],"_historique_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
# }


for (city_x in cities){
  
  weather_station_x = df_cities %>% filter(city == city_x) %>% pull(weather_station)
  dep_x = df_cities %>% filter(city == city_x) %>% pull(dep)
  pop_x = df_cities %>% filter(city == city_x) %>% pull(population) / df_cities %>% filter(city == city_x) %>% pull(surface)
  
  
  
  W_tot_df <-  read_delim(paste0(path, "/dpt_",dep_x, "_historique_RR-T-Vent.csv.gz"), delim = ";", na = "", show_col_types = FALSE) %>%
    filter(NOM_USUEL %in% c(weather_station_x)) %>%
    mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
    filter(year >= min(years)) %>%
    select(c("LAT", "LON", "RR", "TM", "TN", "TX", "NOM_USUEL", "date", "year")) %>%
    rename(lat = LAT, lon = LON, P = RR, T_av = TM, T_m = TN, T_M = TX, region = NOM_USUEL) %>%
    mutate(DOY = yday(date)) %>%
    mutate(DOS = yday(date)) %>%
    mutate(pop = pop_x)
  
  #corrections for missing data
  na_T_av = which(is.na(W_tot_df$T_av))
  W_tot_df$T_av[na_T_av] = 0.5*(W_tot_df$T_m[na_T_av]+W_tot_df$T_M[na_T_av])
  W_tot_df$P[which(is.na(W_tot_df$P))] = 0
  
  save(W_tot_df, file = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_MeteoFrance_",
                               city_x, "_", min(years), "_", max(years), ".RData")) #Nizza
  
}
