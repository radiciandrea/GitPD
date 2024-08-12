# Function buld by Paul to download data

rm(list = ls())

# modified to include path

library(httr)
library(dplyr)
library(sf)
library(furrr)
library(readr)
library(lubridate)

# fun_extract_meteo_data <- function(month,year,path){
#   
#   data <- paste0("synop.",as.character(year),as.character(month),".csv.gz")
#   url <- paste0("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/Archive/",data)
#   
#   httr::GET(url,httr::write_disk(file.path(path,data)),httr::progress(),config = list(maxredirs=-1))
#   
# }

path = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance"
folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_elab" 

years <- 2007:2024


# change from Montpellier airport to Nice

df_meteofrance_2023_2024 <- read_delim(paste0(path, "/Q_06_latest-2023-2024_RR-T-Vent.csv.gz"), delim = ";", na = "", show_col_types = FALSE) %>%
  filter(NOM_USUEL %in% c("NICE")) %>%
  mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
  group_by(NOM_USUEL,date,year,month,week) %>%
  summarise(RFD = sum(RR, na.rm = T),
            TMN = mean(TM, na.rm = T),
            TMIN = mean(TN, na.rm = T),
            TMAX = mean(TX, na.rm = T)) %>%
  rename(nom_commune = NOM_USUEL)

#do the same also for previous years

df_meteofrance_2007_2022 <- read_delim(paste0(path, "/Q_06_previous-1950-2022_RR-T-Vent.csv.gz"), delim = ";", na = "", show_col_types = FALSE) %>%
  filter(NOM_USUEL %in% c("NICE")) %>%
  mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
  group_by(NOM_USUEL,date,year,month,week) %>%
  summarise(RFD = sum(RR, na.rm = T),
            TMN = mean(TM, na.rm = T),
            TMIN = mean(TN, na.rm = T),
            TMAX = mean(TX, na.rm = T)) %>%
  filter(year >= min(years)) %>%
  rename(nom_commune = NOM_USUEL)

df_meteofrance <- rbind(df_meteofrance_2007_2022, df_meteofrance_2023_2024)

folder_shape = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/"
name = "France"
domain = st_read(paste0(folder_shape, "/domain_sel_", name, ".shp"))

Montp_cell <- domain %>%
  filter(region == "194")

for(y in years){
  
  df_meteofrance_y <- df_meteofrance %>%
    filter(year == y)
  
  W_tot_df = df_meteofrance_y %>%
    mutate(region = Montp_cell$region) %>%
    rename(T_av = TMN) %>%
    rename(T_m = TMIN) %>%
    rename(T_M = TMAX) %>%
    rename(P = RFD) %>%
    mutate(lat = (Montp_cell$top+Montp_cell$bottom)/2) %>%
    mutate(lon = (Montp_cell$right+Montp_cell$left)/2) %>%
    mutate(pop = Montp_cell$popkm2) %>%
    mutate(DOY = as.numeric(strftime(date, format = "%j")))%>%
    mutate(DOS = DOY) %>%
    ungroup()%>%
    select(-c("month", "week"))
  
  save(W_tot_df, file = paste0(folder_out, "/Nice_", y, "_France.RData") )
}
