# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R

#per plottare anni indipendenti


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)

type = "_01"

folder_out = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim", type)

# name = "W_EU"
# year = 2005

files = list.files(folder_out)

name = substring(files[1], nchar(type)+10, nchar(type)+13)
years = substring(files, nchar(type)+15, nchar(type)+18) #CORREGGGERE QUI

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

E0_m = matrix(NA, ncol = length(E0_v), nrow = length(files))
rm(Sim)

for (i in 1:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  E0_m[i,]= E0_v
  rm(Sim)
}

# correzione degli NaN con formula geomatrica
E0_m_c <- apply(E0_m, 2, function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
# E0_m_c <- E0_m

#Metelamnn, geometric mean. = exp(mean(log))
years_sel_1 = 2007:2014 # # 2006:2016
E0_m_c_sel_1 <- apply(E0_m[which(years %in% years_sel_1),], 2,
                    function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_sel_1 = apply(E0_m_c_sel_1, 2,
               function(x){exp(mean(log(x)))})

years_sel_2 = 2015:2022 # 2017:2023 
E0_m_c_sel_2 <- apply(E0_m[which(years %in% years_sel_2),], 2,
                      function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
E0_sel_2 = apply(E0_m_c_sel_2, 2,
                 function(x){exp(mean(log(x)))})

E0_diff = (E0_sel_2 - E0_sel_1)/E0_sel_1

# to plot

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel", type, "_", name,".shp")) 

br = c(0, 10^(-3:3), 10^10)
br_diff = c(-10^c(2, log(50,10), 0), 0, 10^(0:3), 10^10)

#Metelmann map
col_br= c("#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3", "#F2CDBB", "#F29878", "#D04B45", "#B00026")

domain_years_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_1 = E0_sel_1)%>%
  mutate(E0_1_level=cut(E0_1, breaks=br,
                      labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_1_level=factor(as.character(E0_1_level), levels=rev(levels(E0_1_level)))) %>%
  mutate(E0_2 = E0_sel_2)%>%
  mutate(E0_2_level=cut(E0_2, breaks=br,
                        labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_2_level=factor(as.character(E0_2_level), levels=rev(levels(E0_2_level)))) %>%
  mutate(E0_diff = E0_diff)%>%
  mutate(E0_diff_level=cut(E0_diff, breaks=br_diff,
                           labels=sapply(br_diff[-length(br_diff)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_diff_level=factor(as.character(E0_diff_level), levels=rev(levels(E0_diff_level))))

if (name == "France") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
} else if (name == "Occitanie") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
  regions_sh <- regions_sh %>%
    filter(Region == "Languedoc-Roussillon et Midi-P")
} else if (name == "W_EU") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/W_EU_s.shp")
}

obs_Kramer <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/Kramer_2015_Albo_W_EU.shp")
obs_GBIF <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/GBIF_dwnl_Albo_W_EU.shp")

# Plots: 2010 and 2020s

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(paste0("R0 diapausing eggs, period = ", min(years_sel_1), "-", max(years_sel_1)))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  geom_sf(data = obs_GBIF, alpha = 1, colour = "green", size=0.3)+
  geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(paste0("R0 diapausing eggs, period = ", min(years_sel_2), "-", max(years_sel_2)))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_diff_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  ggtitle(paste0("% multiplier R0 diapausing eggs, [",
                 min(years_sel_2),"-", max(years_sel_2),"] vs [",
                 min(years_sel_1),"-", max(years_sel_1),"]"))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_v))+ #E0_v
  scale_fill_gradient(trans = "log")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

# alternative

E0_freq_m <- colSums(E0_m>1, na.rm = T)/colSums(E0_m>0, na.rm = T)

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_freq_m), colour = NA)+ #
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow")+
  ggtitle(paste0("R0 diapausing eggs, period = ", min(years_sel), "-", max(years_sel)))
# + scale_fill_gradient(trans = "log")


## New areas with R0 > 1 (Risk of establishment)
### processing for EMERGENCE conference

col_br_sint= c("#384AB4", "#8EB0FE", "#F29878", "#B00026")
levels_sint= c("Historically suitable", "Recently suitable", "Recently unsuitable", "Historically unsuitable")

domain_indicators <- domain_sel%>%
  arrange(region) %>%
  mutate(E0_hist = E0_sel_1)%>%
  mutate(E0_recent = E0_sel_2)%>%
  mutate(Risk_zone = case_when(
    (E0_hist > 1) & (E0_recent > 1) ~ "Historically suitable",
    (E0_hist < 1) & (E0_recent > 1) ~ "Recently suitable",
    (E0_hist > 1) & (E0_recent < 1) ~ "Recently unsuitable",
    (E0_hist < 1) & (E0_recent < 1) ~ "Historically unsuitable",
    .default = "NA"
  ))

domain_indicators$Risk_zone <- factor(domain_indicators$Risk_zone, levels = levels_sint)

# net increase suitable area
100*(sum(domain_indicators$E0_recent>1, na.rm = T)/sum(domain_indicators$E0_hist>1, na.rm = T)-1)

ggplot()+
  geom_sf(data = domain_indicators, aes(fill = Risk_zone), colour = NA)+
  scale_fill_manual(values = rev(col_br_sint))

### plot for EMERGENCE conference
library(ggspatial)
library(prettymapr)
library(ggrepel)
library(RJSONIO)

folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/ArtiConForm/07_Exposome_sep_2024/images/"

regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")

domain_years_sel_FR <- domain_years_sel %>%
  filter(!is.na(Country))

domain_indicators_FR <- domain_indicators %>%
  filter(!is.na(Country))

# relative increase suitable area in France over historic
100*(sum(domain_indicators_FR$E0_recent>1, na.rm = T)/sum(domain_indicators_FR$E0_hist>1, na.rm = T)-1)

# relative increase suitable area in France
#historic
100*(sum(domain_indicators_FR$E0_hist>1, na.rm = T)/nrow(domain_indicators_FR))
#recent
100*(sum(domain_indicators_FR$E0_recent>1, na.rm = T)/nrow(domain_indicators_FR))
#recent-historic
100*((sum(domain_indicators_FR$E0_recent>1, na.rm = T) - sum(domain_indicators_FR$E0_hist>1, na.rm = T))/nrow(domain_indicators_FR))
#unsuitable
100*(sum(domain_indicators_FR$E0_recent<1, na.rm = T)/nrow(domain_indicators_FR))


# plot cities (from)

locateCountry = function(nameCity, codeCountry) {
  cleanCityName = gsub(' ', '%20', nameCity)
  url = paste(
    "http://nominatim.openstreetmap.org/search?city="
    , cleanCityName
    , "&countrycodes="
    , codeCountry
    , "&limit=9&format=json"
    , sep="")
  resOSM = fromJSON(url)
  if(length(resOSM) > 0) {
    return(c(resOSM[[1]]$lon, resOSM[[1]]$lat))
  } else return(rep(NA,2)) 
}

cities_df <- data.frame(name = c("Paris", "Marseille", "Lyon", "Toulouse", "Bordeaux", "Nice", "Lille", "Montpellier", "Strasbourg", "Rennes", "Nantes", "Ajaccio"))
cities_df$code = "FR"

coord = t(apply(cities_df, 1, function(aRow) as.numeric(locateCountry(aRow[1], aRow[2]))))

cities_df$lon = coord[,1]
cities_df$lat = coord[,2]

points <- lapply(1:nrow(coord), function(i){st_point(coord[i,])})
points_sf <- st_sfc(points, crs = 4326)
cities_sf <- st_sf('city' = cities_df$name, 'geometry' = points_sf)  

#plot 1

g1 <- ggplot()+
  geom_sf(data = domain_years_sel_FR, aes(fill = E0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "white")+
  geom_sf(data = points_sf)+
  ggtitle(bquote(E[0]~~(period~2007-2014)))+
  theme_minimal()+
  guides(fill=guide_legend(title=bquote(E[0])))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.25, size = 6)

ggsave(file= paste0(folder_plot, "E0_1_level.svg"), plot= g1 , width=10, height=8)

#plot 2

g2 <- ggplot()+
  geom_sf(data = domain_years_sel_FR, aes(fill = E0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "white")+
  ggtitle(bquote(E[0]~~(period~2015-2022)))+
  theme_minimal() +
  guides(fill=guide_legend(title=bquote(E[0])))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.25, size = 6)

ggsave(file= paste0(folder_plot, "E0_2_level.svg"), plot= g2 , width=10, height=8)

# plot 3

col_br_sint_FR<- c("#384AB4",  "#F29878", "#B00026") #"#8EB0FE",

gvar <- ggplot()+
  geom_sf(data = domain_indicators_FR, aes(fill = Risk_zone), colour = NA)+
  scale_fill_manual(values = rev(col_br_sint_FR))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  ggtitle(bquote(E[0]~qualitative~variation))+
  theme_minimal() +
  guides(fill=guide_legend(title="Change"))+
  geom_sf(data = cities_sf) +
  geom_label_repel(data = cities_df, aes(x = lon, y = lat, label = name),
                   label.padding = 0.25, size = 6)
  # geom_text(data = cities_df, aes(x = lon, y = lat, label = name), hjust=-0.1, vjust=-0.1)

ggsave(file= paste0(folder_plot, "E0_var_level.svg"), plot= gvar , width=10, height=8)

100*(sum(domain_indicators_FR$E0_recent>1, na.rm = T)/sum(domain_indicators_FR$E0_hist>1, na.rm = T)-1)

#Export as: 800 * 600? 
