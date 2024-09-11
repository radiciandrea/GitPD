# plot del cycle
# MM_pis_matrix_EOBS_cycle_consec_R0.R

#per plottare R0 

rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(lubridate)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim_consec_R0"

files = list.files(folder_out, pattern = ".RData")

name = substring(files[1], 13, 16)
years = substring(files, 18, 21)

first_day = as.Date(paste0(min(years), "-01-01"))
last_day = as.Date(paste0(max(years), "-12-31"))

date_sim = seq(from = first_day, to = last_day, by = 'day')
DOS_sim = 1:length(date_sim)
n_d = length(DOS_sim)

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

n_c = 5 # numero di classi
n_r = ncol(R0) #numero di regioni
regions = 1:n_r

n_d_i = nrow(R0)
R0_tot = matrix(NA, ncol = n_r, nrow = n_d) 
R0_tot[1:n_d_i,]=R0

# sum R_0 >1
R0_m = matrix(NA, ncol = n_r, nrow = length(files))
R0_m[1,] = colSums(R0>0)

k = n_d_i
for (i in 2:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  n_d_i = nrow(R0)
  R0_tot[k + 1:n_d_i,]=R0
  k = k + n_d_i
  R0_m[i,] = colSums(R0>0)
}


#########################
#plot

R0_m_df = data.frame("variable" = "R0",
                              "region" = rep(regions, each = max(DOS_sim)),
                              "t" = rep(DOS_sim, n_r),
                              "value" = c(R0_tot)) #5 classes

#210

id_reg = 1597 #

#Roma: 1091, 1992
#Nizza: 1597 e un'altra

region_x = id_reg #regions[id_reg]

R0_m_x_df <- R0_m_df %>%
  filter(region == region_x)

R0_x_df<- dcast(R0_m_x_df, t ~ variable)

ggplot(R0_m_x_df, aes(x = t, y = value, color = variable))+
  geom_line()+
  scale_y_continuous(trans='log2', limits = c(1, max(R0_m_x_df$value)))+
  # ylim(1, max(R0_m_x_df$value))+
  # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
  labs(color = paste0("Abundances per classes (", region_x, ")")) +
  theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),

#### Geo plot 

years_sel_1 = 2007:2014 # # 2006:2016
R0_1 = colMeans(R0_m[which(years %in% years_sel_1),])

years_sel_2 = 2015:2022 # 2017:2023 
R0_2 = colMeans(R0_m[which(years %in% years_sel_2),])

# to plot
domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_W_EU.shp")) %>%
  arrange(region)

x = c(224, 123, 62, 31, 0, 1)
x_lab = c(">4 m", ">2 m", ">1 m", ">0 d", "0 d")
col_x <- c("#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724")


# domain_years_sel <- domain_sel%>%
#   arrange(region) %>%
#   mutate(R0_1 = R0_sel_1)%>%
#   mutate(R0_1_level=cut(R0_1, breaks=br,
#                         labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
#   mutate(R0_1_level=factor(as.character(R0_1_level), levels=rev(levels(R0_1_level)))) %>%
#   mutate(R0_2 = R0_sel_2)%>%
#   mutate(R0_2_level=cut(R0_2, breaks=br,
#                         labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
#   mutate(R0_2_level=factor(as.character(R0_2_level), levels=rev(levels(R0_2_level))))

R0_1_level <- case_when(R0_1 > x[1] ~ x_lab[1],
                      R0_1 > x[2] ~ x_lab[2],
                      R0_1 > x[3] ~ x_lab[3],
                      R0_1 > x[4] ~ x_lab[4],
                      R0_1 == x[5] ~ x_lab[5])

R0_2_level <- case_when(R0_2 > x[1] ~ x_lab[1],
                        R0_2 > x[2] ~ x_lab[2],
                        R0_2 > x[3] ~ x_lab[3],
                        R0_2 > x[4] ~ x_lab[4],
                        R0_2 == x[5] ~ x_lab[5])

Risk_zone <-  case_when((R0_1 < 1) & (R0_2 < 1) ~ "Not potentially endemic",
                        (R0_1 > 1) & (R0_2 > 1) ~ "Historically potentially endemic",
                        (R0_1 < 1) & (R0_2 > 1) ~ "Newly potentially endemic")

if (name == "France") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
} else if (name == "Occitanie") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")
  regions_sh <- regions_sh %>%
    filter(Region == "Languedoc-Roussillon et Midi-P")
} else if (name == "W_EU") {
  regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/W_EU_s.shp")
}

# Plots: 2010 and 2020s

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = R0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_x))+
  # geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  # geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(paste0("R0 Dengue, period = ", min(years_sel_1), "-", max(years_sel_1)))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = R0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_x))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  # geom_sf(data = obs_GBIF, alpha = 1, colour = "green", size=0.3)+
  # geom_sf(data = obs_Kramer, alpha = 1, colour = "yellow", size=0.8)+
  ggtitle(paste0("R0 Dengue, period = ", min(years_sel_2), "-", max(years_sel_2)))
# + scale_fill_gradient(trans = "log")

### plot for CC RIO conference
library(ggspatial)
library(prettymapr)
library(ggrepel)
library(RJSONIO)

folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/ArtiConForm/08bis_CC_RIO_SOOI_Sep_2024/Images/"

countries_sh <-  st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/european-countries.shp")

#plot 1

g1 <- ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = R0_1_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_x))+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60)) +
  ggtitle(bquote(R[0]~~(period~2007-2014)))+
  theme_minimal()+
  guides(fill=guide_legend(title=bquote(R[0])))

ggsave(file= paste0(folder_plot, "R0_1_level.png"), plot= g1 , units="in", width=5.5, height=7, dpi=300)

#plot 2

g2 <- ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = R0_2_level), colour = NA)+ #
  scale_fill_manual(values = rev(col_x))+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60)) +
  ggtitle(bquote(R[0]~~(period~2015-2022)))+
  theme_minimal() +
  guides(fill=guide_legend(title=bquote(R[0])))

ggsave(file= paste0(folder_plot, "R0_2_level.png"),  plot= g1 , units="in", width=5.5, height=7, dpi=300)

# plot 3

col_x_sint_FR<- c("#384AB4",  "#F29878", "#B00026") #"#8EB0FE",

gvar <- ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = Risk_zone), colour = NA)+
  scale_fill_manual(values = rev(col_x_sint_FR))+
  geom_sf(data = countries_sh, alpha = 0, colour = "grey90")+
  coord_sf(xlim = c(-15, 18), ylim = c(36, 60)) +
  ggtitle(bquote(R[0]~qualitative~variation))+
  theme_minimal() +
  guides(fill=guide_legend(title="Change"))

ggsave(file= paste0(folder_plot, "R0_var_level.png"), plot= gvar , units="in", width=5.5, height=7, dpi=300)

