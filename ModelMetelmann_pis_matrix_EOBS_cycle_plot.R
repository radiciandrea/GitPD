# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim"

# name = "W_EU"
# year = 2005

files = list.files(folder_out)

name = substring(files[1], 10, 13)
years = substring(files, 15, 18)

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

E0_m = matrix(NA, ncol = length(E0_v), nrow = length(files))

for (i in 1:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  E0_m[i,]= E0_v
}

# correzione degli NaN con formula geomatrica
E0_m_c <- apply(E0_m, 2, function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})

#Metelamnn, geometric mean. = exp(mean(log))
years_sel = 2017:2023 # 2017:2023 # 2006:2016
E0_sel = apply(E0_m_c[which(years %in% years_sel),], 2, function(x){exp(mean(log(x)))})

E0_diff = 100*(apply(E0_m_c[which(years %in% 2017:2023),], 2, function(x){exp(mean(log(x)))})- 
  apply(E0_m_c[which(years %in% 2006:2016),], 2, function(x){exp(mean(log(x)))}))/
  apply(E0_m_c[which(years %in% 2006:2016),], 2, function(x){exp(mean(log(x)))})

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

br = c(0, 10^(-3:3), 10^10)
br_diff = c(-10^c(2, log(50,10), 0), 0, 10^(0:3), 10^10)

#green-yellow-red-purple
col_br= c("#007917", "#65992E", "#8FB338", "#E2CF4D", "#F89061", "#F96970", "#F97ADC", "#A494FB")

#Metelmann map
col_br= c("#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3", "#F2CDBB", "#F29878", "#D04B45", "#B00026")

domain_years_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0 = E0_sel)%>%
  mutate(E0_level=cut(E0, breaks=br,
                      labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_level=factor(as.character(E0_level), levels=rev(levels(E0_level)))) %>%
  mutate(E0_diff = E0_diff)%>%
  mutate(E0_diff_level=cut(E0_diff, breaks=br_diff,
                      labels=sapply(br_diff[-length(br_diff)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_diff_level=factor(as.character(E0_diff_level), levels=rev(levels(E0_diff_level))))

regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")

if (name == "Occitanie") {
  regions_sh <- regions_sh %>%
    filter(Region == "Languedoc-Roussillon et Midi-P")
}

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_level))+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  ggtitle(paste0("R0 diapausing eggs, period = ", min(years_sel), "-", max(years_sel)))
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_diff_level))+ #
  scale_fill_manual(values = rev(col_br))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")+
  ggtitle("% difference R0 diapausing eggs, [2017-2023] vs [2006-2016]")
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_years_sel, aes(fill = E0_v))+ #E0_v
  scale_fill_gradient(trans = "log")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")

#########################
#plot pop
Sim_m_df = data.frame("variable" = rep(c("E", "J", "I", "A", "E_d"), each = n_r*max(DOS_sim)),
                      "region" = rep(rep(regions, each = max(DOS_sim)), 5),
                      "t" = rep(DOS_sim, n_r*5),
                      "value" = c(Sim[, 2:(1+5*n_r)])) #5 classes

# st_write(domain_sel, paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/res_sim_2011_", name, ".shp"))
#plot
id_reg = 210 #Montpellier = 93 in Occitanie # in Francia 340 cella maledetta, 568, 569, 608, 650 # 126 (maghreb), 210 max

region_x = regions[id_reg]

Sim_m_x_df <- Sim_m_df %>%
  filter(region == region_x)
Sim_x_df<- dcast(Sim_m_x_df, t ~ variable)

ggplot(Sim_m_x_df, aes(x = t, y = value, color = variable))+
  geom_line()+
  scale_y_continuous(trans='log2', limits = c(1, max(Sim_m_x_df$value)))+
  # ylim(1, max(Sim_m_x_df$value))+
  # ggtitle(paste0("Abundances per classes (", region_x, ")")) +
  labs(color = paste0("Abundances per classes (", region_x, ")")) +
  theme(legend.position = "bottom") #plot.title=element_text(margin=margin(t=40,b=-30)),

# compute laid eggs: change into integration function #beta should be calculatedd hour by hour
