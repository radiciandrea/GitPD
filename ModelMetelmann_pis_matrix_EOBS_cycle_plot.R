# plot del cycle
# ModelMetelmann_pis_matrix_EOBS_cycle.R


rm(list = ls())

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)

folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_sim"

name = "W_EU"
year = 2005

load(paste0(folder_out, "/Sim_EOBS_", name, "_", year, ".RData"))

domain_sel <- st_read(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_", name, ".shp")) 

br = c(0, 10^(-3:3), 10^10)

#Metelmann map
#br = c(0, 10^(-3:3), 10^10)
#col_br= c("#384AB9", "#4F71D4", "#8CB1FF", "#8CB1FF", "#8CB1FF", "#8CB1FF", "#8CB1FF", "#8CB1FF")

domain_sel <- domain_sel%>%
  arrange(region) %>%
  mutate(E0 = E0_v)%>%
  mutate(E0_level=cut(E0, breaks=br,
                      labels=sapply(br[-length(br)], function(x){paste0(">", as.character(x))}))) %>%
  mutate(E0_level=factor(as.character(E0_level), levels=rev(levels(E0_level))))

regions_sh <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/regions_2015_metropole_region.shp")

if (name == "Occitanie") {
  regions_sh <- regions_sh %>%
    filter(Region == "Languedoc-Roussillon et Midi-P")
}


ggplot()+
  geom_sf(data = domain_sel, aes(fill = E0_level))+ #
  scale_fill_manual(values = rev(c("#007917", "#65992E", "#8FB338", "#E2CF4D", "#F89061", "#F96970", "#F97ADC", "#A494FB")))+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")
# + scale_fill_gradient(trans = "log")

ggplot()+
  geom_sf(data = domain_sel, aes(fill = E0_v))+ #E0_v
  scale_fill_gradient(trans = "log")+
  geom_sf(data = regions_sh, alpha = 0, colour = "grey90")


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
