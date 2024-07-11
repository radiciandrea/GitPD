# plot del cycle
# MM_pis_matrix_EOBS_cycle_consec_plot.R

#per plottare anni consecutivi


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

first_day = as.Date(paste0(min(years), "-01-01"))
last_day = as.Date(paste0(max(years), "-12-31"))

DOS_sim = seq(from = first_day, to = last_day, by = 'day')
n_d = length(DOS_sim)

#carichiamo 1 per le dimensioni
load(paste0(folder_out, "/", files[1]))

n_r = ncol(Sim)-1
n_d_i = nrow(Sim)
Sim_tot = matrix(NA, ncol = n_r, nrow = n_d) 
Sim_tot[1:n_d_i,]=Sim

k = n_d_i
for (i in 2:length(files)){
  file = files[i]
  load(paste0(folder_out, "/", file))
  n_d_i = nrow(Sim)
  
  Sim_tot[]
}

# correzione degli NaN con formula geomatrica
E0_m_c <- apply(E0_m, 2, function(x){x[which(is.nan(x))] = exp(mean(log(x[which(is.nan(x)==F)]))); return(x)})
# E0_m_c <- E0_m

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
