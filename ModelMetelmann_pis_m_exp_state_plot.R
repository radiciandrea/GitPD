# I try to code the model by Metelmann 2019
# Running on matrix in which we alter the weather variable

# EXPOSURE STATE (T, P) + PARAMETERS IN SOLVER

#T' = T + T_add
#P' = P*P_mol in summer or P^n/sum(P)/sum(P^n)

rm(list = ls())
gc()

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)
library(data.table)
library(pracma)

#Plot
library(metR)
library(ggrepel)
library(ggpubr)

cities = c("Montpellier", "Paris", "Paris_S", "Madrid", "Rome_E", 
           "London_N", "Berlin", "Lisbon", "Lyon", "Barcelona", 
           "Milan", "Zurich", "Munich", "Bruxelles", "Amsterdam", 
           "Sicily (Catania)", "Vienna", "Copenhagen", "Praga", 
           "Ljubjiana", "Zagreb", "Stockhom", "Oslo", "Dublin")

cities = c("Coimbra", "Montpellier", "Madrid", "Lugano", "Frankfurt aum Main", "Paris", "Innsbruck", "Antwerp", "London_N")

folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Exposure_state_mol/"
# #or
# folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Exposure_state_pow/"

years_eval = c(2003, 2008, 2013, 2018, 2023)

for (city_x in cities){
  
  load(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_EOBS_",
              city_x, "_", min(years), "_", max(years), ".RData"))
  
  load(paste0(folder_plot, "Ind_", city_x,".RData"))
  
  point_df <- data.frame("name" = city_x,
                         "year" = years_eval,
                         "T_av_summer" = NA,
                         "P_summ_tot" = NA)
  
  # recc Didier: between 1 may and 1 oct
  for(y in years_eval){
    
    point_df$T_av_summer[which(point_df$year == y)] <- W_tot_df %>% 
      filter(year %in% c((y-3):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 274) %>% #1st of october
      dplyr::summarize(T_av_m = mean(T_av)) %>%
      pull(T_av_m)
    
    point_df$P_summ_tot[which(point_df$year == y)] <- W_tot_df %>% 
      filter(year %in% c((y-3):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 274) %>% #1st of october
      dplyr::summarize(P_summ_tot = sum(P)/length(years)) %>%
      pull(P_summ_tot)
  }

  
  #Ad
  breaks_E0 = c(0, 8)
  
  # g1 <- ggplot() +
  #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = Ad ), breaks = breaks_Ad) +
  #   ggtitle(paste0("Average log adults/ha (May to Sept) in ", city_x))+
  #   theme_test()+ 
  #   geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
  #                    label.padding = 0.15) #size = 4
  
  g0_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = E0))+
    scale_fill_viridis_c(limits = c(min(breaks_E0), max(breaks_E0)),
                         na.value = "#32003C")+
    ggtitle(paste0("Average E0~, ", city_x))+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = E0),
                 color = "red", breaks = c(1))+
    theme_test()+
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
                     label.padding = 0.15) #size = 4

  
  #Ad
  breaks_Ad = seq(3, 30000, by = 500)
  
  # g1 <- ggplot() +
  #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = Ad ), breaks = breaks_Ad) +
  #   ggtitle(paste0("Average log adults/ha (May to Sept) in ", city_x))+
  #   theme_test()+ 
  #   geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
  #                    label.padding = 0.15) #size = 4
  
  g1_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = log10(Ad)))+
    scale_fill_viridis_c(limits = c(min(log10(breaks_Ad)), max(log10(breaks_Ad))),
                         na.value = "#32003C")+
    ggtitle(paste0("Average log10 adults/ha (May to Sept)"))+
    theme_test()+
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
                     label.padding = 0.15) #size = 4
  
  #R0
  breaks_R = c(0, 0.5, 1, 2, 4, 7, 10)
  
  # g2 <- ggplot()+
  #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = R0), breaks = breaks_R)+
  #   geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = R0),
  #                color = "red", breaks = c(1))+
  #   ggtitle(paste0("Average R0 (May to Sept)"))+
  #   theme_test()+ 
  #   geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
  #                    label.padding = 0.15) #size = 4
  
  g2_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = R0))+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = R0),
                 color = "red", breaks = c(1))+
    scale_fill_viridis_c(limits = c(min(breaks_R), max(breaks_R)),
                         na.value = "#32003C")+
    ggtitle("Average R0 (May to Sept)")+
    theme_test()+
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
                     label.padding = 0.15) #size = 4
  
  
  breaks_nR = c(0, 1, 5, 10, 20, 50, 90, 150)
  
  # g3 <- ggplot()+
  #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = nR0), breaks = breaks_nR)+
  #   ggtitle(paste0("n days with R0 >1"))+
  #   theme_test()+
  #   geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "black") +
  #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
  #                    label.padding = 0.15) #size = 4
  
  g3_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = nR0))+
    scale_fill_viridis_c(limits = c(min(breaks_nR), max(breaks_nR )),
                         na.value = "#32003C")+
    ggtitle("n days with R0 >1")+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = nR0),
                 color = "red", breaks = c(1))+
    theme_test()+
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
                     label.padding = 0.15) #size = 4
  
  
  # Save
  g_tot <- ggarrange(g0_c, g1_c, g3_c, ncol = 1)
  
  ggsave(paste0(folder_plot, "NEW_g", city_x ,".png"), g_tot, units="in", height=8, width= 5.5, dpi=300)
}
