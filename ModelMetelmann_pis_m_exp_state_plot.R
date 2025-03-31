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

cities = c("Montpellier", "Bilbao", "Augsburg", "Paris-centre", "Paris-suburbs", "Paris-region")

cities = c("Montpellier", "Rennes", "Strasbourg")

cities = c("Montpellier", "Toulouse", "Rodez")

cities = c("Antwerp")

cities = c("Marseille", "Avignon", "Gap")

years_eval = c(2004, 2007, 2010, 2014, 2017, 2020, 2023) #2004:2023
years_eval = c(2004, 2023)
years_lab = c("2000-2004", "2019-2023")
years = 2000:2023

#MULTIPLICATIVE
folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Exposure_state_mol/"
# #or
# folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Exposure_state_pow/"

for (city_x in cities){
  
  load(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_EOBS_",
              city_x, "_", min(years), "_", max(years), ".RData"))
  
  load(paste0(folder_plot, "Ind_", city_x,".RData"))
  
  point_df <- data.frame("name" = city_x,
                         "year" = years_eval,
                         "T_av_summer" = NA,
                         "P_summ_tot" = NA,
                         "E0" = NA,
                         "Ad" = NA,
                         "nR0" = NA,
                         "labE0" = years_lab,
                         "labAd" = years_lab,
                         "labnR0" = years_lab)
  
  # recc Didier: between 1 may and 1 oct
  for(y in years_eval){
    
    T_av_summer_x <- W_tot_df %>% 
      filter(year %in% c((y-4):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 304) %>% #1st of november
      dplyr::summarize(T_av_summer = mean(T_av)) %>%
      pull(T_av_summer)
    
    P_summ_tot_x <- W_tot_df %>% 
      filter(year %in% c((y-4):y)) %>%
      filter(DOY >= 121) %>% #1st may
      filter(DOY < 304) %>% #1st of november
      dplyr::summarize(P_summ_tot = sum(P)/length(c((y-4):y))) %>%
      pull(P_summ_tot)
    
    err = (Ind_df$T_av_summer-T_av_summer_x)^2/max(Ind_df$T_av_summer)+(Ind_df$P_summ_tot-P_summ_tot_x)^2/max(Ind_df$P_summ_tot)
    
    E0_x <- Ind_df$E0[which(err == min(err))]
    Ad_x <- Ind_df$Ad[which(err == min(err))]
    nR0_x <- Ind_df$nR0[which(err == min(err))]
    
    point_df$T_av_summer[which(point_df$year == y)] <- T_av_summer_x
    point_df$P_summ_tot[which(point_df$year == y)] <- P_summ_tot_x
    point_df$E0[which(point_df$year == y)] <- E0_x
    point_df$Ad[which(point_df$year == y)] <- Ad_x
    point_df$nR0[which(point_df$year == y)] <- nR0_x
    point_df$labE0[which(point_df$year == y)] <- paste0(y-4,"-",y, " (", round(E0_x,1), ")")
    point_df$labAd[which(point_df$year == y)] <- paste0(y-4,"-",y, " (", round(Ad_x,1), ")")
    point_df$labnR0[which(point_df$year == y)] <- paste0(y-4,"-",y, " (", round(nR0_x,1), ")")
}
  
  # prepare labels
  
  
  
  # #E0
  # breaks_E0 = c(10^(-7), 10^7)
  # 
  # g0_c <- ggplot()+
  #   geom_contour_fill(data = Ind_df,
  #                     aes(x = T_av_summer, y = P_summ_tot, z = log10(E0)))+
  #   scale_fill_viridis_c(limits = log10(c(min(breaks_E0), max(breaks_E0))),
  #                        na.value = "#32003C", option = "plasma")+
  #   ggtitle(paste0("Average E0, ", city_x))+
  #   geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = E0),
  #                color = "black", breaks = c(1))+
  #   theme_test()+
  #   geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
  #   geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color = "white", size = 2) +
  #   guides(size = "legend", colour = "none")+
  #   scale_color_grey()+
  #   geom_label_repel(data = point_df %>% filter(year ==  2004 | year ==  2023), aes(x = T_av_summer, y = P_summ_tot, label = lab),
  #                    label.padding = 0.15, segment.color = NA) #size = 4
  
  
  br = 10^(-7:7)
  col_br = c("#020206","#141a40","#26327a", "#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3",
             "#F2CDBB", "#F29878", "#D04B45", "#B00026", "#640015", "#180005", "#000000")
  
  Ind_df <- Ind_df%>%
    mutate(E0_app = E0)
  
  Ind_df$E0_app[which(Ind_df$E0> max(br))] = max(br)
  Ind_df$E0_app[which(Ind_df$E0< min(br))] = min(br)
  
  min_E0_app = floor(log10(min(Ind_df$E0_app)))
  max_E0_app = ceil(log10(max(Ind_df$E0_app)))
  
  br_post = 10^seq(min_E0_app, max_E0_app, by = 1)
  col_br_post = col_br[which(br %in% br_post[2:length(br_post)])-1]
  
  
  # Plot
  g0_c <- ggplot() +
    geom_contour_fill(
      data = Ind_df,
      aes(x = T_av_summer, y = P_summ_tot, z = log(E0_app))
    ) +
    scale_fill_gradientn(
      colors = col_br_post, # Use the defined colors
      values = scales::rescale(log10(br_post)), # Rescale breaks for the log10 scale
      na.value = "#32003C" # Define the color for NA values
    )+
    ggtitle(paste0("Average E0, ", city_x))+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = E0),
                 color = "black", breaks = c(1))+
    theme_test()+
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color = "white", size = 2) +
    guides(size = "legend", colour = "none")+
    scale_color_grey()+
    geom_label_repel(data = point_df %>% filter(year ==  2004 | year ==  2023),
                     aes(x = T_av_summer, y = P_summ_tot, label = labE0),
                     label.padding = 0.15, segment.color = NA, size = 3) #size = 4
  
  #Ad
  breaks_Ad = seq(0.03, 30000, by = 500)
  
  g1_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = log10(Ad)))+
    scale_fill_viridis_c(limits = c(min(log10(breaks_Ad)), max(log10(breaks_Ad))),
                         na.value = "#32003C", option = "mako")+
    ggtitle(paste0("Average log10 adults/ha (May to Oct), " , city_x))+
    theme_test()+
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color = "white", size = 2) +
    guides(size = "legend", colour = "none")+
    scale_color_grey()+
    geom_label_repel(data = point_df %>% filter(year ==  2004 | year ==  2023),
                     aes(x = T_av_summer, y = P_summ_tot, label = labAd),
                     label.padding = 0.15,segment.color = NA, size = 3) #size = 4
  
  # #R0
  # breaks_R = c(0, 0.5, 1, 2, 4, 7, 10)
  # 
  # g2_c <- ggplot()+
  #   geom_contour_fill(data = Ind_df,
  #                     aes(x = T_av_summer, y = P_summ_tot, z = R0))+
  #   geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = R0),
  #                color = "red", breaks = c(1))+
  #   scale_fill_viridis_c(limits = c(min(breaks_R), max(breaks_R)),
  #                        na.value = "#32003C")+
  #   ggtitle("Average R0 (May to Oct)")+
  #   theme_test()+
  #   geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
  #   geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
  #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = P_summ_tot, label = year),
  #                    label.padding = 0.15) #size = 4
  
  
  breaks_nR = c(0, 1, 5, 10, 20, 40, 80, 120, 170)
  
  g3_c <- ggplot()+
    geom_contour_fill(data = Ind_df,
                      aes(x = T_av_summer, y = P_summ_tot, z = nR0))+
    scale_fill_viridis_c(limits = c(min(breaks_nR), max(breaks_nR )),
                         na.value = "#32003C")+
    ggtitle(paste0("n days with R0 >1, ", city_x))+
    geom_contour(data = Ind_df, aes(x = T_av_summer, y = P_summ_tot, z = nR0),
                 color = "red", breaks = c(1))+
    theme_test()+
    geom_path(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color= "white") +
    geom_point(data = point_df, aes(x = T_av_summer, y = P_summ_tot), color = "white", size = 2) +
    guides(size = "legend", colour = "none")+
    scale_color_grey()+
    geom_label_repel(data = point_df %>% filter(year == 2004 | year ==  2023),
                     aes(x = T_av_summer, y = P_summ_tot, label = labnR0),
                     label.padding = 0.15, segment.color = NA, size = 3) #size = 4
  
  
  # Save
  # g_tot <- ggarrange(g0_c, g1_c, g3_c, ncol = 1)
  
  ggsave(paste0(folder_plot, "NEW_wl_g", city_x ,"_E0_15.png"),
         g0_c +
           guides(size = "legend", fill = "none")+ 
           theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 plot.title = element_blank(),
                 plot.margin = unit(c(0.025, 0, 0, 0), "in"))+
                   scale_x_continuous(expand = c(0, 0))+
                   scale_y_continuous(expand = c(0, 0)),
         units="in", height=1.2, width= 3.6, dpi=300)
  
  ggsave(paste0(folder_plot, "NEW_wl_g", city_x ,"_Ad_15.png"),
         g1_c +
           guides(size = "legend", fill = "none")+ 
           theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 plot.title = element_blank(),
                 plot.margin = unit(c(0.025, 0, 0, 0), "in"))+
           scale_x_continuous(expand = c(0, 0))+
           scale_y_continuous(expand = c(0, 0)),
         units="in", height=1.2, width= 3.6, dpi=300)
  
  ggsave(paste0(folder_plot, "NEW_wl_g", city_x ,"_nR_15.png"),
         g3_c +
           guides(size = "legend", fill = "none")+ 
           theme(axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 plot.title = element_blank(),
                 plot.margin = unit(c(0.025, 0, 0, 0), "in"))+
           scale_x_continuous(expand = c(0, 0))+
           scale_y_continuous(expand = c(0, 0)),
         units="in", height=1.2, width= 3.6, dpi=300)
  
  # ggsave(paste0(folder_plot, "NEW_g", city_x ,"_nR_15.png"), g3_c + guides(size = "legend", fill = "none") + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_blank()), units="in", height=1.5, width= 3.5, dpi=300)
  
  # ggsave(paste0(folder_plot, "NEW_g", city_x ,".png"), g_tot, units="in", height=8, width= 5.5, dpi=300)
}



#  # VARIANCE-BASED (TO BE UPTDATED)
# 
# folder_plot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Exposure_state_pow/"
# 
# for (city_x in cities){
#   
#   load(paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather/Weather_EOBS_",
#               city_x, "_", min(years), "_", max(years), ".RData"))
#   
#   load(paste0(folder_plot, "Ind_", city_x,".RData"))
#   
#   point_df <- data.frame("name" = city_x,
#                          "year" = years_eval,
#                          "T_av_summer" = NA,
#                          "sdP_summer" = NA)
#   
#   # recc Didier: between 1 may and 1 oct
#   for(y in years_eval){
#     
#     point_df$T_av_summer[which(point_df$year == y)] <- W_tot_df %>% 
#       filter(year %in% c((y-3):y)) %>%
#       filter(DOY >= 121) %>% #1st may
#       filter(DOY < 274) %>% #1st of october
#       dplyr::summarize(T_av_m = mean(T_av)) %>%
#       pull(T_av_m)
#     
#     point_df$sdP_summer[which(point_df$year == y)] <- W_tot_df %>% 
#       filter(year %in% c((y-3):y)) %>%
#       filter(DOY >= 121) %>% #1st may
#       filter(DOY < 274) %>% #1st of october
#       dplyr::summarize(P_sd = sd(P)) %>%
#       pull(P_sd)
#   }
#   
#   
#   #Ad
#   breaks_E0 = c(0, 8)
#   
#   # g1 <- ggplot() +
#   #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = Ad ), breaks = breaks_Ad) +
#   #   ggtitle(paste0("Average log adults/ha (May to Sept) in ", city_x))+
#   #   theme_test()+ 
#   #   geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#   #                    label.padding = 0.15) #size = 4
#   
#   g0_c <- ggplot()+
#     geom_contour_fill(data = Ind_df,
#                       aes(x = T_av_summer, y = sdP_summer, z = E0))+
#     scale_fill_viridis_c(limits = c(min(breaks_E0), max(breaks_E0)),
#                          na.value = "#32003C")+
#     ggtitle(paste0("Average E0~, ", city_x))+
#     geom_contour(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = E0),
#                  color = "red", breaks = c(1))+
#     theme_test()+
#     geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#                      label.padding = 0.15) #size = 4
#   
#   
#   #Ad
#   breaks_Ad = seq(3, 30000, by = 500)
#   
#   # g1 <- ggplot() +
#   #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = Ad ), breaks = breaks_Ad) +
#   #   ggtitle(paste0("Average log adults/ha (May to Sept) in ", city_x))+
#   #   theme_test()+ 
#   #   geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#   #                    label.padding = 0.15) #size = 4
#   
#   g1_c <- ggplot()+
#     geom_contour_fill(data = Ind_df,
#                       aes(x = T_av_summer, y = sdP_summer, z = log10(Ad)))+
#     scale_fill_viridis_c(limits = c(min(log10(breaks_Ad)), max(log10(breaks_Ad))),
#                          na.value = "#32003C")+
#     ggtitle(paste0("Average log10 adults/ha (May to Sept)"))+
#     theme_test()+
#     geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#                      label.padding = 0.15) #size = 4
#   
#   #R0
#   breaks_R = c(0, 0.5, 1, 2, 4, 7, 10)
#   
#   # g2 <- ggplot()+
#   #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = R0), breaks = breaks_R)+
#   #   geom_contour(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = R0),
#   #                color = "red", breaks = c(1))+
#   #   ggtitle(paste0("Average R0 (May to Sept)"))+
#   #   theme_test()+ 
#   #   geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#   #                    label.padding = 0.15) #size = 4
#   
#   g2_c <- ggplot()+
#     geom_contour_fill(data = Ind_df,
#                       aes(x = T_av_summer, y = sdP_summer, z = R0))+
#     geom_contour(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = R0),
#                  color = "red", breaks = c(1))+
#     scale_fill_viridis_c(limits = c(min(breaks_R), max(breaks_R)),
#                          na.value = "#32003C")+
#     ggtitle("Average R0 (May to Sept)")+
#     theme_test()+
#     geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#                      label.padding = 0.15) #size = 4
#   
#   
#   breaks_nR = c(0, 1, 5, 10, 20, 50, 90, 150)
#   
#   # g3 <- ggplot()+
#   #   geom_contour_filled(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = nR0), breaks = breaks_nR)+
#   #   ggtitle(paste0("n days with R0 >1"))+
#   #   theme_test()+
#   #   geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "black") +
#   #   geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#   #                    label.padding = 0.15) #size = 4
#   
#   g3_c <- ggplot()+
#     geom_contour_fill(data = Ind_df,
#                       aes(x = T_av_summer, y = sdP_summer, z = nR0))+
#     scale_fill_viridis_c(limits = c(min(breaks_nR), max(breaks_nR )),
#                          na.value = "#32003C")+
#     ggtitle("n days with R0 >1")+
#     geom_contour(data = Ind_df, aes(x = T_av_summer, y = sdP_summer, z = nR0),
#                  color = "red", breaks = c(1))+
#     theme_test()+
#     geom_point(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_path(data = point_df, aes(x = T_av_summer, y = sdP_summer), color= "white") +
#     geom_label_repel(data = point_df, aes(x = T_av_summer, y = sdP_summer, label = year),
#                      label.padding = 0.15) #size = 4
#   
#   
#   # Save
#   g_tot <- ggarrange(g0_c, g1_c, g3_c, ncol = 1)
#   
#   ggsave(paste0(folder_plot, "NEW_g", city_x ,".png"), g_tot, units="in", height=8, width= 5.5, dpi=300)
# }
