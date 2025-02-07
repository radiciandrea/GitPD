# code to compare

# average adult mosquito lifetime
# and
# average DENGUE EIP
# in Europe in recent years

library(dplyr)
library(pracma)
library(sf)
library(ggplot2)

#folders
folder_eobs = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/EOBS_elab"
folder_shp = ""

#load weather EOBS for each year: specifications
type = "01"
name = "W_EU"
years = 2005:2023

for (year in years){
  
  #getting weather from EOBS 
  load(paste0(folder_eobs, "/EOBS_sel_", type, "_", year, "_", name, ".RData")) 
  
  #days :regions
  DOY = unique(W_tot_df$DOY)
  n_d = max(DOY)
  n_r = nrow(W_tot_df)/n_d
  
  temp = matrix(W_tot_df$T_av, nrow = n_d)
  
  #estimate average life expentacy ofr adult mosquitoes LT:= 1/ma
  
  #Metelmann (well-known formula)
  mu_A = -log(0.677 * exp(-0.5*((temp-20.9)/13.2)^6)*temp^0.1) # adult mortality rate
  mu_A[which(temp<=0)] = Inf
  
  # #Tran (uhm apprently adult mosquitoes don't die if its too cold)
  # mu_A = 0.04417 + 0.00217*temp
  # mu_A[which(mu_A<0.02)] = 0.02 
  
  LE_A = 1/mu_A
  
  #esitmate EIP
  EIP = 1.03*(4*exp(5.15 - 0.123*temp)) #Caminade 2016 ("the Dengue one")

  #number of days in which EIP < LE_A (Compatibility with transmission)
  C = colSums(EIP<LE_A)

  #Average EIP and LE_A in months in which there are cases (J-A-S)
  d_i = yday(paste0("01-07-", year))
  d_f = yday(paste0("01-10-", year))
  LE_A_JAS = colSums(LE_A[d_i:d_f,])/(d_f-d_i+1)
  EIP_JAS = colSums(EIP[d_i:d_f,])/(d_f-d_i+1)
  
  
}

# to plot
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_01_W_EU.shp") %>%
  arrange(region)
countries_sh <-  st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/european-countries.shp")



g1 <- ggplot()+
  geom_sf(data = domain, aes(fill = C), colour = NA)+ #
  #scale_fill_manual(values = col_x)+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("C")+
  theme_void()

g2 <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_A_JAS), colour = NA)+ #
  scale_fill_gradient(trans = "log")+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("C")+
  theme_void()

g3 <- ggplot()+
  geom_sf(data = domain, aes(fill = EIP_JAS), colour = NA)+ #
  scale_fill_gradient(trans = "log")+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("C")+
  theme_void()

