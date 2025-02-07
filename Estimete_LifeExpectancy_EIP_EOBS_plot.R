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
years = 2006:2023

#load shp
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/domain_sel_01_W_EU.shp") %>%
  arrange(region)
countries_sh <-  st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm/european-countries.shp")
n_r = nrow(domain)

#initialise matrices
LE_A_JAS_m = matrix(NA, nrow = length(years), ncol = n_r)
EIP_JAS_m = matrix(NA, nrow = length(years), ncol = n_r)
C_m = matrix(NA, nrow = length(years), ncol = n_r)
CP_JAS_m = matrix(NA, nrow = length(years), ncol = n_r)

for (year in years){
  
  #getting weather from EOBS 
  load(paste0(folder_eobs, "/EOBS_sel_", type, "_", year, "_", name, ".RData")) 
  
  #days :regions
  DOY = unique(W_tot_df$DOY)
  n_d = max(DOY)
  
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
  
  #probability that EIP < LE_A
  CP = 1-pexp(EIP, rate = mu_A)

  #Average EIP and LE_A in months in which there are cases (J-A-S)
  d_i = yday(paste0("01-07-", year))
  d_f = yday(paste0("01-10-", year))
  LE_A_JAS = colMeans(LE_A[d_i:d_f,])
  EIP_JAS = colMeans(EIP[d_i:d_f,])
  
  #Average (daily) fraction of mosquitoes that can survive EIP
  #this is independent of mosquito presence
  CP_JAS = colMeans(CP[d_i:d_f,])
  
  LE_A_JAS_m[which(years == year),] = LE_A_JAS 
  EIP_JAS_m[which(years == year),] = EIP_JAS 
  C_m[which(years == year),] = C 
  CP_JAS_m[which(years == year),] = CP_JAS 
  
}

# Average 2006-2014
years_1 = 2006:2014

LE_A_JAS_1 = colMeans(LE_A_JAS_m[years %in% years_1,])
EIP_JAS_1 = colMeans(EIP_JAS_m[years %in% years_1,])
C_1 = colMeans(C_m[years %in% years_1,])
CP_JAS_1 = colMeans(CP_JAS_m[years %in% years_1,])

g_LE_A_JAS_1 <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_A_JAS_1), colour = NA)+ #
  scale_fill_gradient(trans = "log")+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_EIP_JAS_1 <- ggplot()+
  geom_sf(data = domain, aes(fill = EIP_JAS_1), colour = NA)+ #
  scale_fill_gradient(trans = "log")+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_C_1 <- ggplot()+
  geom_sf(data = domain, aes(fill = C_1), colour = NA)+ #
  #scale_fill_manual(values = col_x)+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_CP_JAS_1 <- ggplot()+
  geom_sf(data = domain, aes(fill = CP_JAS_1), colour = NA)+ #
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

# Average 2015-2023

years_2 = 2015:2024

LE_A_JAS_2 = colMeans(LE_A_JAS_m[years %in% years_2,])
EIP_JAS_2 = colMeans(EIP_JAS_m[years %in% years_2,])
C_2 = colMeans(C_m[years %in% years_2,])
CP_JAS_2 = colMeans(CP_JAS_m[years %in% years_2,])

g_LE_A_JAS_2 <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_A_JAS_2), colour = NA)+ #
  scale_fill_gradient(trans = "log")+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_EIP_JAS_2 <- ggplot()+
  geom_sf(data = domain, aes(fill = EIP_JAS_2), colour = NA)+ #
  scale_fill_gradient(trans = "log")+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_C_2 <- ggplot()+
  geom_sf(data = domain, aes(fill = C_2), colour = NA)+ #
  #scale_fill_manual(values = col_x)+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_CP_JAS_2 <- ggplot()+
  geom_sf(data = domain, aes(fill = CP_JAS_2), colour = NA)+ #
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

# Average variation

LE_A_JAS_delta = LE_A_JAS_2 - LE_A_JAS_1
EIP_JAS_delta = EIP_JAS_2 - EIP_JAS_1
C_delta = C_2 - C_1
CP_JAS_delta = CP_JAS_2 - CP_JAS_1

g_LE_A_JAS_delta <- ggplot()+
  geom_sf(data = domain, aes(fill = LE_A_JAS_delta), colour = NA)+ #
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_EIP_JAS_delta <- ggplot()+
  geom_sf(data = domain, aes(fill = EIP_JAS_delta), colour = NA)+ #
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_C_delta <- ggplot()+
  geom_sf(data = domain, aes(fill = C_delta), colour = NA)+ #
  #scale_fill_manual(values = col_x)+
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()

g_CP_JAS_delta <- ggplot()+
  geom_sf(data = domain, aes(fill = CP_JAS_delta), colour = NA)+ #
  geom_sf(data = countries_sh, alpha = 0, colour = "white")+
  coord_sf(xlim = c(-15, 19), ylim = c(36, 60)) +
  ggtitle("")+
  theme_void()
