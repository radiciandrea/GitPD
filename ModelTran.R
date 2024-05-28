# Modello Tran 2013

rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)

#parameters (Tran 2013)

beta_1 = 95 # Number of eggs laid by ovipositing nulliparous females (per female)
beta_2 = 75 # Number of eggs laid by ovipositing parous females (per female)
K_L = 250000 # Standard environment carrying capacity for larvae (larvae ha−1)
K_P = 250000 # Standard environment carrying capacity for pupae (pupae ha−1)
sigma = 0.5 # Sex-ratio at the emergence
mu_E = 0.05 # Egg mortality rate (day−1)
mu_L = 0.08 # Minimum larva mortality rate (day−1)
mu_P = 0.03 # Minimum pupa mortality rate (day−1)
mu_em = 0.1 # Mortality rate during adult emergence (day−1)
mu_A = 0.02 # Minimum adult mortality rate (day−1)
mu_r = 0.08 # Adult mortality rate related to seeking behavior (day−1)
T_E = 10.4 # Minimal temperature needed for egg development (°C)
TDD_E = 110 # Total number of degree-day necessary for egg development (°C)
gamma_Aem = 0.4 # Development rate of emerging adults (day−1)
gamma_Ah = 0.2 # Transition rate from host-seeking to engorged adults (day−1)
gamma_Ao = 0.2 # Transition rate from oviposition site-seeking to host-seeking adults (day−1)
T_Ag = 10 # Minimal temperature needed for egg maturation (°C)
TDD_Ag = 77 # Total number of degree-days necessary for egg maturation (°C)
t_s_diap = 31+28+10 # Start of the favorable season - 10 Mar (for diapause)
t_end_diap = 31+28+31+30+31+30+31+31+30 # End of the favorable season - 30 Sept (for diapause)

# temp = 15 - 13*cos(d/365*2*pi); # temperatura sinusoidale, min 1 gen = 2 gradi, max 1 lug = 28
# prec = temp*rep(c(0,0,0,1), length.out = (t_end-t_s+1)) # piove ogni 4 giorni con questa forma strana
# 
# #Getting T and P from Agroclim Climatik (Montpellier)
# library(readxl)
# W <- read_excel("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Agroclim/T_P_Montpellier_2000_2023_INRAE_STATION_34172005.xls", range = "A10:F8776")
# 
# temp = W$TM[7306 - 1 + d] # lets start from 1/1/2020
# prec = W$RR[7306 - 1 + d] 

#Getting T and P and Eggs from Arpae (see ReadNc+ARPAE) + nc by Cyril

load("C:/Users/Andrea/Desktop/Alcuni file permanenti/Post_doc/Dati/Eggs_Weather_ER_20112021.RData")

# chose a region and years
region_x = "BOLOGNA"
year_x = 2011:2021

W_df <- W_tot_df %>%
  filter(region == region_x) %>%
  filter(year %in% year_x )

temp <- W_df$T_av
prec <- W_df$P

# T and P
t_s = W_df$DOS[1] # simulate multiple year
t_end = tail(W_df$DOS, n = 1)
d = t_s:t_end

# p cumulated over 2 weeks and normalized between 0 and 1 (over a year?)
p_cumm = sapply(1:length(prec), function(x){return(sum(prec[max(1,x-13):x]))})
p_cumm_norm = p_cumm/max(p_cumm)

# functions

f_E = (temp-T_E)/TDD_E *(temp-T_E>0) # Transition function from egg to larva

# WARNING: this should probably depend on praceipitation as well

f_L = - 0.0007*temp^2 + 0.0392 * temp - 0.3911 # Transition function from larva to pupa

# WARNING 1: this function souhld be > 0
f_L = pmax(0, f_L)

f_P = 0.0008*temp^2 - 0.0051 * temp + 0.0319 # Transition function from pupa to emerging adult
f_Ag = (temp-T_Ag)/TDD_Ag *(temp-T_Ag>0) # Transition function from engorged adult to oviposition site—seeking adult
m_L = exp(-temp/0.5) + mu_L # Larva mortality (day−1)
m_P = exp(-temp/0.5) + mu_P # Pupa mortality rate (day−1)
m_A = pmax(mu_A, 0.04417 + 0.00217*temp) # Adult mortality rate (day−1)
k_L = K_L*(p_cumm_norm+1) # Environment carrying capacity of larvae (ha−1)
k_P = K_P*(p_cumm_norm+1) # Environment carrying capacity of pupae (ha−1)

# Warning: this should be corrected by the Area. Here we make the Hp A = 100 ha
k_L = k_L*100
k_P = k_P*100

z = 1*(d %% 365 > t_s_diap)*(d %% 365 < t_end_diap)

n_s = 1 # number of locations (added; 1 for no dimension)

# list with parameters to be passed to the ODE system
parms = list(beta_1 = beta_1,
             beta_2 = beta_2,
             K_L = K_L,
             K_P = K_P,
             sigma = sigma,
             mu_E = mu_E,
             mu_em = mu_em,
             mu_r = mu_r,
             gamma_Aem = gamma_Aem,
             gamma_Ah = gamma_Ah,
             gamma_Ao = gamma_Ao,
             f_E = f_E,
             f_L = f_L,
             f_P = f_P,
             f_Ag = f_Ag,
             m_L = m_L,
             m_P = m_P,
             m_A = m_A,
             k_L = k_L,
             k_P = k_P,
             n_s = n_s,
             t_s = t_s,
             z = z) 

df <- function(t, x, parms) {
  
  # initial conditions and paramters
  with(parms, { 
    
    E = x[(1+n_s*0):(1*n_s)]
    L = x[(1+n_s*1):(2*n_s)]
    P = x[(1+n_s*2):(3*n_s)]
    A_em = x[(1+n_s*3):(4*n_s)]
    A_1h = x[(1+n_s*4):(5*n_s)]
    A_1g = x[(1+n_s*5):(6*n_s)]
    A_1o = x[(1+n_s*6):(7*n_s)]
    A_2h = x[(1+n_s*7):(8*n_s)]
    A_2g = x[(1+n_s*8):(9*n_s)]
    A_2o = x[(1+n_s*9):(10*n_s)]
    
    t_n = t[1]-t_s+1 # time of numerical integration
    
    # ODE definition 
    dE = gamma_Ao*(beta_1*A_1o + beta_2*A_2o) - (mu_E + z[t_n]*f_E[t_n])*E
    dL = z[t_n]*f_E[t_n]*E - (m_L[t_n]*(1+L/k_L[t_n]) + f_L[t_n])*L
    dP = f_L[t_n]*L - (m_P[t_n] + f_P[t_n])*P
    dA_em = f_P[t_n]*P*sigma*exp(-mu_em*(1+P/k_P[t_n])) - (m_A[t_n]+gamma_Aem)*A_em
    dA_1h = gamma_Aem*A_em - (m_A[t_n] + mu_r + gamma_Ah)*A_1h
    dA_1g = gamma_Ah*A_1h - (m_A[t_n] + f_Ag[t_n])*A_1g
    dA_1o = f_Ag[t_n]*A_1g - (m_A[t_n] + mu_r + gamma_Ao)*A_1o
    dA_2h = gamma_Ao*(A_1o + A_2o) - (m_A[t_n] + mu_r + gamma_Ah)*A_2h
    dA_2g = gamma_Ah*A_2h - (m_A[t_n] + f_Ag[t_n])*A_2g
    dA_2o = f_Ag[t_n]*A_2g - (m_A[t_n] + mu_r + gamma_Ao)*A_2o
    
    dx <- c(dE, dL, dP, dA_em, dA_1h, dA_1g, dA_1o, dA_2h, dA_2g, dA_2o)
    
    return(list(dx))})
}

# System initialization
E0 = 10^6 # at 1st of January
L0 = 0
P0 = 0
A_em0 = 0
A_1h0 = 0
A_1g0 = 0
A_1o0 = 0
A_2h0 = 0
A_2g0 = 0
A_2o0 = 0

X_0 = c(E0, L0, P0, A_em0, A_1h0, A_1g0, A_1o0, A_2h0, A_2g0, A_2o0)

#integration
Sim <- as.data.frame(ode(X_0, d, df, parms))

#plot
colnames(Sim) = c("t", "E", "L", "P", "A_em", "A_1h",
                  "A_1g", "A_1o", "A_2h", "A_2g", "A_2o")
Sim_m = reshape2::melt(Sim, id = 't')

ggplot(Sim_m, aes(x = t, y = value, color = variable))+
  geom_point()

#Simulated eggs (eq 4)

Eggs_laid_sim_df <- data.frame(DOY = Sim$t[1:t_end],
                               Eggs = gamma_Ao*(beta_1*Sim$A_1o + beta_2*Sim$A_2o),
                               type = "laid, simulated")

#plot

Eggs_df <- Eggs_tot_df %>%
  filter(region == "BOLOGNA") %>%
  filter(year == 2018) %>%
  select("eggs", "type")

Eggs_df$

Egg_comp_df <- rbind(Eggs_laid_sim_df, Eggs_df)

Egg_comp_df <- Egg_comp_df %>%
  group_by(type)%>%
  mutate(Relative_eggs = Eggs/max(Eggs, na.rm = T))%>%
  ungroup()

ggplot(data = Egg_comp_df, aes(x = DOY, y = Relative_eggs, color = type))+
  geom_line()+
  geom_point()
             
             