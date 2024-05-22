# Modello Tran 2013

library(deSolve)
library(ggplot2)
library(reshape2) 

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
t_s = 31+28+10 # Start of the favorable season - 10 Mar
t_end = 31+28+31+30+31+30+31+31+30 # End of the favorable season - 30 Sept

# T and P
d = 1:365
t = 15 - 13*cos(d/365*2*pi); # temperatura sinusoidale, min 1 gen = 2 gradi, max 1 lug = 28
p = t*c(0, rep(c(0,0,0,1), 91)) # piove ogni 4 giorni con questa forma strana

# p cumulated over 2 weeks and normalized between 0 and 1 (over a year?)
p_cumm = sapply(1:365, function(x){return(sum(p[max(1,x-13):x]))})
p_cumm_norm = p_cumm/max(p_cumm)

# functions

f_E = (t-T_E)/TDD_E *(t-T_E>0) # Transition function from egg to larva
f_L = - 0.0007*t^2 + 0.0392 * t - 0.3911 # Transition function from larva to pupa
f_P = 0.0008*t^2 - 0.0051 * t + 0.0319 # Transition function from pupa to emerging adult
f_Ag = (t-T_Ag)/TDD_Ag *(t-T_Ag>0) # Transition function from engorged adult to oviposition site—seeking adult
m_L = exp(-t/0.5) + mu_L # Larva mortality (day−1)
m_P = exp(-t/0.5) + mu_P # Pupa mortality rate (day−1)
m_A = max(mu_A, 0.04417 + 0.00217*t) # Adult mortality rate (day−1)
k_L = K_L*(p_cumm_norm+1) # Environment carrying capacity of larvae (ha−1)
k_P = K_P*(p_cumm_norm+1) # Environment carrying capacity of pupae (ha−1)


# list with parameters to be passed to the ODE system
df <- function(t, x) {
  
  # initial conditions and paramters
  E0 = x[,1]
  L0 = x[,2]
  P0 = x[,3]
  A_em0 = x[,4]
  A_1h0 = x[,5]
  A_1g0 = x[,6]
  A_1o0 = x[,7]
  A_2h0 = x[,8]
  A_2g0 = x[,9]
  A_2o0 = x[,10]
    
    # ODE definition 
    dE = gamma_Ao*(beta_1*A_1o + beta_2*A_2o) - (mu_E + f_E[t[1]])*E
    dL = f_E[t[1]]*E - (m_L(1+L/k_L[t[1]]) + f_L[t[1]])*L
    dP = f_L[t[1]]*L - (m_P[t[1]] + f_P[t[1]])*P
    dA_em = f_P[t[1]]*P*sigma*exp(-mu_em*(1+P/k_[t[1]])) - (m_A[t[1]]+gamma_Aem)*A_em
    dA_1h = gamma_Aem*A_em - (m_A[t[1]] + mu_r + gamma_Ah)*A_1h
    dA_1g = gamma_Ah*A_1h - (m_A[t[1]] + f_Ag)*A_1g
    dA_1o = f_Ag[t[1]]*A_1g - (m_A[t[1]] + mu_r + gamma_Ao)*A_1o
    dA_2h = gamma_Ao*(A_1o + A_2o) - (m_A[t[1]] + mu_r + gamma_Ah)*A_2h
    dA_2g = gamma_Ah*A_2h - (m_A[t[1]] + f_Ag)*A_2g
    dA_2o = f_Ag[t[1]]*A_2g - (m_A[t[1]] + mu_r + gamma_Ao)*A_2o
    
    dx <- c(dE, dL, dP, dA_em, dA_1h, dA_1g, dA_1o, dA_2h, dA_2g, dA_2o)
    
    return(list(dx))
}

# System initialization
E0 = max(k_P)
L0 = 0
P0 = 0
A_em0 = 0
A_1h0 = 0
A_1g0 = 0
A_1o0 = 0
A_2h0 = 0
A_2g0 = 0
A_2o0 = 0

l_sim = 365

X_0 = cbind(E0, L0, P0, A_em0, A_1h0, A_1g0, A_1o0, A_2h0, A_2g0, A_2o0)

#integration
Sim <- as.data.frame(ode(X_0, 1:l_sim, df))

#plot
colnames(Sim) = c("t", "J", "As", "Ai", "S", "I")
Sim_m = reshape2::melt(Sim, id = 't')

ggplot(Sim_m, aes(x = t, y = value, color = variable))+
  geom_point()

