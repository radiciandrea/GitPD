# Little 2 pop epidemic model

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

n_s = 10 #number of seasons
l_s = 60 #length of season

# functions

theta_delta = rep(c(rep(0,l_s), rep(1,l_s)), n_s/2) # "hatching" is time dependent
theta_beta = theta_delta*rep(c(0,1,0,0.5), l_s*n_s/4) # infection mosquitoes is time dependent
# no vertical infection

# list with parameters to be passed to the ODE system
parms <- list(gamma = gamma,
              mu_j = mu_j,
              K = K,
              delta = delta,
              mu_a = mu_a,
              beta_AsI = beta_AsI,
              beta_SAi = beta_SAi,
              alpha = alpha) 

df <- function(t, x, parms) {
  
  # initial conditions and paramters
  with(parms, { 
    J <- x[1:(length(x)/5)]
    As <- x[(length(x)/5 +1):(2*length(x)/5)]
    Ai <- x[(2*length(x)/5+1):(3*length(x)/5)]
    S <- x[(3*length(x)/5+1):(4*length(x)/5)]
    I <- x[(4*length(x)/5+1):length(x)]
    
    # ODE definition 
    dJ = gamma*(As+Ai) - mu_j*J*(1+J/K) - delta*theta_delta[t[1]] *J
    dAs = delta*theta_delta[t[1]] *J - beta_AsI*theta_beta[t[1]]*As*I - mu_a*As
    dAi = beta_AsI*theta_beta[t[1]]*As*I - mu_a*Ai
    dS = - beta_AsI*S*Ai + alpha*I
    dI = beta_AsI*S*Ai - alpha*I
    
    dx <- c(dJ, dAs, dAi, dS, dI)
    
    return(list(dx))})
}

# System initialization
J_0 = 1500
As_0 = 500
Ai_0 = 0
S_0 = 0
I_0 = 0

l_sim = n_s*l_s

X_0 = c(J_0, As_0, Ai_0, S_0, I_0)

#integration
Sim <- as.data.frame(ode(X_0, 1:l_sim, df, parms))

#plot
colnames(Sim) = c("t", "J", "As", "Ai", "S", "I")
Sim_m = reshape2::melt(Sim, id = 't')

ggplot(Sim_m, aes(x = t, y = value, color = variable))+
  geom_point()

