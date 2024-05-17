# Little 2 pop epidemic model

library(deSolve)
library(ggplot2)
library(reshape2) 

#parameters

gamma = 2 #fertility rate mosquitoes: 4 mosquitoes/day, sex ratio 1:1
mu_j = 1/30 #mortality rate eggs/juveniles
K = 1000  # carrying capacity juveniles
delta = 0.5 # passage J to A
mu_a = 1/50 # mortality rate mosquitoes
beta_AsI = 0.1 # mosquito infectious rate
beta_SAi = 0.1  # human infectious rate
alpha = 1/14 #human recovery rate

# functions

theta_delta = c(rep(0,60), rep(1,60), rep(0,60), rep(1,60)) # "hatching" is time dependent
theta_beta = theta_delta*rep(c(0,1,0,0.5), 60) # infection mosquitoes is time dependent
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
    dAi = beta_AsI*theta_beta[t[1]]*As*I - mu_a*As
    dS = - beta_AsI*S*Ai + alpha*I
    dI = beta_AsI*S*Ai - alpha*I
    
    dx <- c(dJ, dAs, dAi, dS, dI)
    
    return(list(dx))})
}

# System initialization
J_0 = 500
As_0 = 20
Ai_0 = 50
S_0 = 200
I_0 = 0

l_sim = 60*4

X_0 = c(J_0, As_0, Ai_0, S_0, I_0)

#integration
Sim <- as.data.frame(ode(X_0, 1:l_sim, df, parms))

#plot
colnames(Sim) = c("t", "J", "As", "Ai", "S", "I")
Sim_m = reshape2::melt(Sim, id = 't')

ggplot(Sim_m, aes(x = t, y = value, color = variable))+
         geom_point()

       