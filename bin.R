# Little 2 pop epidemic model

#parameters

gamma = 2 #fertility rate mosquitoes: 4 mosquitoes/day, sex ratio 1:1
mu_j = 1/30 #mortality rate eggs/juveniles
K = 1000  # carrying capacity juveniles
delta = 0.5 # passage J to A
mu_a = 1/50 # mortality rate mosquitoes
beta_AsI = 0.1 # mosquito infectious rate
beta_SAi = 0.1  # human infectious rate
alfa = 1/14 #human recovery rate

# functions

theta_delta = c(rep(0,60), rep(1,60), rep(0,60), rep(1,60))
theta_beta = theta_delta*rep(c(0,1,0,0.5), 60)

# list with parameters to be passed to the ODE system
parms <- list(gamma = gamma,
              mu_j = mu_j,
              K = K,
              delta = delta,
              mu_a = mu_a,
              beta_AsI = beta_AsI,
              beta_SAi = beta_SAi,
              alfa = alfa) 

