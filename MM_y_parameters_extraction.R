# Model Metelmann
# recomputer parms at each year

prec_y = prec[id_d_y,]
temp_M_y = temp_M[id_d_y,]
temp_m_y = temp_m[id_d_y,]

#parameters (Metelmann 2019)
sigma_y = sigma[id_d_y,]
mu_A_y = mu_A[id_d_y,]
omega_y = omega[id_d_y,]

gamma_y = apply(gamma[id_d_y,], 2, function(x){min(x)})

# Compute K
K_y = sapply(1:n_r, function(y){return(lambda * (1-alpha_evap)/(1 - alpha_evap^DOY_y)*
                                       sapply(DOY_y, function(x){return(sum(alpha_evap^(x:1-1) * (alpha_dens*prec_y[1:x,y] + alpha_rain*H[y])))}))
}) 

# Compute h
h_y = h[id_d_y,]

# list with parameters to be passed to the ODE system
parms = list(omega = omega_y,
             h = h_y,
             K = K_y,
             mu_A = mu_A_y,
             delta_E = delta_E,
             sigma = sigma_y,
             gamma = gamma_y,
             temp_M = temp_M_y,
             temp_m = temp_m_y)
