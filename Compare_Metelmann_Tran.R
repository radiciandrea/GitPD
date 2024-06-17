# Compare results for Bologna 2011-2022

# first run ModelTran with Bologna and 2011 2021
# Beware: 
# Carrying capacity need to be re-defined
# mortalities expressed as in arbocartoR 

Eggs_laid_sim_Tran_df <- data.frame(DOS = Sim$t[1:t_end],
                               eggs = gamma_Ao*(beta_1*Sim$A_1o + beta_2*Sim$A_2o),
                               type = "laid, simulated, Tran")

Eggs_laid_sim_cum_Tran_df <- Eggs_laid_sim_Tran_df
Eggs_laid_sim_cum_Tran_df$eggs = sapply(Eggs_laid_sim_cum_Tran_df$DOS,
                                        function(x){return(sum(Eggs_laid_sim_Tran_df$eggs[max(1, x-13):x]))})

# second run ModelMetelmann_params_in_solver.R

Eggs_laid_sim_Metelmann_df <- data.frame(DOS = Sim$t[d-t_s+1],
                               eggs = beta_approx[d]*Sim$A[d-t_s+1], #"all eggs, diapaused or not"
                               type = "laid, simulated, Metelmann")

Eggs_laid_sim_cum_Metelmann_df <- Eggs_laid_sim_Metelmann_df
Eggs_laid_sim_cum_Metelmann_df$eggs = sapply(Eggs_laid_sim_cum_Metelmann_df$DOS,
                                             function(x){return(sum(Eggs_laid_sim_Metelmann_df$eggs[max(1, x-13):x]))})

# consider eggs database
Eggs_df <- Eggs_tot_df %>%
  filter(region == region_x) %>%
  filter(DOS %in% Sim$t[d-t_s+1]) %>%
  select("DOS", "eggs", "type")


# join sims
Egg_comp_df <- rbind(Eggs_df, Eggs_laid_sim_Tran_df, Eggs_laid_sim_Metelmann_df)
Egg_comp_df <- rbind(Eggs_df, Eggs_laid_sim_cum_Tran_df, Eggs_laid_sim_cum_Metelmann_df)

#ignore 2011 for adjustment - comoure relative eggs
Egg_comp_df <- Egg_comp_df %>%
  filter(DOS > 365) %>%
  group_by(type)%>%
  mutate(relative_eggs_m = eggs/mean(eggs, na.rm = T))%>%
  mutate(relative_eggs_M = eggs/max(eggs, na.rm = T))%>%
  ungroup()

ggplot(Egg_comp_df, aes(x = DOS, y = relative_eggs_M, color = type))+
  geom_line(data = Egg_comp_df %>% filter(type != "observed"))+
  geom_point(data = Egg_comp_df %>% filter(type == "observed"))
