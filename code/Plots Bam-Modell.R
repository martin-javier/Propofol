# Plots für BAM-Model

# Visualization of the smooth effects 
plot(model_final)
# Selects only the variable BMI
plot(model_final, select = 2, se = TRUE)

ped_df <- ped_data %>% make_newdata(tend = unique(tend), Age = seq_range(Age, n = 3))

# Survival Probability Plot

# nach Alter
ped_df %>% group_by(Age) %>% add_surv_prob(model_final) %>%
  ggplot(aes(x = tend, y = surv_prob, ymax = surv_lower, ymin = surv_upper, group = Age)) +
  geom_line(aes(col = Age)) + geom_ribbon(aes(fill = Age), alpha = 0.2) +
  ylab(expression(hat(S)(t))) + xlab(expression(t))

# nach Anzahl von Propofol 
ped_df_propofol <- ped_data %>% make_newdata(tend = unique(tend), Days_Propofol = seq_range(Days_Propofol, n = 4))

ped_df_propofol %>% group_by(Days_Propofol) %>% add_surv_prob(model_final) %>% 
  ggplot(aes(x = tend, y = surv_prob, ymax = surv_lower, ymin = surv_upper, group = Days_Propofol)) +
  geom_line(aes(col = Days_Propofol)) + #+ geom_ribbon(aes(fill = Days_Propofol), alpha = 0.2) +
  ylab(expression(hat(S)(t))) + xlab(expression(t))

# Cumulative Hazard Plot

# nach Alter
ped_df %>%
  group_by(Age) %>%
  add_cumu_hazard(model_final) %>%
  ggplot(aes(x = tend, y = cumu_hazard, ymin = cumu_lower, ymax = cumu_upper, group = Age)) +
  geom_hazard(aes(col = Age)) + geom_ribbon(aes(fill = Age), alpha = 0.2) +
  ylab(expression(hat(Lambda)(t))) + xlab(expression(t))

# nach Anzahl von Propofol
ped_df_propofol %>% group_by(Days_Propofol) %>% add_cumu_hazard(model_final) %>% 
  ggplot(aes(x = tend, y = cumu_hazard, ymax = cumu_upper, ymin = cumu_lower, group = Days_Propofol)) +
  geom_hazard(aes(col = Days_Propofol)) + #+ geom_ribbon(aes(fill = Days_Propofol), alpha = 0.2) +
  ylab(expression(hat(S)(t))) + xlab(expression(t))
