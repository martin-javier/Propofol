# Analysis-Plots for 2 PAMMs used in presentation slides

# Define the models used for plotting:
# Model 1: Outcome = death, propofol as a binary variable, calorie intake as >70% of target (binary)
model1 <- model_death_propDays_calsAbove70pct

# Model 2: Outcome = discharge, with propofol-calories and calorie intake as >16 kcal/kg (binary)
model2 <- model_disc_propCals_calsAbove16

# Custom ggplot theme for consistency in the plots
theme.adjusted <- theme(
  axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
  axis.title.x = element_text(margin = margin(t = 20), size = 22), 
  axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
  axis.title.y = element_text(margin = margin(r = 20), size = 22),
  title = element_text(color = "black"),
  plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
  plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
  panel.grid.major = element_line(color = "black", linewidth = 0.1), 
  panel.grid.minor = element_line(color = "gray", linewidth  = 0.1),
  plot.background = element_rect(fill = "beige", color = NA)
)


# Spline-plots for Model 1 ####

## 1. Spline Age ####

# Extract the first smooth term (for s(Age))
smooth_age_1 <- model1$smooth[[1]]
# Create a sequence of Age values covering the observed range
x_age_1 <- seq(min(model1$model$Age), max(model1$model$Age), length.out = 100)
# Compute the design matrix for the smooth term s(Age)
X_age_1 <- PredictMat(smooth_age_1, data.frame(Age = x_age_1))

# Extract the coefficients for s(Age)
coef_indices_age_1 <- smooth_age_1$first.para:smooth_age_1$last.para
coef_values_age_1 <- model1$coefficients[coef_indices_age_1]

# Calculate the fitted values and standard errors for s(Age)
fitted_age_1 <- X_age_1 %*% coef_values_age_1
vcov_age_1 <- model1$Vp[coef_indices_age_1, coef_indices_age_1]  # Variance-covariance matrix
se_age_1 <- sqrt(rowSums((X_age_1 %*% vcov_age_1) * X_age_1))

# Create a data frame for plotting the smooth term for Age (transformed to Hazard Ratio scale)
smooth_age_df_1 <- data.frame(
  x = x_age_1,
  fit = exp(fitted_age_1),            # Exponentiate to get hazard ratios
  upper = exp(fitted_age_1 + 2 * se_age_1),  # Upper confidence interval
  lower = exp(fitted_age_1 - 2 * se_age_1)   # Lower confidence interval
)

# Plot for s(Age)
model1_sp_age <- ggplot(smooth_age_df_1, aes(x = x, y = fit)) +
  geom_line(color = "darkgreen", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "darkgreen") +
  scale_y_continuous(breaks = seq(0, 3.5, by = 0.5), limits = c(0, 3.5)) +
  scale_x_continuous(breaks = seq(20, 100, by = 10), limits = c(18, 105)) + 
  labs(title = "Glatter Term: Alter", x = "Alter",
       y = expression("Hazard Ratio  " * exp * " " * (hat(beta)))) +
  theme.adjusted


## 2. Spline BMI ####

# Extract the second smooth term (for BMI)
smooth_bmi_1 <- model1$smooth[[2]]
x_bmi_1 <- seq(min(model1$model$BMI), max(model1$model$BMI), length.out = 100)
X_bmi_1 <- PredictMat(smooth_bmi_1, data.frame(BMI = x_bmi_1))

# Extract coefficients for s(BMI)
coef_indices_bmi_1 <- smooth_bmi_1$first.para:smooth_bmi_1$last.para
coef_values_bmi_1 <- model1$coefficients[coef_indices_bmi_1]

# Calculate fitted values and standard errors for s(BMI)
fitted_bmi_1 <- X_bmi_1 %*% coef_values_bmi_1
vcov_bmi_1 <- model1$Vp[coef_indices_bmi_1, coef_indices_bmi_1]
se_bmi_1 <- sqrt(rowSums((X_bmi_1 %*% vcov_bmi_1) * X_bmi_1))

# Create a data frame for plotting the smooth term for BMI
smooth_bmi_df_1 <- data.frame(
  x = x_bmi_1,
  fit = exp(fitted_bmi_1),            # Exponentiate for Hazard Ratio
  upper = exp(fitted_bmi_1 + 2 * se_bmi_1),
  lower = exp(fitted_bmi_1 - 2 * se_bmi_1)
)

# Plot for s(BMI)
model1_sp_bmi <- ggplot(smooth_bmi_df_1, aes(x = x, y = fit)) +
  geom_line(color = "darkorchid3", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "darkorchid3") +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.7)) +
  scale_x_continuous(breaks = seq(10, 100, by = 10), limits = c(13, 110)) +
  labs(title = "Glatter Term: BMI", x = "BMI",
       y = expression("Hazard Ratio  " * exp * " " * (hat(beta)))) +
  theme.adjusted


## 3. Spline ApacheIIScore ####

# Extract the third smooth term (for S(ApacheIIScore))
smooth_apache_1 <- model1$smooth[[3]]
x_apache_1 <- seq(min(model1$model$ApacheIIScore), max(model1$model$ApacheIIScore), length.out = 100)
X_apache_1 <- PredictMat(smooth_apache_1, data.frame(ApacheIIScore = x_apache_1))

# Extract coefficients for s(ApacheIIScore)
coef_indices_apache_1 <- smooth_apache_1$first.para:smooth_apache_1$last.para
coef_values_apache_1 <- model1$coefficients[coef_indices_apache_1]

# Calculate fitted values and standard errors for s(ApacheIIScore)
fitted_apache_1 <- X_apache_1 %*% coef_values_apache_1
vcov_apache_1 <- model1$Vp[coef_indices_apache_1, coef_indices_apache_1]
se_apache_1 <- sqrt(rowSums((X_apache_1 %*% vcov_apache_1) * X_apache_1))

# Create a data frame for plotting the smooth term for ApacheIIScore
smooth_apache_df_1 <- data.frame(
  x = x_apache_1,
  fit = exp(fitted_apache_1),            # Exponentiate for Hazard Ratio
  upper = exp(fitted_apache_1 + 2 * se_apache_1),
  lower = exp(fitted_apache_1 - 2 * se_apache_1)
)

# Plot for s(ApacheIIScore)
model1_sp_apache <- ggplot(smooth_apache_df_1, aes(x = x, y = fit)) +
  geom_line(color = "deepskyblue3", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "deepskyblue3") +
  scale_y_continuous(breaks = seq(0, 6, by = 0.5), limits = c(0, 6.3)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 71)) +
  labs(title = "Glatter Term: ApacheIIScore", x = "ApacheIIScore",
       y = expression("Hazard Ratio  " * exp * " " * (hat(beta)))) +
  theme.adjusted



# Spline-plots for Model 2 ####

## 1. Spline Age ####
smooth_age_2 <- model2$smooth[[1]]
x_age_2 <- seq(min(model2$model$Age), max(model2$model$Age), length.out = 100)
X_age_2 <- PredictMat(smooth_age_2, data.frame(Age = x_age_2))

coef_indices_age_2 <- smooth_age_2$first.para:smooth_age_2$last.para
coef_values_age_2 <- model2$coefficients[coef_indices_age_2]

fitted_age_2 <- X_age_2 %*% coef_values_age_2
vcov_age_2 <- model2$Vp[coef_indices_age_2, coef_indices_age_2]
se_age_2 <- sqrt(rowSums((X_age_2 %*% vcov_age_2) * X_age_2))

smooth_age_df_2 <- data.frame(
  x = x_age_2,
  fit = exp(fitted_age_2),
  upper = exp(fitted_age_2 + 2 * se_age_2),
  lower = exp(fitted_age_2 - 2 * se_age_2)
)

# Plot for s(Age)
model2_sp_age <- ggplot(smooth_age_df_2, aes(x = x, y = fit)) +
  geom_line(color = "darkgreen", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "darkgreen") +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) +
  scale_x_continuous(breaks = seq(20, 100, by = 10), limits = c(18, 105)) + 
  labs(title = "Glatter Term: Alter", x = "Alter",
       y = expression("Hazard Ratio  " * exp * " " * (hat(beta)))) +
  theme.adjusted


## 2. Spline BMI ####
smooth_bmi_2 <- model2$smooth[[2]]
x_bmi_2 <- seq(min(model2$model$BMI), max(model2$model$BMI), length.out = 100)
X_bmi_2 <- PredictMat(smooth_bmi_2, data.frame(BMI = x_bmi_2))

coef_indices_bmi_2 <- smooth_bmi_2$first.para:smooth_bmi_2$last.para
coef_values_bmi_2 <- model2$coefficients[coef_indices_bmi_2]

fitted_bmi_2 <- X_bmi_2 %*% coef_values_bmi_2
vcov_bmi_2 <- model2$Vp[coef_indices_bmi_2, coef_indices_bmi_2]
se_bmi_2 <- sqrt(rowSums((X_bmi_2 %*% vcov_bmi_2) * X_bmi_2))

smooth_bmi_df_2 <- data.frame(
  x = x_bmi_2,
  fit = exp(fitted_bmi_2),
  upper = exp(fitted_bmi_2 + 2 * se_bmi_2),
  lower = exp(fitted_bmi_2 - 2 * se_bmi_2)
)

# Plot for s(BMI)
model2_sp_bmi <- ggplot(smooth_bmi_df_2, aes(x = x, y = fit)) +
  geom_line(color = "darkorchid3", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "darkorchid3") +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) +
  scale_x_continuous(breaks = seq(10, 100, by = 10), limits = c(13, 110)) +
  labs(title = "Glatter Term: BMI", x = "BMI",
       y = expression("Hazard Ratio  " * exp * " " * (hat(beta)))) +
  theme.adjusted


## 3. Spline ApacheIIScore ####
smooth_apache_2 <- model2$smooth[[3]]
x_apache_2 <- seq(min(model2$model$ApacheIIScore), max(model2$model$ApacheIIScore), length.out = 100)
X_apache_2 <- PredictMat(smooth_apache_2, data.frame(ApacheIIScore = x_apache_2))

coef_indices_apache_2 <- smooth_apache_2$first.para:smooth_apache_2$last.para
coef_values_apache_2 <- model2$coefficients[coef_indices_apache_2]

fitted_apache_2 <- X_apache_2 %*% coef_values_apache_2
vcov_apache_2 <- model2$Vp[coef_indices_apache_2, coef_indices_apache_2]
se_apache_2 <- sqrt(rowSums((X_apache_2 %*% vcov_apache_2) * X_apache_2))

smooth_apache_df_2 <- data.frame(
  x = x_apache_2,
  fit = exp(fitted_apache_2),
  upper = exp(fitted_apache_2 + 2 * se_apache_2),
  lower = exp(fitted_apache_2 - 2 * se_apache_2)
)

# Plot for s(ApacheIIScore)
model2_sp_apache <- ggplot(smooth_apache_df_2, aes(x = x, y = fit)) +
  geom_line(color = "deepskyblue3", size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "deepskyblue3") +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 71)) +
  labs(title = "Glatter Term: ApacheIIScore", x = "ApacheIIScore",
       y = expression("Hazard Ratio  " * exp * " " * (hat(beta)))) +
  theme.adjusted



# Forest Plots for Hazard Ratios ####

# Create a set of user-friendly labels for the variables
renamed_labels <- c(
  "ProteinBelow0.8GperKG1" = "Protein < 0,8 g/kg",
  "Propofol1" = "Propofol (binär)",
  "PropofolCal" = "Propofol-Kalorien",
  "ParenteralNut1" = "Parenterale Ernährung",
  "OralIntake1" = "Orale Aufnahme",
  "inMV1" = "Mechanische Beatmung",
  "factor(Year)2014" = "Jahr: 2014",
  "factor(Year)2013" = "Jahr: 2013",
  "factor(Year)2011" = "Jahr: 2011",
  "factor(Year)2009" = "Jahr: 2009",
  "factor(Year)2008" = "Jahr: 2008",
  "factor(Sex)Male" = "Geschlecht: männlich",
  "factor(LeadAdmDiag)Sepsis" = "Leitdiag.: Sepsis",
  "factor(LeadAdmDiag)Respiratory" = "Leitdiag.: Respiratorisch",
  "factor(LeadAdmDiag)Renal" = "Leitdiag.: Renal",
  "factor(LeadAdmDiag)Orthopedic/Trauma" = "Leitdiag.: Orthopädisch/Trauma",
  "factor(LeadAdmDiag)Neurologic" = "Leitdiag.: Neurologisch",
  "factor(LeadAdmDiag)Metabolic" = "Leitdiag.: Stoffwechsel",
  "factor(LeadAdmDiag)Gastrointestinal" = "Leitdiag.: Gastrointestinal",
  "factor(LeadAdmDiag)Cardio-Vascular" = "Leitdiag.: Kardiovaskulär",
  "factor(AdmCatID)Surgical/Emeregency" = "Aufnahmekat.: Nofall OP",
  "factor(AdmCatID)Surgical/Elective" = "Aufnahmekat.: Geplante OP",
  "CalsPercentageAbove701" = "Kalorien > 70%",
  "CalsAbove16kcalPerKG1" = "Kalorien > 16 kcal/kg"
)


## Model 1 Forest Plot ####

plot1 <- gg_fixed(model1)
se_1 <- sqrt(diag(model1$Vp))
coef_1 <- model1$coefficients

# Remove Year factors from the plot data
plot1$data <- plot1$data %>% 
  filter(!grepl("factor\\(Year\\)", variable))

# Create a data frame of hazard ratios and confidence intervals for Model 1
results_1 <- data.frame(
  variable = names(model1$coefficients),
  coef = coef_1,
  coef_exp = exp(coef_1),
  ci_lower = exp(coef_1 - 1.96 * se_1),
  ci_upper = exp(coef_1 + 1.96 * se_1)
)

# Remove spline terms, intercept, and Year factors
results_1 <- results_1 %>%
  filter(!grepl("s\\(", variable)) %>%       
  filter(!grepl("Intercept", variable)) %>%  
  filter(!grepl("factor\\(Year\\)", variable))

# Create the forest plot for Model 1
model1_frst <- ggplot(results_1, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
  scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot1$data$variable]) +
  ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
  ggtitle("Forest Plot der Hazard Ratios (Event = Tod)") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
    axis.title.x = element_text(margin = margin(t = 20), size = 22), 
    axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
    axis.title.y = element_blank(),
    title = element_text(color = "black"),
    plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
    plot.background = element_rect(fill = "beige", color = NA)
  )


# Model 2 Forest Plot ####

plot2 <- gg_fixed(model2)
se_2 <- sqrt(diag(model2$Vp))
coef_2 <- model2$coefficients

# Remove Year factors from the plot data
plot2$data <- plot2$data %>% 
  filter(!grepl("factor\\(Year\\)", variable))

# Create a data frame of hazard ratios and confidence intervals for Model 2
results_2 <- data.frame(
  variable = names(model2$coefficients),
  coef = coef_2,
  coef_exp = exp(coef_2),
  ci_lower = exp(coef_2 - 1.96 * se_2),
  ci_upper = exp(coef_2 + 1.96 * se_2)
)

# Remove spline terms, intercept, and Year factors
results_2 <- results_2 %>%
  filter(!grepl("s\\(", variable)) %>%       
  filter(!grepl("Intercept", variable)) %>%  
  filter(!grepl("factor\\(Year\\)", variable))

# Create the forest plot for Model 2
model2_frst <- ggplot(results_2, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
  scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot2$data$variable]) +
  ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
  ggtitle("Forest Plot der Hazard Ratios (Event = Entlassung)") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
    axis.title.x = element_text(margin = margin(t = 20), size = 22), 
    axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
    axis.title.y = element_blank(),
    title = element_text(color = "black"),
    plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
    plot.background = element_rect(fill = "beige", color = NA)
  )



# Autosave helpers ####

model_plots <- list(
  model1_sp_age, model1_sp_bmi, model1_sp_apache, model2_sp_age,
  model2_sp_bmi, model2_sp_apache, model1_frst, model2_frst
)
model_plot_names <- c(
  "model1_sp_age", "model1_sp_bmi", "model1_sp_apache", "model2_sp_age",
  "model2_sp_bmi", "model2_sp_apache", "model1_frst", "model2_frst"
)
