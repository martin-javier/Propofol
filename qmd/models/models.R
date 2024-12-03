library(survival)
library(ggsurvplot)
library(ggplot2)
library(patchwork)


## model 1
cox_model <- coxph(
  Surv(Disc0To60, PatientDischarged) ~ Age + BMI + ApacheIIScore + Sex + LeadAdmDiag + AdmCat +
    Days_Propofol + DaysMechVent + Days_OralIntake + Days_ParNut,
  data = model_data
)


# Output the summary of the Cox model
summary(cox_model)

# Extract coefficients and confidence intervals
cox_summary <- summary(cox_model)
coefficients <- cox_summary$coefficients
conf_int <- cox_summary$conf.int

# Prepare data for plotting
plot_data <- data.frame(
  Variable = rownames(coefficients),
  HR = conf_int[, 1],
  Lower95 = conf_int[, 3],
  Upper95 = conf_int[, 4],
  PValue = coefficients[, 5]
)

# Create the forest plot
ggplot(plot_data, aes(x = reorder(Variable, HR), y = HR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower95, ymax = Upper95), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Hazard Ratios",
       x = "Kovariaten",
       y = "Hazard Ratio") +
  theme_minimal()

ggsave("HR.png")

## not working for another model with na.omit()
## Schoenfeld
# Generate Schoenfeld residuals
ph_test <- cox.zph(cox_model)

# Extract residuals for all covariates
schoenfeld_residuals <- as.data.frame(ph_test$residuals)

# Add time variable
schoenfeld_residuals$Time <- ph_test$x


# Pivot data into long format
schoenfeld_long <- pivot_longer(
  schoenfeld_residuals,
  cols = -Time,
  names_to = "Covariate",
  values_to = "Residual"
)

####  Martingale residuals ####
###############################
# Calculate Martingale residuals
martingale_resid <- residuals(cox_model, type = "martingale")

# Extract the dataset used in the Cox model
aligned_data <- model_data[rownames(model_data) %in% rownames(cox_model$y), ]

# Confirm alignment
nrow(aligned_data) == length(martingale_resid)  # Should return TRUE


# Define the variables to plot
variables <- c("Age", "BMI", "ApacheIIScore", "Days_Propofol", 
               "DaysMechVent", "Days_OralIntake", "Days_ParNut")

# Create a list to store the individual plots
plots <- list()

# Loop through each variable and create a plot
for (var in variables) {
  p <- ggplot(data.frame(Value = aligned_data[[var]], Residuals = martingale_resid),
              aes(x = Value, y = Residuals)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", color = "blue") +
    labs(title = paste(var),
         x = var,
         y = "Residuals") +
    theme_minimal()
  plots[[var]] <- p  # Store the plot in the list
}

# Combine all plots into a single visualization (2 rows, 3 columns)
combined_plot <- wrap_plots(plots, nrow = 4, ncol = 2)


# Display the combined plot
print(combined_plot)




####### dfbeta ######
#####################
# Calculate influence diagnostics
dfbeta <- residuals(cox_model, type = "dfbeta")
# Extract variable names
variable_names <- names(coef(cox_model))

# Assign names to dfbeta columns
colnames(dfbeta) <- variable_names
# Plot DFBETA for a specific covariate (e.g., Age)
ggplot(data.frame(Index = 1:nrow(dfbeta), DFBETA = dfbeta[, "Age"]),
       aes(x = Index, y = DFBETA)) +
  geom_point(alpha = 0.5) +
  labs(title = "DFBETA for Age",
       x = "Observation Index",
       y = "DFBETA (Age)") +
  theme_minimal()


####### deviance ###############
################################
# Calculate deviance residuals
deviance_resid <- residuals(cox_model, type = "deviance")

# Plot deviance residuals
ggplot(data.frame(Index = 1:length(deviance_resid), Residuals = deviance_resid),
       aes(x = Index, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  labs(title = "Deviance Residuals",
       x = "Observation Index",
       y = "Deviance Residuals") +
  theme_minimal()
