# Analysis-Plots for 2 PAMMs used in Report

# Define the models used for plotting:
# Model 1: Outcome = death, propofol as a binary variable, calorie intake as >70% of target (binary)
model1 <- model_death_propDays_calsAbove70pct

# Model 2: Outcome = death, with propofol-calories and calorie intake as >70% of target (binary)
model2 <- model_death_propCals_calsAbove70pct

# Model 3: Outcome = discharge, propofol as a binary variable, calorie intake as >70% of target (binary)
model3 <- model_disc_propDays_calsAbove70pct

# Model 4: Outcome = discharge, with propofol-calories and calorie intake as >70% of target (binary)
model4 <- model_disc_propCals_calsAbove70pct

# Custom ggplot theme for consistency in the plots
theme.adjusted <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 22),
                        axis.title.x = element_text(margin = margin(t = 20), size = 32), 
                        axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 22, angle = 0),
                        axis.title.y = element_text(margin = margin(r = 20), size = 32),
                        title = element_text(color = "black"),
                        plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
                        panel.grid.major = element_line(color = "darkgray", linewidth = 0.2), 
                        panel.grid.minor = element_line(color = "gray", linewidth  = 0.1),
                        plot.background = element_rect(fill = "white", color = NA))

create_spline_plot <- function(model, variable_name, color = "deepskyblue3", 
                               x_limit = NULL, y_limit = c(0, 2.5), 
                               x_breaks = seq(0, 100, by = 10), 
                               y_breaks = seq(0, 2.5, by = 0.5), 
                               variable_label = variable_name) {
  # Find the corresponding smooth term
  smooth_index <- which(sapply(model$smooth, function(s) s$term == variable_name))
  
  if (length(smooth_index) == 0) {
    stop(paste("Variable", variable_name, "was not found as a smooth term in the model."))
  }
  
  smooth_term <- model$smooth[[smooth_index]]
  
  # Create the prediction matrix for the spline
  x_values <- seq(min(model$model[[variable_name]], na.rm = TRUE), 
                  max(model$model[[variable_name]], na.rm = TRUE), 
                  length.out = 100)
  
  # Set default x_limit if not provided
  if (is.null(x_limit)) {
    x_limit <- range(x_values)
  }
  
  pred_data <- setNames(data.frame(x_values), variable_name)  # Correctly create the dataframe
  X_mat <- PredictMat(smooth_term, pred_data)
  
  # Extract coefficient indices and values
  coef_indices <- smooth_term$first.para:smooth_term$last.para
  coef_values <- model$coefficients[coef_indices]
  
  # Compute expected values and standard errors
  fitted_values <- X_mat %*% coef_values
  vcov_matrix <- model$Vp[coef_indices, coef_indices]
  se_values <- sqrt(rowSums((X_mat %*% vcov_matrix) * X_mat))
  
  # Create dataframe for plotting
  smooth_df <- data.frame(
    x = x_values,
    fit = exp(fitted_values),
    upper = exp(fitted_values + 2 * se_values),
    lower = exp(fitted_values - 2 * se_values)
  )
  
  # Generate the plot
  ggplot(smooth_df, aes(x = x, y = fit)) +
    geom_line(color = color, size = 1.5) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = color) +
    geom_hline(yintercept = 1, color = "black", linetype = "dashed", size = 1) +
    scale_y_continuous(breaks = y_breaks, limits = y_limit) +
    scale_x_continuous(breaks = x_breaks, limits = x_limit) +
    labs(title = paste("Glatter Term:", variable_label), x = variable_label,
         y = expression("Hazard Ratio  " * exp * " " * (hat(beta)))) +
    theme.adjusted
}



# Model 1
model1_sp_age <- create_spline_plot(model1, "Age", color = "darkgreen", x_limit = c(18, 102), variable_label = "Alter",
                   y_limit = c(0, 3.5), y_breaks = seq(0, 3.5, 0.5))

model1_sp_bmi <- create_spline_plot(model1, "BMI", color = "darkorchid3", x_limit = c(13, 109), y_limit = c(0, 2.6))

model1_sp_apacheII <- create_spline_plot(model1, "ApacheIIScore", color = "deepskyblue3", x_limit = c(0, 72), 
                   y_limit = c(0, 7), y_breaks = seq(0, 7, 0.5))


# Model 2 
model2_sp_age <- create_spline_plot(model2, "Age", color = "darkgreen", x_limit = c(18, 102), variable_label = "Alter",
                   y_limit = c(0, 3.5), y_breaks = seq(0, 3.5, 0.5))

model2_sp_bmi <- create_spline_plot(model2, "BMI", color = "darkorchid3", x_limit = c(13, 109), y_limit = c(0, 2.6))

model2_sp_apacheII <- create_spline_plot(model2, "ApacheIIScore", color = "deepskyblue3", x_limit = c(0, 72), 
                   y_limit = c(0, 7), y_breaks = seq(0, 7, 0.5))

model2_sp_propofolCal <- create_spline_plot(model2, "PropofolCal", color = "orange", x_limit = c(0, 5500), x_breaks = seq(0, 5500, by = 500), 
                   y_limit = c(0, 5), y_breaks = seq(0, 5, by = 0.5), variable_label = "Propofol Kalorien")


# Model 3
model3_sp_age <- create_spline_plot(model3, "Age", color = "darkgreen", x_limit = c(18, 102), variable_label = "Alter",
                   y_limit = c(0, 3.5), y_breaks = seq(0, 3.5, 0.5))

model3_sp_bmi <- create_spline_plot(model3, "BMI", color = "darkorchid3", x_limit = c(13, 109), y_limit = c(0, 2.6))

model3_sp_apacheII <- create_spline_plot(model3, "ApacheIIScore", color = "deepskyblue3", x_limit = c(0, 72), 
                   y_limit = c(0, 7), y_breaks = seq(0, 7, 0.5))


# Model 4
model4_sp_age <- create_spline_plot(model4, "Age", color = "darkgreen",x_limit = c(18, 102), variable_label = "Alter", 
                   y_limit = c(0, 2.6))
model4_sp_bmi <- create_spline_plot(model4, "BMI", color = "darkorchid3", x_limit = c(13, 109), y_limit = c(0, 2.6))

model4_sp_apacheII <- create_spline_plot(model4, "ApacheIIScore", color = "deepskyblue3", x_limit = c(0, 72), 
                   y_limit = c(0, 7), y_breaks = seq(0, 7, 0.5))

model4_sp_propofolCal <- create_spline_plot(model4, "PropofolCal", color = "orange", x_limit = c(0, 5500), x_breaks = seq(0, 5500, by = 500), 
                   y_limit = c(0, 5), y_breaks = seq(0, 5, by = 0.5), variable_label = "Propofol Kalorien")
      
model_splines <- list(model1_sp_age, model1_sp_bmi, model1_sp_apacheII,
                   model2_sp_age, model2_sp_bmi, model2_sp_apacheII, model2_sp_propofolCal,
                   model3_sp_age, model3_sp_bmi, model3_sp_apacheII,
                   model4_sp_age, model4_sp_bmi, model4_sp_apacheII, model4_sp_propofolCal)

model_splines_names <- c(
  "model1_sp_age", "model1_sp_bmi", "model1_sp_apacheII",
  "model2_sp_age", "model2_sp_bmi", "model2_sp_apacheII", "model2_sp_propofolCal",
  "model3_sp_age", "model3_sp_bmi", "model3_sp_apacheII",
  "model4_sp_age", "model4_sp_bmi", "model4_sp_apacheII", "model4_sp_propofolCal"
)
             