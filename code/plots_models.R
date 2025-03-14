# Forest Plots for 6 of the fitted models

# Define models and theme ####
# Model 1: Outcome = death, propofol as a binary variable, calorie intake as >70% of target (binary)
model1 <- model_death_propDays_calsAbove70pct

# Model 2: Outcome = death, with propofol-calories and calorie intake as >70% of target (binary)
model2 <- model_death_propCals_calsAbove70pct

# Model 3: Outcome = discharge, propofol as a binary variable, calorie intake as >70% of target (binary)
model3 <- model_disc_propDays_calsAbove70pct

# Model 4: Outcome = discharge, with propofol-calories and calorie intake as >70% of target (binary)
model4 <- model_disc_propCals_calsAbove70pct

# Model 5: Outcome = discharge, propofol as a binary variable, calorie intake above 16 kcal/kg
model5 <- model_death_subgrp_int_propDays_calsAbove16

# Model 6: Outcome = discharge, propofol as a binary variable, calorie intake above 16 kcal/kg
model6 <- model_disc_subgrp_int_propDays_calsAbove16

# Custom ggplot theme for consistency in the plots
theme.adjusted <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5,
                                                   margin = margin(t = 5), size = 22),
                        axis.title.x = element_text(margin = margin(t = 20), size = 32), 
                        axis.text.y = element_text(hjust = 1, margin = margin(r = 10),
                                                   size = 22, angle = 0),
                        axis.title.y = element_text(margin = margin(r = 20), size = 32),
                        title = element_text(color = "black"),
                        plot.title = element_text(size = 28, color = "black",
                                                  face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 17, color = "black",
                                                     face = "italic"),
                        panel.grid.major = element_line(color = "darkgray", linewidth = 0.2), 
                        panel.grid.minor = element_line(color = "gray", linewidth  = 0.1),
                        plot.background = element_rect(fill = "white", color = NA))


# Function to create Forest Plots ####

# Translate and reword the Confounders for the labels
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
  "CalsAbove16kcalPerKG1" = "Kalorien > 16 kcal/kg",
  "AgeKat>65:Propofol1" = "Interaktion: (Alter > 65) & Propofol",
  "AgeKat>65" = "Alter > 65",
  "Propofol0:SexMale" = "Interaktion: Kein Propofol & Männlich",
  "Propofol1:SexMale" = "Interaktion: Propofol & Männlich"
)


generate_forest_plot <- function(model, plot_title, subgroup = FALSE) {
  plot_data <- gg_fixed(model)
  se <- sqrt(diag(model$Vp))
  coef_values <- model$coefficients
  model_tidy <- tidy(model, parametric = TRUE)
  
  # Remove Year factors from the plot data
  plot_data$data <- plot_data$data %>% 
    filter(!grepl("factor\\(Year\\)", variable))
  
  # Create a data frame of hazard ratios and confidence intervals
  results <- data.frame(
    variable = names(model$coefficients),
    coef = coef_values,
    coef_exp = exp(coef_values),
    ci_lower = exp(coef_values - 1.96 * se),
    ci_upper = exp(coef_values + 1.96 * se)
  )
  
  results <- left_join(results, model_tidy %>% select(term, p.value),
                       by = c("variable" = "term"))
  results <- results %>%
    mutate(significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ))
  
  # Remove spline terms, intercept, and Year factors
  results <- results %>%
    filter(!grepl("s\\(", variable)) %>%       
    filter(!grepl("Intercept", variable)) %>%  
    filter(!grepl("factor\\(Year\\)", variable))
  
  # If subgroup = TRUE, only Interactions
  if (subgroup) {
    results <- results %>%
      filter(variable %in% c("Propofol1:SexMale", "AgeKat>65:Propofol1"))
  }
  
  # Create the forest plot
  ggplot(results, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
    geom_pointrange(aes(color = ifelse(coef_exp < 1, "red", "steelblue"))) +
    geom_hline(yintercept = 1, linetype = "solid", color = "black") +  # Horizontal line at 1
    scale_color_identity(guide = "none") +
    geom_text(aes(y = ci_upper + 0.05, label = significance), size = 6, color = "black") +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
    scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot_data$data$variable]) +
    ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
    ggtitle(plot_title) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
      axis.title.x = element_text(hjust = 0.39, margin = margin(t = 20), size = 22), 
      axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
      axis.title.y = element_blank(),
      title = element_text(color = "black"),
      plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
      plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
      plot.background = element_rect(fill = "white", color = NA)
    )
}


# Creates Forest Plots ####

model1_frst <- generate_forest_plot(model1, "Forest Plot der Hazard Ratios (Event = Tod)")
model2_frst <- generate_forest_plot(model2, "Forest Plot der Hazard Ratios (Event = Tod)") # No difference here (PropCals as spline)
model3_frst <- generate_forest_plot(model3, "Forest Plot der Hazard Ratios (Event = Entlassung)")
model4_frst <- generate_forest_plot(model4, "Forest Plot der Hazard Ratios (Event = Entlassung)")
model5_frst_whole <- generate_forest_plot(model5, "Forest Plot der Hazard Ratios für Subgruppen
                                          (Event = Tod)", subgroup = FALSE)
model6_frst_whole <- generate_forest_plot(model6, "Forest Plot der Hazard Ratios für Subgruppen
                                          (Event = Entlassung)", subgroup = FALSE)
model5_frst_interactions <- generate_forest_plot(model5, "Forest Plot der Hazard Ratios für Subgruppen
                                                 (Event = Tod)", subgroup = TRUE)
model6_frst_interactions <- generate_forest_plot(model6, "Forest Plot der Hazard Ratios für Subgruppen
                                                 (Event = Entlassung)", subgroup = TRUE)


# Autosave helpers ####

model_plots <- list(model1_frst, model2_frst, model3_frst, model4_frst, model5_frst_whole,
                    model6_frst_whole, model5_frst_interactions, model6_frst_interactions)

model_plot_names <- c("model1_frst", "model2_frst", "model3_frst", "model4_frst",
                      "model5_frst_whole", "model6_frst_whole", "model5_frst_interactions",
                      "model6_frst_interactions")
