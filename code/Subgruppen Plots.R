# # Subgruppen Forest Plots
# 
# # Subgruppen
# # Frauen, Death, Propofol/PropofolCal
# summary(model_death_female_propDays_calsAbove16) #niedrigster AIC von den Frauen
# #summary(model_death_female_propCals_calsAbove16)
# #"model_death_female_propDays_calsAbove16"
# 
# 
# # Frauen, Discharged, Propofol/PropofolCal
# #summary(model_disc_female_propDays_calsAbove70pct)
# #summary(model_disc_female_propCals_calsAbove16)
# 
# # Ältere Patienten über 65, Death, Propofol/PropofolCal
# summary(model_death_older65_propDays_calsAbove70pct) # niedrigster AIC von den Älter 65
# #summary(model_death_older65_propCals_calsAbove70pct)
# #"model_death_older65_propDays_calsAbove70pct"
# 
# 
# # Ältere Patienten über 65, Discharged, Propofol/PropofolCal
# #summary(model_disc_older65_propDays_calsAbove70pct)
# #summary(model_disc_older65_propCals_calsAbove70pct)
# 
# 
# 
# # Berechnung von den kleinsten AICs pro Subgruppe
# # Modelle für Frauen
# aic_female_death_days <- AIC(model_death_female_propDays_calsAbove16)
# aic_female_death_cals <- AIC(model_death_female_propCals_calsAbove16)
# aic_female_disc_days <- AIC(model_disc_female_propDays_calsAbove70pct)
# aic_female_disc_cals <- AIC(model_disc_female_propCals_calsAbove16)
# 
# # Modelle für Ältere Patienten über 65
# aic_older65_death_days <- AIC(model_death_older65_propDays_calsAbove70pct)
# aic_older65_death_cals <- AIC(model_death_older65_propCals_calsAbove70pct)
# aic_older65_disc_days <- AIC(model_disc_older65_propDays_calsAbove70pct)
# aic_older65_disc_cals <- AIC(model_disc_older65_propCals_calsAbove70pct)
# 
# 
# # Für Frauen
# aic_female <- c(
#   model_death_female_propDays_calsAbove16 = AIC(model_death_female_propDays_calsAbove16),
#   model_death_female_propCals_calsAbove16 = AIC(model_death_female_propCals_calsAbove16),
#   model_disc_female_propDays_calsAbove70pct = AIC(model_disc_female_propDays_calsAbove70pct),
#   model_disc_female_propCals_calsAbove16 = AIC(model_disc_female_propCals_calsAbove16)
# )
# 
# # Für Ältere Patienten über 65
# aic_older65 <- c(
#   model_death_older65_propDays_calsAbove70pct = AIC(model_death_older65_propDays_calsAbove70pct),
#   model_death_older65_propCals_calsAbove70pct = AIC(model_death_older65_propCals_calsAbove70pct),
#   model_disc_older65_propDays_calsAbove70pct = AIC(model_disc_older65_propDays_calsAbove70pct),
#   model_disc_older65_propCals_calsAbove70pct = AIC(model_disc_older65_propCals_calsAbove70pct)
# )
# 
# # Für Frauen
# lowest_aic_female <- names(sort(aic_female)[1:2])
# 
# # Für Ältere Patienten über 65
# lowest_aic_older65 <- names(sort(aic_older65)[1:2])
# 
# # Zeige die Modellnamen an
# lowest_aic_female
# lowest_aic_older65


model_sub_female <- model_death_female_propDays_calsAbove16
model_sub_age <- model_death_age_int_propDays_calsAbove16
model_sub_interaction <- model_disc_subgrp_int_propDays_calsAbove16

library(mgcv)
library(ggplot2)

renamed_labels <- c(
  "ProteinBelow0.8GperKG1" = "Protein < 0.8g/kg",
  "Propofol1" = "Propofol Binary",
  "PropofolCal" = "Propofol Calories",
  "ParenteralNut1" = "Parenteral Nutrition",
  "OralIntake1" = "Oral Intake",
  "inMV1" = "Mechanical Ventilation",
  "factor(Year)2014" = "Year: 2014",
  "factor(Year)2013" = "Year: 2013",
  "factor(Year)2011" = "Year: 2011",
  "factor(Year)2009" = "Year: 2009",
  "factor(Year)2008" = "Year: 2008",
  "factor(Sex)Male" = "Sex: Male",
  "factor(LeadAdmDiag)Sepsis" = "LeadAdmDiag: Sepsis",
  "factor(LeadAdmDiag)Respiratory" = "LeadAdmDiag: Respiratory",
  "factor(LeadAdmDiag)Renal" = "LeadAdmDiag: Renal",
  "factor(LeadAdmDiag)Orthopedic/Trauma" = "LeadAdmDiag: Orthopedic/Trauma",
  "factor(LeadAdmDiag)Neurologic" = "LeadAdmDiag: Neurologic",
  "factor(LeadAdmDiag)Metabolic" = "LeadAdmDiag: Metabolic",
  "factor(LeadAdmDiag)Gastrointestinal" = "LeadAdmDiag: Gastrointestinal",
  "factor(LeadAdmDiag)Cardio-Vascular" = "LeadAdmDiag: Cardio-Vascular",
  "factor(AdmCatID)Surgical/Emeregency" = "AdmCat: Surgical/Emergency",
  "factor(AdmCatID)Surgical/Elective" = "AdmCat: Surgical/Elective",
  "CalsPercentageAbove701" = "Calories > 70%",
  "CalsAbove16kcalPerKG1" = "Calories > 16kcal/kg",
  "AgeKat>65:Propofol1" = "Interaction: (Age > 65) & Propofol",
  "AgeKat>65" = "Age > 65",
  "Propofol0:SexMale" = "Interaction: No Propofol & Male",
  "Propofol1:SexMale" = "Interaction: Propofol & Male"
)
# Erstelle den Plot basierend auf model_sub_female

plot_female <- gg_fixed(model_sub_female)
se_female <- sqrt(diag(model_sub_female$Vp))
coef_female <- model_sub_female$coefficients

# Greife auf die Daten im Plot zu und filtere die Jahre heraus
plot_female$data <- plot_female$data %>% 
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne alle Variablen mit "factor(Year)"
# Berechnung der Hazard Ratios und Konfidenzintervalle
results_female <- data.frame(
  variable = names(model_sub_female$coefficients),                  # Nur die parametrischen Variablen
  coef = coef_female,                                 # Log-Skala Koeffizienten
  coef_exp = exp(coef_female),                        # Hazard Ratios
  ci_lower = exp(coef_female - 1.96 * se_female),            # Unteres Konfidenzintervall
  ci_upper = exp(coef_female + 1.96 * se_female)             # Oberes Konfidenzintervall
)
# Entferne Splines, den Intercept und andere unerwünschte Variablen
results_female <- results_female %>%
  filter(!grepl("s\\(", variable)) %>%         # Entferne Splines
  filter(!grepl("Intercept", variable)) %>%    # Entferne den Intercept
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne spezifische Faktoren

# Plot mit den gefilterten Daten
ggplot(results_female, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange() +                                   # Punkte und CI-Balken
  coord_flip() +                                        # Flip für horizontales Layout 
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
  scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot_female$data$variable]) +
  ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
  ggtitle("Forest Plot of Hazard Ratios (Female)") +
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

# Erstelle den Plot basierend auf model_sub_age

plot_age <- gg_fixed(model_sub_age)
se_age <- sqrt(diag(model_sub_age$Vp))
coef_age <- model_sub_age$coefficients

# Greife auf die Daten im Plot zu und filtere die Jahre heraus
plot_age$data <- plot_age$data %>% 
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne alle Variablen mit "factor(Year)"
# Berechnung der Hazard Ratios und Konfidenzintervalle
results_age <- data.frame(
  variable = names(model_sub_age$coefficients),                  # Nur die parametrischen Variablen
  coef = coef_age,                                 # Log-Skala Koeffizienten
  coef_exp = exp(coef_age),                        # Hazard Ratios
  ci_lower = exp(coef_age - 1.96 * se_age),            # Unteres Konfidenzintervall
  ci_upper = exp(coef_age + 1.96 * se_age)             # Oberes Konfidenzintervall
)
# Entferne Splines, den Intercept und andere unerwünschte Variablen
results_age <- results_age %>%
  filter(!grepl("s\\(", variable)) %>%         # Entferne Splines
  filter(!grepl("Intercept", variable)) %>%    # Entferne den Intercept
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne spezifische Faktoren

# Plot mit den gefilterten Daten
ggplot(results_age, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange() +                                   # Punkte und CI-Balken
  coord_flip() +                                        # Flip für horizontales Layout 
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
  scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot_age$data$variable]) +
  ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
  ggtitle("Forest Plot of Hazard Ratios (Age <= 65)") +
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

# Erstelle den Plot basierend auf model_sub_interaction

plot_interaction <- gg_fixed(model_sub_interaction)
se_interaction <- sqrt(diag(model_sub_interaction$Vp))
coef_interaction <- model_sub_interaction$coefficients

# Greife auf die Daten im Plot zu und filtere die Jahre heraus
plot_interaction$data <- plot_interaction$data %>% 
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne alle Variablen mit "factor(Year)"
# Berechnung der Hazard Ratios und Konfidenzintervalle
results_interaction <- data.frame(
  variable = names(model_sub_interaction$coefficients),                  # Nur die parametrischen Variablen
  coef = coef_interaction,                                 # Log-Skala Koeffizienten
  coef_exp = exp(coef_interaction),                        # Hazard Ratios
  ci_lower = exp(coef_interaction - 1.96 * se_interaction),            # Unteres Konfidenzintervall
  ci_upper = exp(coef_interaction + 1.96 * se_interaction)             # Oberes Konfidenzintervall
)
# Entferne Splines, den Intercept und andere unerwünschte Variablen
results_interaction <- results_interaction %>%
  filter(!grepl("s\\(", variable)) %>%         # Entferne Splines
  filter(!grepl("Intercept", variable)) %>%    # Entferne den Intercept
  filter(!grepl("factor\\(Year\\)", variable)) %>%    # Entferne den Intercept
  filter(variable != "Propofol0:SexMale") # Entferne spezifische Faktoren

# Plot mit den gefilterten Daten
ggplot(results_interaction, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange() +                                   # Punkte und CI-Balken
  coord_flip() +                                        # Flip für horizontales Layout 
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
  scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot_interaction$data$variable]) +
  ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
  ggtitle("Forest Plot of Hazard Ratios (Discharged)") +
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
