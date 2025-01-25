model1 <- model_death_propDays_calsAbove70pct
model2 <- model_disc_propCals_calsAbove16

library(mgcv)
library(ggplot2)

theme.adjusted <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
                        axis.title.x = element_text(margin = margin(t = 20), size = 22), 
                        axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
                        axis.title.y = element_text(margin = margin(r = 20), size = 22),
                        title = element_text(color = "black"),
                        plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
                        panel.grid.major = element_line(color = "black", linewidth = 0.1), 
                        panel.grid.minor = element_line(color = "gray", linewidth  = 0.1),
                        plot.background = element_rect(fill = "beige", color = NA))

# Glatte Terme manuell extrahieren und darstellen
# 1. Spline: s(Age)
smooth_age_1 <- model1$smooth[[1]]  # Erster glatter Term
x_age_1 <- seq(min(model1$model$Age), max(model1$model$Age), length.out = 100)  # Wertebereich der Eingabedaten
X_age_1 <- PredictMat(smooth_age_1, data.frame(Age = x_age_1))  # Basisfunktionen für s(Age)

# Koeffizienten für s(Age) extrahieren
coef_indices_age_1 <- smooth_age_1$first.para:smooth_age_1$last.para
coef_values_age_1 <- model1$coefficients[coef_indices_age_1]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_age_1 <- X_age_1 %*% coef_values_age_1
vcov_age_1 <- model1$Vp[coef_indices_age_1, coef_indices_age_1]  # Varianz-Kovarianz-Matrix
se_age_1 <- sqrt(rowSums((X_age_1 %*% vcov_age_1) * X_age_1))

smooth_age_df_1 <- data.frame(
  x = x_age_1,
  fit = fitted_age_1,
  upper = fitted_age_1 + 2 * se_age_1,
  lower = fitted_age_1 - 2 * se_age_1
)

# 2. Spline: s(BMI)
smooth_bmi_1 <- model1$smooth[[2]]  # Zweiter glatter Term
x_bmi_1 <- seq(min(model1$model$BMI), max(model1$model$BMI), length.out = 100)  # Wertebereich der Eingabedaten
X_bmi_1 <- PredictMat(smooth_bmi_1, data.frame(BMI = x_bmi_1))

# Koeffizienten für s(BMI) extrahieren
coef_indices_bmi_1 <- smooth_bmi_1$first.para:smooth_bmi_1$last.para
coef_values_bmi_1 <- model1$coefficients[coef_indices_bmi_1]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_bmi_1 <- X_bmi_1 %*% coef_values_bmi_1
vcov_bmi_1 <- model1$Vp[coef_indices_bmi_1, coef_indices_bmi_1]  # Varianz-Kovarianz-Matrix
se_bmi_1 <- sqrt(rowSums((X_bmi_1 %*% vcov_bmi_1) * X_bmi_1))

smooth_bmi_df_1 <- data.frame(
  x = x_bmi_1,
  fit = fitted_bmi_1,
  upper = fitted_bmi_1 + 2 * se_bmi_1,
  lower = fitted_bmi_1 - 2 * se_bmi_1
)

# 3. Spline: s(ApacheIIScore)
smooth_apache_1 <- model1$smooth[[3]]  # Dritter glatter Term
x_apache_1 <- seq(min(model1$model$ApacheIIScore), max(model1$model$ApacheIIScore), length.out = 100)
X_apache_1 <- PredictMat(smooth_apache_1, data.frame(ApacheIIScore = x_apache_1))

# Koeffizienten für s(ApacheIIScore) extrahieren
coef_indices_apache_1 <- smooth_apache_1$first.para:smooth_apache_1$last.para
coef_values_apache_1 <- model1$coefficients[coef_indices_apache_1]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_apache_1 <- X_apache_1 %*% coef_values_apache_1
vcov_apache_1 <- model1$Vp[coef_indices_apache_1, coef_indices_apache_1]  # Varianz-Kovarianz-Matrix
se_apache_1 <- sqrt(rowSums((X_apache_1 %*% vcov_apache_1) * X_apache_1))

smooth_apache_df_1 <- data.frame(
  x = x_apache_1,
  fit = fitted_apache_1,
  upper = fitted_apache_1 + 2 * se_apache_1,
  lower = fitted_apache_1 - 2 * se_apache_1
)

# Visualisierung mit ggplot2
# Plot für s(Age)
ggplot(smooth_age_df_1, aes(x = x, y = fit)) +
  geom_line(color = "blue", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5), limits = c(-2, 2)) +
  scale_x_continuous(breaks = seq(20, 100, by = 10), limits = c(18, 105)) + 
  labs(title = "Estimated Effect: Age", x = "Age", y = expression("Regression Coefficient  " * hat(beta))) +
  theme.adjusted

# Plot für s(BMI)
ggplot(smooth_bmi_df_1, aes(x = x, y = fit)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5), limits = c(-2.01, 2)) + 
  scale_x_continuous(breaks = seq(10, 100, by = 10), limits = c(13, 110)) + 
  geom_line(color = "green", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "green") +
  labs(title = "Estimated Effect: BMI", x = "BMI", y = expression("Regression Coefficient  " * hat(beta))) +
  theme.adjusted

# Plot für s(ApacheIIScore)
ggplot(smooth_apache_df_1, aes(x = x, y = fit)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5), limits = c(-2, 2)) + 
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 71)) + 
  geom_line(color = "red", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
  labs(title = "Estimated Effect: ApacheIIScore", x = "ApacheIIScore", y = expression("Regression Coefficient  " * hat(beta))) +
  theme.adjusted

# Glatte Terme manuell extrahieren und darstellen
# 1. Spline: s(Age)
smooth_age_2 <- model2$smooth[[1]]  # Erster glatter Term
x_age_2 <- seq(min(model2$model$Age), max(model2$model$Age), length.out = 100)  # Wertebereich der Eingabedaten
X_age_2 <- PredictMat(smooth_age_2, data.frame(Age = x_age_2))  # Basisfunktionen für s(Age)

# Koeffizienten für s(Age) extrahieren
coef_indices_age_2 <- smooth_age_2$first.para:smooth_age_2$last.para
coef_values_age_2 <- model2$coefficients[coef_indices_age_2]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_age_2 <- X_age_2 %*% coef_values_age_2
vcov_age_2 <- model2$Vp[coef_indices_age_2, coef_indices_age_2]  # Varianz-Kovarianz-Matrix
se_age_2 <- sqrt(rowSums((X_age_2 %*% vcov_age_2) * X_age_2))

smooth_age_df_2 <- data.frame(
  x = x_age_2,
  fit = fitted_age_2,
  upper = fitted_age_2 + 2 * se_age_2,
  lower = fitted_age_2 - 2 * se_age_2
)

# 2. Spline: s(BMI)
smooth_bmi_2 <- model2$smooth[[2]]  # Zweiter glatter Term
x_bmi_2 <- seq(min(model2$model$BMI), max(model2$model$BMI), length.out = 100)  # Wertebereich der Eingabedaten
X_bmi_2 <- PredictMat(smooth_bmi_2, data.frame(BMI = x_bmi_2))

# Koeffizienten für s(BMI) extrahieren
coef_indices_bmi_2 <- smooth_bmi_2$first.para:smooth_bmi_2$last.para
coef_values_bmi_2 <- model2$coefficients[coef_indices_bmi_2]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_bmi_2 <- X_bmi_2 %*% coef_values_bmi_2
vcov_bmi_2 <- model2$Vp[coef_indices_bmi_2, coef_indices_bmi_2]  # Varianz-Kovarianz-Matrix
se_bmi_2 <- sqrt(rowSums((X_bmi_2 %*% vcov_bmi_2) * X_bmi_2))

smooth_bmi_df_2 <- data.frame(
  x = x_bmi_2,
  fit = fitted_bmi_2,
  upper = fitted_bmi_2 + 2 * se_bmi_2,
  lower = fitted_bmi_2 - 2 * se_bmi_2
)

# 3. Spline: s(ApacheIIScore)
smooth_apache_2 <- model2$smooth[[3]]  # Dritter glatter Term
x_apache_2 <- seq(min(model2$model$ApacheIIScore), max(model2$model$ApacheIIScore), length.out = 100)
X_apache_2 <- PredictMat(smooth_apache_2, data.frame(ApacheIIScore = x_apache_2))

# Koeffizienten für s(ApacheIIScore) extrahieren
coef_indices_apache_2 <- smooth_apache_2$first.para:smooth_apache_2$last.para
coef_values_apache_2 <- model2$coefficients[coef_indices_apache_2]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_apache_2 <- X_apache_2 %*% coef_values_apache_2
vcov_apache_2 <- model2$Vp[coef_indices_apache_2, coef_indices_apache_2]  # Varianz-Kovarianz-Matrix
se_apache_2 <- sqrt(rowSums((X_apache_2 %*% vcov_apache_2) * X_apache_2))

smooth_apache_df_2 <- data.frame(
  x = x_apache_2,
  fit = fitted_apache_2,
  upper = fitted_apache_2 + 2 * se_apache_2,
  lower = fitted_apache_2 - 2 * se_apache_2
)

# Visualisierung mit ggplot2
# Plot für s(Age)
ggplot(smooth_age_df_2, aes(x = x, y = fit)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5), limits = c(-2, 2)) + 
  scale_x_continuous(breaks = seq(20, 100, by = 10), limits = c(18, 105)) + 
  geom_line(color = "blue", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(title = "Estimated Effect: Age", x = "Age", y = expression("Regression Coefficient  " * hat(beta))) +
  theme.adjusted

# Plot für s(BMI)
ggplot(smooth_bmi_df_2, aes(x = x, y = fit)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5), limits = c(-2, 2)) + 
  scale_x_continuous(breaks = seq(10, 100, by = 10), limits = c(13, 110)) + 
  geom_line(color = "green", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "green") +
  labs(title = "Estimated Effect: BMI", x = "BMI", y = expression("Regression Coefficient  " * hat(beta))) +
  theme.adjusted

# Plot für s(ApacheIIScore)
ggplot(smooth_apache_df_2, aes(x = x, y = fit)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5), limits = c(-2, 2)) + 
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 71)) + 
  geom_line(color = "red", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
  labs(title = "Estimated Effect: ApacheIIScore", x = "ApacheIIScore", y = expression("Regression Coefficient  " * hat(beta))) +
  theme.adjusted

renamed_labels <- c(
  "ProteinBelow0.8GperKG1" = "Protein < 0.8g/kg",
  "Propofol1" = "Propofol Days",
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
  "CalsAbove16kcalPerKG1" = "Calories > 16kcal/kg"
)
# Erstelle den Plot basierend auf model1
plot1 <- gg_fixed(model1)
se_1 <- sqrt(diag(model1$Vp))
coef_1 <- model1$coefficients

# Greife auf die Daten im Plot zu und filtere die Jahre heraus
plot1$data <- plot1$data %>% 
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne alle Variablen mit "factor(Year)"
# Berechnung der Hazard Ratios und Konfidenzintervalle
results_1 <- data.frame(
  variable = names(model1$coefficients),                  # Nur die parametrischen Variablen
  coef = coef_1,                                 # Log-Skala Koeffizienten
  coef_exp = exp(coef_1),                        # Hazard Ratios
  ci_lower = exp(coef_1 - 1.96 * se_1),            # Unteres Konfidenzintervall
  ci_upper = exp(coef_1 + 1.96 * se_1)             # Oberes Konfidenzintervall
)
# Entferne Splines, den Intercept und andere unerwünschte Variablen
results_1 <- results_1 %>%
  filter(!grepl("s\\(", variable)) %>%         # Entferne Splines
  filter(!grepl("Intercept", variable)) %>%    # Entferne den Intercept
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne spezifische Faktoren

# Plot mit den gefilterten Daten
ggplot(results_1, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange() +                                   # Punkte und CI-Balken
  coord_flip() +                                        # Flip für horizontales Layout 
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
  scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot1$data$variable]) +
  ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
  ggtitle("Forest Plot of Hazard Ratios (Fixed Effects)") +
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
  
          # Interpreation: Das Konfidenzintervall beschreibt die Unsicherheit der Schätzung. 
          # Wenn das CI 0 überschreitet, ist der Effekt statistisch nicht signifikant.


# Erstelle den Plot basierend auf model2
plot2 <- gg_fixed(model2)
se_2 <- sqrt(diag(model2$Vp))
coef_2 <- model2$coefficients

# Greife auf die Daten im Plot zu und filtere die Jahre heraus
plot2$data <- plot2$data %>% 
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne alle Variablen mit "factor(Year)"

# Berechnung der Hazard Ratios und Konfidenzintervalle
results_2 <- data.frame(
  variable = names(model2$coefficients),      # Nur die parametrischen Variablen
  coef = coef_2,                 # Log-Skala Koeffizienten
  coef_exp = exp(coef_2),        # Hazard Ratios
  ci_lower = exp(coef_2 - 1.96 * se_2), # Unteres Konfidenzintervall
  ci_upper = exp(coef_2 + 1.96 * se_2)  # Oberes Konfidenzintervall
)

# Entferne Splines, den Intercept und andere unerwünschte Variablen
results_2 <- results_2 %>%
  filter(!grepl("s\\(", variable)) %>%         # Entferne Splines
  filter(!grepl("Intercept", variable)) %>%    # Entferne den Intercept
  filter(!grepl("factor\\(Year\\)", variable)) # Entferne spezifische Faktoren

# Plot mit den gefilterten Daten
ggplot(results_2, aes(x = variable, y = coef_exp, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange() +                                   # Punkte und CI-Balken
  coord_flip() +                                        # Flip für horizontales Layout  
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits = c(0, 2.5)) + 
  scale_x_discrete(labels = renamed_labels[names(renamed_labels) %in% plot2$data$variable]) +
  ylab(expression("Hazard Ratio " * exp(hat(beta)))) +
  ggtitle("Forest Plot of Hazard Ratios (Fixed Effects)") +
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

