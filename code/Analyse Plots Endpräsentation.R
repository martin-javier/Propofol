model1 <- model_death_propDays_calsAbove70pct_reICU
model2 <- model_disc_propCals_calsAbove16_reICU

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

# Das Modell ist bereits definiert als `model1`
# Formel: ped_status ~ s(Age, bs = "ps") + s(BMI, bs = "ps") + s(ApacheIIScore, bs = "ps") + ...

# Glatte Terme manuell extrahieren und darstellen
# 1. Spline: s(Age)
smooth_age <- model1$smooth[[1]]  # Erster glatter Term
x_age <- seq(min(model1$model$Age), max(model1$model$Age), length.out = 100)  # Wertebereich der Eingabedaten
X_age <- PredictMat(smooth_age, data.frame(Age = x_age))  # Basisfunktionen für s(Age)

# Koeffizienten für s(Age) extrahieren
coef_indices_age <- smooth_age$first.para:smooth_age$last.para
coef_values_age <- model1$coefficients[coef_indices_age]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_age <- X_age %*% coef_values_age
vcov_age <- model1$Vp[coef_indices_age, coef_indices_age]  # Varianz-Kovarianz-Matrix
se_age <- sqrt(rowSums((X_age %*% vcov_age) * X_age))

smooth_age_df_1 <- data.frame(
  x = x_age,
  fit = fitted_age,
  upper = fitted_age + 2 * se_age,
  lower = fitted_age - 2 * se_age
)

# 2. Spline: s(BMI)
smooth_bmi <- model1$smooth[[2]]  # Zweiter glatter Term
x_bmi <- seq(min(model1$model$BMI), max(model1$model$BMI), length.out = 100)  # Wertebereich der Eingabedaten
X_bmi <- PredictMat(smooth_bmi, data.frame(BMI = x_bmi))

# Koeffizienten für s(BMI) extrahieren
coef_indices_bmi <- smooth_bmi$first.para:smooth_bmi$last.para
coef_values_bmi <- model1$coefficients[coef_indices_bmi]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_bmi <- X_bmi %*% coef_values_bmi
vcov_bmi <- model1$Vp[coef_indices_bmi, coef_indices_bmi]  # Varianz-Kovarianz-Matrix
se_bmi <- sqrt(rowSums((X_bmi %*% vcov_bmi) * X_bmi))

smooth_bmi_df_1 <- data.frame(
  x = x_bmi,
  fit = fitted_bmi,
  upper = fitted_bmi + 2 * se_bmi,
  lower = fitted_bmi - 2 * se_bmi
)

# 3. Spline: s(ApacheIIScore)
smooth_apache <- model1$smooth[[3]]  # Dritter glatter Term
x_apache <- seq(min(model1$model$ApacheIIScore), max(model1$model$ApacheIIScore), length.out = 100)
X_apache <- PredictMat(smooth_apache, data.frame(ApacheIIScore = x_apache))

# Koeffizienten für s(ApacheIIScore) extrahieren
coef_indices_apache <- smooth_apache$first.para:smooth_apache$last.para
coef_values_apache <- model1$coefficients[coef_indices_apache]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_apache <- X_apache %*% coef_values_apache
vcov_apache <- model1$Vp[coef_indices_apache, coef_indices_apache]  # Varianz-Kovarianz-Matrix
se_apache <- sqrt(rowSums((X_apache %*% vcov_apache) * X_apache))

smooth_apache_df_1 <- data.frame(
  x = x_apache,
  fit = fitted_apache,
  upper = fitted_apache + 2 * se_apache,
  lower = fitted_apache - 2 * se_apache
)

# Visualisierung mit ggplot2
# Plot für s(Age)
ggplot(smooth_age_df_1, aes(x = x, y = fit)) +
  geom_line(color = "blue", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(title = "Geschätzter Spline-Effekt: Alter", x = "Age", y = "Schätzwerte") +
  theme.adjusted

# Plot für s(BMI)
ggplot(smooth_bmi_df_1, aes(x = x, y = fit)) +
  geom_line(color = "green", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "green") +
  labs(title = "Geschätzter Spline-Effekt: BMI", x = "BMI", y = "Schätzwerte") +
  theme.adjusted

# Plot für s(ApacheIIScore)
ggplot(smooth_apache_df_1, aes(x = x, y = fit)) +
  geom_line(color = "red", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
  labs(title = "Geschätzter Spline-Effekt: ApacheIIScores", x = "ApacheIIScore", y = "Schätzwerte") +
  theme.adjusted

# Das Modell ist bereits definiert als `model2`
# Formel: ped_status ~ s(Age, bs = "ps") + s(BMI, bs = "ps") + s(ApacheIIScore, bs = "ps") + ...

# Glatte Terme manuell extrahieren und darstellen
# 1. Spline: s(Age)
smooth_age <- model2$smooth[[1]]  # Erster glatter Term
x_age <- seq(min(model2$model$Age), max(model2$model$Age), length.out = 100)  # Wertebereich der Eingabedaten
X_age <- PredictMat(smooth_age, data.frame(Age = x_age))  # Basisfunktionen für s(Age)

# Koeffizienten für s(Age) extrahieren
coef_indices_age <- smooth_age$first.para:smooth_age$last.para
coef_values_age <- model2$coefficients[coef_indices_age]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_age <- X_age %*% coef_values_age
vcov_age <- model2$Vp[coef_indices_age, coef_indices_age]  # Varianz-Kovarianz-Matrix
se_age <- sqrt(rowSums((X_age %*% vcov_age) * X_age))

smooth_age_df_2 <- data.frame(
  x = x_age,
  fit = fitted_age,
  upper = fitted_age + 2 * se_age,
  lower = fitted_age - 2 * se_age
)

# 2. Spline: s(BMI)
smooth_bmi <- model2$smooth[[2]]  # Zweiter glatter Term
x_bmi <- seq(min(model2$model$BMI), max(model2$model$BMI), length.out = 100)  # Wertebereich der Eingabedaten
X_bmi <- PredictMat(smooth_bmi, data.frame(BMI = x_bmi))

# Koeffizienten für s(BMI) extrahieren
coef_indices_bmi <- smooth_bmi$first.para:smooth_bmi$last.para
coef_values_bmi <- model2$coefficients[coef_indices_bmi]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_bmi <- X_bmi %*% coef_values_bmi
vcov_bmi <- model2$Vp[coef_indices_bmi, coef_indices_bmi]  # Varianz-Kovarianz-Matrix
se_bmi <- sqrt(rowSums((X_bmi %*% vcov_bmi) * X_bmi))

smooth_bmi_df_2 <- data.frame(
  x = x_bmi,
  fit = fitted_bmi,
  upper = fitted_bmi + 2 * se_bmi,
  lower = fitted_bmi - 2 * se_bmi
)

# 3. Spline: s(ApacheIIScore)
smooth_apache <- model2$smooth[[3]]  # Dritter glatter Term
x_apache <- seq(min(model2$model$ApacheIIScore), max(model2$model$ApacheIIScore), length.out = 100)
X_apache <- PredictMat(smooth_apache, data.frame(ApacheIIScore = x_apache))

# Koeffizienten für s(ApacheIIScore) extrahieren
coef_indices_apache <- smooth_apache$first.para:smooth_apache$last.para
coef_values_apache <- model2$coefficients[coef_indices_apache]

# Geschätzte Werte und Konfidenzintervalle berechnen
fitted_apache <- X_apache %*% coef_values_apache
vcov_apache <- model2$Vp[coef_indices_apache, coef_indices_apache]  # Varianz-Kovarianz-Matrix
se_apache <- sqrt(rowSums((X_apache %*% vcov_apache) * X_apache))

smooth_apache_df_2 <- data.frame(
  x = x_apache,
  fit = fitted_apache,
  upper = fitted_apache + 2 * se_apache,
  lower = fitted_apache - 2 * se_apache
)

# Visualisierung mit ggplot2
# Plot für s(Age)
ggplot(smooth_age_df_2, aes(x = x, y = fit)) +
  geom_line(color = "blue", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(title = "Geschätzter Spline-Effekt: Alter", x = "Age", y = "Schätzwerte") +
  theme.adjusted

# Plot für s(BMI)
ggplot(smooth_bmi_df_2, aes(x = x, y = fit)) +
  geom_line(color = "green", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "green") +
  labs(title = "Geschätzter Spline-Effekt: BMI", x = "BMI", y = "Schätzwerte") +
  theme.adjusted

# Plot für s(ApacheIIScore)
ggplot(smooth_apache_df_2, aes(x = x, y = fit)) +
  geom_line(color = "red", size = 2.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
  labs(title = "Geschätzter Spline-Effekt: ApacheIIScores", x = "ApacheIIScore", y = "Schätzwerte") +
  theme.adjusted



renamed_labels <- c(
  "ProteinBelow0.8GperKG1" = "Protein < 0.8g/kg",
  "Propofol1" = "Propofol",
  "ParenteralNut1" = "Parenteral Nutrition",
  "OralIntake1" = "Oral Intake",
  "inMV1" = "inMV",
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
  "factor(AdmCatID)Surgical/Emeregency" = "AdmCatID: Surgical/Emergency",
  "factor(AdmCatID)Surgical/Elective" = "AdmCatID: Surgical/Elective",
  "CalsPercentageAbove701" = "Calories > 70%"
)

# Forest Plot für Modelle 1 und 2
# x <- as.data.frame(tidy_fixed(model1)[1]) 
# variables <- c("ProteinBelow0.8GperKG1", "Propofol1", "ParenteralNut1", "OralIntake1",
#                "inMV1", "factor(Sex)Male", "factor(LeadAdmDiag)Sepsis", "factor(LeadAdmDiag)Respiratory", 
#                "factor(AdmCatID)Surgical/Elective", "factor(AdmCatID)Surgical/Emeregency",
#                "factor(LeadAdmDiag)Cardio-Vascular", "factor(LeadAdmDiag)Respiratory", 
#                "factor(LeadAdmDiag)Gastrointestinal", "factor(LeadAdmDiag)Neurologic",      
#                "factor(LeadAdmDiag)Sepsis", "factor(LeadAdmDiag)Orthopedic/Trauma",
#                "factor(LeadAdmDiag)Metabolic", "factor(LeadAdmDiag)Renal", "CalsPercentageAbove701")
#  x <- x %>% filter(variable %in% variables)
tidy_fixed(model1)
plot1 <- gg_fixed(model1)
plot1 + 
  scale_x_discrete(labels = renamed_labels) + 
  ggtitle("Forrest Plot of fixed Coefficients") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
        axis.title.x = element_text(margin = margin(t = 20), size = 22), 
        axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
        axis.title.y = element_text(margin = margin(r = 20), size = 22),
        title = element_text(color = "black"),
        plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
        plot.background = element_rect(fill = "beige", color = NA))

plot2 <- gg_fixed(model2)
plot2 + ggtitle("Forrest Plot of fixed Coefficients") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
        axis.title.x = element_text(margin = margin(t = 20), size = 22), 
        axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
        axis.title.y = element_text(margin = margin(r = 20), size = 22),
        title = element_text(color = "black"),
        plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
        plot.background = element_rect(fill = "beige", color = NA))
