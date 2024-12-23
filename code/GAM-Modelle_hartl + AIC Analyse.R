# Modell Vorgabe Prof. Hartl ####

library(dplyr)
library(pammtools)
library(mgcv)

## Patient Died ####
# prepare data
data_death <- data_summed_Day0To7 %>%
  mutate(daysToEvent = if_else(PatientDischarged == 1, 61L, daysToEvent))

### Propofol Days ####
# with Calorie Variable: Days where Calories were lower than 16 kcal/kg
ped_death <- as_ped(
  data = data_death,
  Surv(daysToEvent, PatientDied) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)

model_death_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    Days_Propofol + 
    Days_CalsBelow16kcalPerKG + 
    Days_ProtBelow0.8GperKG,
  data = ped_death,
  family = poisson(),
  offset = offset
)

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
model_death_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    Days_Propofol + 
    Days_CalsPercentageBelow70 + 
    Days_ProtBelow0.8GperKG,
  data = ped_death,
  family = poisson(),
  offset = offset
)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were lower than 16 kcal/kg
model_death_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    totalPropofolCal + 
    Days_CalsBelow16kcalPerKG + 
    Days_ProtBelow0.8GperKG,
  data = ped_death,
  family = poisson(),
  offset = offset
)

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
model_death_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    totalPropofolCal + 
    Days_CalsPercentageBelow70 + 
    Days_ProtBelow0.8GperKG,
  data = ped_death,
  family = poisson(),
  offset = offset
)

## Patient Discharged ####
# prepare data
data_disc <- data_summed_Day0To7 %>%
  mutate(daysToEvent = if_else(PatientDied == 1, 61L, daysToEvent))

### Propofol Days ####
# with Calorie Variable: Days where Calories were lower than 16 kcal/kg
ped_disc <- as_ped(
  data = data_disc,
  Surv(daysToEvent, PatientDischarged) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)

model_disc_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    Days_Propofol + 
    Days_CalsBelow16kcalPerKG + 
    Days_ProtBelow0.8GperKG,
  data = ped_disc,
  family = poisson(),
  offset = offset
)

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
model_disc_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    Days_Propofol + 
    Days_CalsPercentageBelow70 + 
    Days_ProtBelow0.8GperKG,
  data = ped_disc,
  family = poisson(),
  offset = offset
)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were lower than 16 kcal/kg
model_disc_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    totalPropofolCal + 
    Days_CalsBelow16kcalPerKG + 
    Days_ProtBelow0.8GperKG,
  data = ped_disc,
  family = poisson(),
  offset = offset
)

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
model_disc_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 10) + 
    s(BMI, bs = "ps", k = 10) + 
    s(ApacheIIScore, bs = "ps", k = 10) + 
    s(inMV0To7, bs = "ps", k = 10) + 
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    Days_OralIntake +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + 
    totalPropofolCal + 
    Days_CalsPercentageBelow70 + 
    Days_ProtBelow0.8GperKG,
  data = ped_disc,
  family = poisson(),
  offset = offset
)

# Summary der Modelle
summary(model_death_propDays_16kcal)
summary(model_death_propDays_70pct)
summary(model_death_propCals_16kcal)
summary(model_death_propCals_70pct)
summary(model_disc_propDays_16kcal)
summary(model_disc_propDays_70pct)
summary(model_disc_propCals_16kcal)
summary(model_disc_propCals_70pct)

# Berechnung der Modellgüte für alle Modelle
scores <- list(
  propDays_16kcal_died = AIC(model_death_propDays_16kcal),
  propDays_70pct_died = AIC(model_death_propDays_70pct),
  propCals_16kcal_died = AIC(model_death_propCals_16kcal),
  propCals_70pct_died = AIC(model_death_propCals_70pct),
  propDays_16kcal_disc = AIC(model_disc_propDays_16kcal),
  propDays_70pct_disc = AIC(model_disc_propDays_70pct),
  propCals_16kcal_disc = AIC(model_disc_propCals_16kcal),
  propCals_70pct_disc = AIC(model_disc_propCals_70pct)
)

# Beste Modelle auswählen
best_propDays_died <- ifelse(which.min(c(scores$propDays_16kcal_died, scores$propDays_70pct_died)) == 1, "model_death_propDays_16kcal", "model_death_propDays_70pct")
best_propCals_died <- ifelse(which.min(c(scores$propCals_16kcal_died, scores$propCals_70pct_died)) == 1, "model_death_propCals_16kcal", "model_death_propCals_70pct")

best_propDays_disc <- ifelse(which.min(c(scores$propDays_16kcal_disc, scores$propDays_70pct_disc)) == 1, "model_disc_propDays_16kcal", "model_disc_propDays_70pct")
best_propCals_disc <- ifelse(which.min(c(scores$propCals_16kcal_disc, scores$propCals_70pct_disc)) == 1, "model_disc_propCals_16kcal", "model_disc_propCals_70pct")

# Ergebnisse anzeigen
cat("Bestes Modell für Propofol-Tage (Patient Died):", best_propDays_died, "\n")
cat("Bestes Modell für Propofol-Kalorien (Patient Died):", best_propCals_died, "\n")
cat("Bestes Modell für Propofol-Tage (Patient Discharged):", best_propDays_disc, "\n")
cat("Bestes Modell für Propofol-Kalorien (Patient Discharged):", best_propCals_disc, "\n")




# # Vergleich der Variablen Days_CalsBelow16kcalPerKG und Days_CalsPercentageBelow70
# 
# library(sensitivity)
# 
# # Sensitivitätsanalyse für die Modelle mit Days_CalsBelow16kcalPerKG und Days_CalsPercentageBelow70
# 
# # Modellfunktion für Days_CalsBelow16kcalPerKG
# model_function_16kcal <- function(X) {
#   predict(model_death_propDays_16kcal, newdata = X, type = "response")
# }
# 
# # Modellfunktion für Days_CalsPercentageBelow70
# model_function_70pct <- function(X) {
#   predict(model_death_propDays_70pct, newdata = X, type = "response")
# }
# 
# # Eingabedaten mit relevanten Variablen erstellen
# data_inputs <- data.frame(
#   Days_CalsBelow16kcalPerKG = runif(100, min = 0, max = 7),
#   Days_CalsPercentageBelow70 = runif(100, min = 0, max = 7),
#   Days_OralIntake = runif(100, min = 0, max = 7),
#   Sex = sample(c("Male", "Female"), 100, replace = TRUE),
#   Year = sample(c(2007, 2008, 2009, 2011, 2013, 2014), 100, replace = TRUE),
#   AdmCat = sample(c("Medical", "Surgical/Emeregency", "Surgical/Elective"), 100, replace = TRUE),
#   LeadAdmDiag = sample(c("Orthopedic/Trauma", "Respiratory", "Gastrointestinal", "Other", "Sepsis", "Neurologic", "Cardio-Vascular", "Metabolic", "Renal"), 100, replace = TRUE),
#   inMV0To7 = runif(100, min = 0, max = 7),
#   Days_ProtBelow0.8GperKG = runif(100, min = 0, max = 7),
#   Days_Propofol = runif(100, min = 0, max = 7),
#   Days_ParenteralNut = sample(0:7, 100, replace = TRUE),
#   Age = runif(100, min = 18, max = 90),
#   BMI = runif(100, min = 15, max = 45),
#   ApacheIIScore = runif(100, min = 0, max = 100),
#   totalPropofolCal = runif(100, min = 0, max = 3000)
# )
# 
# # Sensitivitätsanalyse für Days_CalsBelow16kcalPerKG
# sobol_16kcal <- sobol(model = model_function_16kcal, X1 = data_inputs, X2 = data_inputs, nboot = 100)
# 
# # Sensitivitätsanalyse für Days_CalsPercentageBelow70
# sobol_70pct <- sobol(model = model_function_70pct, X1 = data_inputs, X2 = data_inputs, nboot = 100)
# 
# # Ergebnisse vergleichen
# cat("Sensitivitätsanalyse für Days_CalsBelow16kcalPerKG:\n")
# print(sobol_16kcal)
# 
# cat("Sensitivitätsanalyse für Days_CalsPercentageBelow70:\n")
# print(sobol_70pct)
# 
# # Auswahl des besseren Modells
# if (mean(sobol_16kcal$S) > mean(sobol_70pct$S)) {
#   cat("Days_CalsBelow16kcalPerKG ist die bessere Variable")
# } else {
#   cat("Days_CalsPercentageBelow70 ist die bessere Variable")
# }
