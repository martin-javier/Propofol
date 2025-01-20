# Modell Vorgabe Prof. Hartl ####

library(dplyr)
library(pammtools)
library(mgcv)

## Patient Died ####
# prepare data
data_death <- data_summed_Day0To7 %>%
  mutate(daysToEvent = if_else(PatientDischarged == 1, 61L, daysToEvent))

### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
ped_death <- as_ped(
  data = data_death,
  Surv(daysToEvent, PatientDied) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)

model_death_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

## Patient Discharged ####

### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
ped_disc <- as_ped(
  data = data_summed_Day0To7,
  Surv(daysToEvent, PatientDischarged) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)

model_disc_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)


# Modelle für "Patient Died"
saveRDS(model_death_propDays_16kcal, "models/model_death_propDays_16kcal.rds")
saveRDS(model_death_propDays_70pct, "models/model_death_propDays_70pct.rds")
saveRDS(model_death_propCals_16kcal, "models/model_death_propCals_16kcal.rds")
saveRDS(model_death_propCals_70pct, "models/model_death_propCals_70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_propDays_16kcal, "models/model_disc_propDays_16kcal.rds")
saveRDS(model_disc_propDays_70pct, "models/model_disc_propDays_70pct.rds")
saveRDS(model_disc_propCals_16kcal, "models/model_disc_propCals_16kcal.rds")
saveRDS(model_disc_propCals_70pct, "models/model_disc_propCals_70pct.rds")


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




# # Vergleich der Variablen Days_CalsAbove16kcalPerKG und Days_CalsPercentageAbove70
# 
# library(sensitivity)
# 
# # Sensitivitätsanalyse für die Modelle mit Days_CalsAbove16kcalPerKG und Days_CalsPercentageAbove70
# 
# # Modellfunktion für Days_CalsAbove16kcalPerKG
# model_function_16kcal <- function(X) {
#   predict(model_death_propDays_16kcal, newdata = X, type = "response")
# }
# 
# # Modellfunktion für Days_CalsPercentageAbove70
# model_function_70pct <- function(X) {
#   predict(model_death_propDays_70pct, newdata = X, type = "response")
# }
# 
# # Eingabedaten mit relevanten Variablen erstellen
# data_inputs <- data.frame(
#   Days_CalsAbove16kcalPerKG = runif(50, min = 0, max = 7),
#   Days_CalsPercentageAbove70 = runif(50, min = 0, max = 7),
#   Days_OralIntake = runif(50, min = 0, max = 7),
#   Sex = sample(c("Male", "Female"), 50, replace = TRUE),
#   Year = sample(c(2007, 2008, 2009, 2011, 2013, 2014), 50, replace = TRUE),
#   AdmCat = sample(c("Medical", "Surgical/Emeregency", "Surgical/Elective"), 50, replace = TRUE),
#   LeadAdmDiag = sample(c("Orthopedic/Trauma", "Respiratory", "Gastrointestinal", "Other", "Sepsis", "Neurologic", "Cardio-Vascular", "Metabolic", "Renal"), 50, replace = TRUE),
#   inMV0To7 = runif(50, min = 0, max = 7),
#   Days_ProtBelow0.8GperKG = runif(50, min = 0, max = 7),
#   Days_Propofol = runif(50, min = 0, max = 7),
#   Days_ParenteralNut = sample(0:7, 50, replace = TRUE),
#   Age = runif(50, min = 18, max = 90),
#   BMI = runif(50, min = 15, max = 45),
#   ApacheIIScore = runif(50, min = 0, max = 50),
#   totalPropofolCal = runif(50, min = 0, max = 3000)
# )
# 
# # Sensitivitätsanalyse für Days_CalsAbove16kcalPerKG
# sobol_16kcal <- sobol(model = model_function_16kcal, X1 = data_inputs, X2 = data_inputs, nboot = 50)
# 
# # Sensitivitätsanalyse für Days_CalsPercentageAbove70
# sobol_70pct <- sobol(model = model_function_70pct, X1 = data_inputs, X2 = data_inputs, nboot = 50)
# 
# # Ergebnisse vergleichen
# cat("Sensitivitätsanalyse für Days_CalsAbove16kcalPerKG:\n")
# print(sobol_16kcal)
# 
# cat("Sensitivitätsanalyse für Days_CalsPercentageAbove70:\n")
# print(sobol_70pct)
# 
# # Auswahl des besseren Modells
# if (mean(sobol_16kcal$S) > mean(sobol_70pct$S)) {
#   cat("Days_CalsAbove16kcalPerKG ist die bessere Variable")
# } else {
#   cat("Days_CalsPercentageAbove70 ist die bessere Variable")
# }
