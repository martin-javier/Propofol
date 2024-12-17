# Bibliotheken laden
library(dplyr)
library(pammtools) # Für PED-Erstellung
library(mgcv)


data_summary <- model_data
data_summary$daysToEvent[data_summary$PatientDischarged == 1] <- 61L

# Schritt 2: Erstellen des PED für 60 Tage
ped_data <- as_ped(
  data = data_summary,
  Surv(daysToEvent, PatientDied) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)

# Ergebnis prüfen
view(ped_data)

# fehlt noch rando effekt für ICU mit CombinedicuID
model_final <- bam(
  formula = ped_status ~ s(Age, bs = "ps") + 
    s(BMI, bs = "ps") + 
    s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + 
    s(Days_ParenteralNut, bs = "ps") +
    s(Days_OralIntake, bs = "ps") +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + s(Days_ProtIntakeBelow30, bs = "ps") + 
    s(Days_Propofol, bs = "ps"),
  data = ped_data,
  family = poisson(),
  offset = offset
)

summary(model_final)

library(car)
vif_model <- gam.vcomp(model_final)
print(vif_model)
