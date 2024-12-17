# Bibliotheken laden
library(dplyr)
library(pammtools) # Für PED-Erstellung
library(mgcv)

# Schritt 1: Vorbereiten der zusammengefassten Tabelle
final_data_summary <- final_data %>%
  group_by(CombinedID) %>%
  summarise(
    Age = first(Age),
    BMI = first(BMI),
    ApacheIIScore = first(ApacheIIScore),
    DaysMechVent = first(DaysMechVent),
    OralIntake = first(OralIntake),
    ParenteralNut = sum(as.numeric(as.character(ParenteralNut)), na.rm = TRUE),
    Sex = first(Sex),
    Year = first(Year),
    AdmCatID = first(AdmCatID),
    DiagID2 = first(LeadAdmDiag),
    ProteinIntakeBelow30 = sum(as.numeric(as.character(ProteinIntakeBelow30)), na.rm = TRUE),
    propofol_1_11 = sum(as.numeric(as.character(Propofol)), na.rm = TRUE),
    PatientDied = max(PatientDied),    # Ereignisstatus: 1 = Tod, 0 = Zensiert
    Status_icu = first(surv_icu_status_exp),
    DaysToEvent = first(daysToEvent)
  )

final_data_summary$DaysToEvent[final_data_summary$Status_icu == "PatientDischarged"] <- 61L

# Schritt 2: Erstellen des PED für 60 Tage
ped_data <- as_ped(
  data = final_data_summary,
  Surv(DaysToEvent, PatientDied) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)

# Ergebnis prüfen
view(ped_data)


model_final <- bam(
  formula = ped_status ~ s(Age, bs = "ps") + 
    s(BMI, bs = "ps") + 
    s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + 
    s(ParenteralNut, bs = "ps") + # Keine Spline für OralIntake, sondern direkt verwenden
    factor(OralIntake) + # Optional: Als kategoriale Variable
    factor(Sex) + factor(Year) + factor(AdmCatID) + 
    factor(DiagID2) + s(ProteinIntakeBelow30, bs = "ps") + 
    s(propofol_1_11, bs = "ps") ,
  data = ped_data,
  family = poisson(),
  offset = offset
)

summary(model_final)

library(car)
vif_model <- gam.vcomp(model_final)
print(vif_model)
