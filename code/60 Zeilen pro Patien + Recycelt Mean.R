library(dplyr)
library(tidyr)

# Schritt 1: Mittelwert für PropofolCal in den ersten 11 Tagen berechnen und runden
data_long_mean <- data_long %>%
  group_by(CombinedID) %>%
  summarize(mean_propofolcal_11 = round(mean(PropofolCal[Study_Day <= 11], na.rm = TRUE))) %>%
  ungroup()

# Schritt 2: Ergänze Tage 1–60 für alle Patienten
days_to_add <- expand.grid(
  CombinedID = unique(data_long$CombinedID),
  Study_Day = 1:60
)

data_complete <- days_to_add %>%
  left_join(data_long, by = c("CombinedID", "Study_Day")) %>% # Originaldaten hinzufügen
  left_join(data_long_mean, by = "CombinedID") %>% # Mittelwert von PropofolCal hinzufügen
  group_by(CombinedID) %>%
  fill(Year, LeadAdmDiag, AdmCatID, Sex, ApacheIIScore, BMI, PatientDied, 
       PatientDischarged, Age, 
       surv_icu_status, daysToEvent, CombinedicuID, 
       .direction = "downup") %>% # Füllt statische Werte
  mutate(PropofolCal = ifelse(is.na(PropofolCal) & Study_Day > 11, mean_propofolcal_11, PropofolCal)) %>% # Ergänzt Mittelwert
  ungroup()

# Schritt 3: Sortiere die Daten nach Patient (CombinedID) und Tag (Study_Day)
final_data <- data_complete %>%
  arrange(CombinedID, Study_Day)

# Ergebnis prüfen: Anzahl der Zeilen pro Patient
zeilen_check <- final_data %>%
  group_by(CombinedID) %>%
  summarize(total_days = n())

# Zeige Patienten, die nicht 60 Zeilen haben (falls vorhanden)
zeilen_check %>%
  filter(total_days != 60) %>%
  print()

view(final_data)



