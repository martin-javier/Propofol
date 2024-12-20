data_complete_temp <- days_to_add %>%
  left_join(data_long, by = c("CombinedID", "Study_Day")) %>% # Originaldaten hinzufügen
  left_join(data_long_mean, by = "CombinedID") %>% # Mittelwert von PropofolCal hinzufügen
  group_by(CombinedID) %>%
  fill(Year, LeadAdmDiag, AdmCatID, Sex, ApacheIIScore, BMI, PatientDied, 
       PatientDischarged, Age, 
       surv_icu_status, daysToEvent, CombinedicuID, 
       .direction = "downup") %>% # Füllt statische Werte
  mutate(
    # Summiere die Werte der ersten 11 Zeilen von Propofol
    propofol_sum = sum(as.numeric(as.character(Propofol))[row_number() <= 11], na.rm = TRUE),
    # Ersetze Propofol durch diesen Summenwert (1 oder 0 als Faktor)
    Propofol = propofol_sum) %>%
  select(-propofol_sum) %>% # Temporäre Spalte entfernen
  ungroup() %>%
  arrange(CombinedID, Study_Day)



cols_to_update <- colnames(data_complete_temp)

final_data_recycle <- data_complete_temp %>%
  group_by("CombinedID") %>% # Gruppieren nach Patienten
  mutate(
    # Recyceln der Werte für die angegebenen Spalten
    across(
      all_of(cols_to_update),
      ~ ifelse(is.na(.) & row_number() >= 12, .[row_number() == 11], .)
    )) %>%
  ungroup()








final_data_subset_temp <- final_data_recycle %>%
  select(CombinedID, Study_Day , PropofolCal) # Keep only the relevant columns

ped_data_temp <- ped_data %>%
  left_join(final_data_subset_temp, 
            by = c("CombinedID" = "CombinedID", "tend" = "Study_Day")) %>%
  mutate(totalPropofolCal = ifelse(!is.na(PropofolCal), PropofolCal, totalPropofolCal)) %>%
  select(-PropofolCal) # Temporäre Spalte entfernen

# fehlt noch rando effekt für ICU mit CombinedicuID
model_final_calories_recycle <- bam(
  formula = ped_status ~ s(Age, bs = "ps") + 
    s(BMI, bs = "ps") + 
    s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + 
    s(Days_ParenteralNut, bs = "ps") +
    s(Days_OralIntake, bs = "ps") +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + s(Days_ProtIntakeBelow30, bs = "ps") + 
    s(totalPropofolCal, bs = "ps"),
  data = ped_data_temp,
  family = poisson(),
  offset = offset
)

summary(model_final_calories_recycle)
