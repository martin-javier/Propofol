final_data_subset_1 <- final_data %>%
  select(CombinedID, Study_Day , Propofol, PropofolCal) # Keep only the relevant columns

final_data_subset_1 <- final_data_subset_1 %>%
  mutate(
    Propofol = as.numeric(as.character(Propofol)), # Convert Propofol to numeric
    Propofol = ifelse(is.na(Propofol), 
                      ifelse(PropofolCal != 0, 1, 0), 
                      ifelse(Propofol > 0, 1, 0)), # Apply binary logic
    Propofol = as.factor(Propofol) # Convert back to factor
  )


ped_data_single <- ped_data_test %>%
  left_join(final_data_subset_1 %>% select(CombinedID, Study_Day, Propofol),
            by = c("CombinedID" = "CombinedID", "tend" = "Study_Day")) %>%
  mutate(Days_Propofol = ifelse(!is.na(Propofol), Propofol, Days_Propofol)) %>%
  select(-Propofol) # Remove the Propofol column after replacement

ped_data_single <- ped_data_single %>%
  mutate(
    Days_Propofol = as.numeric(as.character(Days_Propofol)), # Convert Propofol to numeric
    Days_Propofol = ifelse(is.na(Days_Propofol), 
                      ifelse(totalPropofolCal != 0, 1, 0), 
                      ifelse(totalPropofolCal > 0, 1, 0)), # Apply binary logic
    Days_Propofol = as.factor(Days_Propofol) # Convert back to factor
  )

# Ergebnis prüfen
view(ped_data_single)

# fehlt noch rando effekt für ICU mit CombinedicuID
model_final_single <- bam(
  formula = ped_status ~ s(Age, bs = "ps") + 
    s(BMI, bs = "ps") + 
    s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + 
    s(Days_ParenteralNut, bs = "ps") +
    s(Days_OralIntake, bs = "ps") +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + s(Days_ProtIntakeBelow30, bs = "ps") + 
    factor(Days_Propofol),
  data = ped_data_single,
  family = poisson(),
  offset = offset
)

summary(model_final_single)
