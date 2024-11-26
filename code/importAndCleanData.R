import_and_clean <- function(){
  # read the Rds files in data folder
  daily <- readRDS("data/daily.Rds")
  ICU <- readRDS("data/ICU.Rds")
  mergedAndCleaned <- readRDS("data/mergedAndCleanedData.Rds")
  patient <- readRDS("data/patient.Rds")
  
  # filter patients by criteria given by Prof. Hartl
  data <- mergedAndCleaned %>%
    filter(Age >= 18, BMI > 13 , DaysInICU >= 7, Study_Day <= 7) %>%
    mutate(ProteinIntakeBelow30 = ifelse(proteinAdjustedPercentage < 30, 1, 0))
  
  # data %>%
  #   group_by(CombinedID) %>%
  #   summarize(unique_bmi_count = n_distinct(BMI)) %>%
  #   filter(unique_bmi_count > 1)
  # # => Every Patients BMI is the same value for every 7 days
  # # Same for every Variable which is summerised with first func below, I checked every single one
  
  # Summarise Data such that every patient has 1 row
  data <- data %>%
    group_by(CombinedID) %>%
    summarise(
      Year = first(Year), Sex = first(Gender), BMI = first(BMI), Age = first(Age),
      Weight = first(Weight), ApacheIIScore = first(ApacheIIScore),
      AdmCat = first(AdmCatID), LeadAdmDiag = first(DiagID2),
      PatientDied = first(PatientDied), PatientDischarged = first(PatientDischarged),
      Surv0to60 = first(Surv0To60), Disc0To60 = first(Disc0To60),
      surv_icu0to60 = first(surv_icu0to60), surv_icu_status = first(surv_icu_status),
      daysToEvent = first(event), DaysInICU = first(DaysInICU),
      CombinedicuID = first(CombinedicuID),
      Calories = first(Calories), Protein = first(Protein),
      DaysMechVent = first(DaysMechVent),
      Days_OralIntake = sum(OralIntake),
      Days_Propofol = sum(as.numeric(as.character(Propofol))), totalPropofolCal = sum(PropofolCal),
      Days_ParNut = sum(PN),
      Days_ProtIntakeBelow30 = sum(ProteinIntakeBelow30),
      .groups = "drop"
    )
  # Ist das sinvoll für PropofolCal die komplette Summe zu nehmen? Frage für Mona/Andreas
  # Dadurch geht halt etwas Wissen verloren also wie viel hat ein Patient an Tag X bekommen
  # jetzt nurnoch Patient hat Tag 1-7 Summe X an PrpofolCal eingenommen
  
  data <- data %>%
    mutate(surv_icu_status_exp = case_when(
      surv_icu_status == "0" ~ "PatientHospital",
      surv_icu_status == "1" ~ "PatientDischarged",
      surv_icu_status == "2" ~ "PatientDied",
    ))
  
  return(data)
}










# mergedAndCleaned %>%
#   group_by(CombinedID) %>%
#   summarize(day_count = n_distinct(Study_Day)) %>%
#   filter(day_count != 11)
# # Das heißt jeder Patient hat 11 Rows und 11 Study Days, auch die die in <11 Tagen gestorben sind
# 
# 
# # Patienten total
# length(unique(mergedAndCleaned$CombinedID))
# nrow(mergedAndCleaned) / 11
# # bestätigt dass jeder Patient 11 Zeilen hat (siehe oben)
# 
# # Anz Patienten überlebt
# nrow(mergedAndCleaned[mergedAndCleaned$PatientDied == 0, ]) / 11
# # gestorbene
# nrow(mergedAndCleaned[mergedAndCleaned$PatientDied == 1, ]) / 11
# 
# # Patienten die nach min 30 Tagen noch gestorben sind
# nrow(mergedAndCleaned[mergedAndCleaned$PatientDied == 1 & mergedAndCleaned$surv_icu0to60 >= 30,]) / 11
# 
# # Größe der Subgruppen: älter 65 Jahre, weibliche und männliche Patienten
# nrow(mergedAndCleaned[mergedAndCleaned$Age >= 65, ]) / 11
# nrow(mergedAndCleaned[mergedAndCleaned$Gender == "Female", ]) / 11
# nrow(mergedAndCleaned[mergedAndCleaned$Gender == "Male", ]) / 11
# 
# # Anz Patienten, mit Aufenthalt min. 7 Tage in ICU
# nrow(mergedAndCleaned[mergedAndCleaned$surv_icu0to60 >= 7, ]) / 11
# nrow(mergedAndCleaned[mergedAndCleaned$DaysInICU >= 7, ]) / 11
# # Vorgaben von Prof Hartl: Aufenthalt min.7 days, Alter >= 18 & BMI > 13
# nrow(mergedAndCleaned[mergedAndCleaned$surv_icu0to60 >= 7 & mergedAndCleaned$Age >= 18
#                       & mergedAndCleaned$BMI > 13, ]) / 11
# # => Es gibt keine Patienten mit BMI <= 13 oder Alter < 18
