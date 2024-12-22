clean_and_summarise <- function(){
  # read the Rds file in data folder
  mergedAndCleaned <- readRDS("data/mergedAndCleanedData.Rds")
  
  # filter patients by criteria given by Prof. Hartl
  data <- mergedAndCleaned %>%
    filter(Age >= 18, BMI > 13 , DaysInICU >= 7) %>%
    mutate(proteinIntake = PN_Protein + EN_Protein) %>%
    mutate(proteinGperKG = proteinIntake / Weight) %>%
    mutate(ProteinBelow0.8GperKG = ifelse(proteinGperKG < 0.8, 1, 0)) %>%
    mutate(CalsBelow16kcalPerKG = ifelse(calproKg < 16, 1, 0)) %>%
    mutate(CalsPercentageBelow70 = ifelse(caloriesPercentage < 70, 1, 0))
  
  # remove columns with "2_4"
  data <- data[, !grepl("2_4", colnames(data))]
  
  # get the Event day by rounding up the days to event value
  data$eventDay <- ceiling(data$event)
  data$lastDayInICU <- ceiling(data$DaysInICU)
  #nrow(data[data$Study_Day > data$lastDay, ])
  # > there's 2409 observations where the patient was already dead or discharged
  data <- data[data$Study_Day <= data$eventDay, ]
  
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
      CombinedicuID = first(CombinedicuID),
      icuByDummy = first(icuByDummy),
      Year = first(Year),
      Age = first(Age),
      BMI = first(BMI),
      ApacheIIScore = first(ApacheIIScore),
      Sex = first(Gender),
      Weight = first(Weight),
      AdmCat = first(AdmCatID),
      LeadAdmDiag = first(DiagID2),
      Days_OralIntake = sum(OralIntake),
      Days_ParenteralNut = sum(PN),
      Days_ProtBelow0.8GperKG = sum(ProteinBelow0.8GperKG),
      Days_CalsBelow16kcalPerKG = sum(CalsBelow16kcalPerKG),
      Days_CalsPercentageBelow70 = sum(CalsPercentageBelow70),
      inMV0To11 = sum(inMV),
      total_DaysMechVent = first(DaysMechVent),
      Days_Propofol = sum(as.numeric(as.character(Propofol))),
      totalPropofolCal = sum(PropofolCal),
      PatientDied = first(PatientDied),
      PatientDischarged = first(PatientDischarged),
      surv_icu_status = first(surv_icu_status),
      daysToEvent = first(event),
      DaysInICU = first(DaysInICU),
      eventDay = first(eventDay),
      lastDayInICU = first(lastDayInICU),
      Days_EnteralNut = sum(EN),
      totalCalories = sum(caloriesIntake),
      DaysRecorded = max(Study_Day),
      .groups = "drop"
    )
  
  # correct wrongly labeled surv_icu_status for some patients
  data <- data %>%
    mutate(surv_icu_status = case_when(
      PatientDischarged == 0 & PatientDied == 0 ~ 0,   # PatientHospital
      PatientDischarged == 1 & PatientDied == 0 ~ 1,   # PatientDischarged
      PatientDischarged == 0 & PatientDied == 1 ~ 2    # PatientDied
    ))
  data <- data %>%
    mutate(surv_icu_status_exp = case_when(
      surv_icu_status == "0" ~ "PatientHospital",
      surv_icu_status == "1" ~ "PatientDischarged",
      surv_icu_status == "2" ~ "PatientDied",
    ))
  # can check with this:
  #unique(data[(data$PatientDied == 0 & data$PatientDischarged == 0 & data$surv_icu_status != 0), ]$CombinedID)
  #unique(data[(data$PatientDied == 0 & data$PatientDischarged == 1 & data$surv_icu_status != 1), ]$CombinedID)
  #unique(data[(data$PatientDied == 1 & data$PatientDischarged == 0 & data$surv_icu_status != 2), ]$CombinedID)
  
  return(data)
}


clean_and_summarise_Days0To7 <- function(){
  # read the Rds file in data folder
  mergedAndCleaned <- readRDS("data/mergedAndCleanedData.Rds")
  
  # filter patients by criteria given by Prof. Hartl
  data <- mergedAndCleaned %>%
    filter(Age >= 18, BMI > 13 , DaysInICU >= 7, Study_Day <= 7) %>%
    mutate(proteinIntake = PN_Protein + EN_Protein) %>%
    mutate(proteinGperKG = proteinIntake / Weight) %>%
    mutate(ProteinBelow0.8GperKG = ifelse(proteinGperKG < 0.8, 1, 0)) %>%
    mutate(CalsBelow16kcalPerKG = ifelse(calproKg < 16, 1, 0)) %>%
    mutate(CalsPercentageBelow70 = ifelse(caloriesPercentage < 70, 1, 0))
  
  # remove columns with "2_4"
  data <- data[, !grepl("2_4", colnames(data))]
  
  # get the Event day by rounding up the days to event value
  data$eventDay <- ceiling(data$event)
  data$lastDayInICU <- ceiling(data$DaysInICU)
  
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
      CombinedicuID = first(CombinedicuID),
      icuByDummy = first(icuByDummy),
      Year = first(Year),
      Age = first(Age),
      BMI = first(BMI),
      ApacheIIScore = first(ApacheIIScore),
      Sex = first(Gender),
      Weight = first(Weight),
      AdmCat = first(AdmCatID),
      LeadAdmDiag = first(DiagID2),
      Days_OralIntake = sum(OralIntake),
      Days_ParenteralNut = sum(PN),
      Days_ProtBelow0.8GperKG = sum(ProteinBelow0.8GperKG),
      Days_CalsBelow16kcalPerKG = sum(CalsBelow16kcalPerKG),
      Days_CalsPercentageBelow70 = sum(CalsPercentageBelow70),
      inMV0To7 = sum(inMV),
      total_DaysMechVent = first(DaysMechVent),
      Days_Propofol = sum(as.numeric(as.character(Propofol))),
      totalPropofolCal = sum(PropofolCal),
      PatientDied = first(PatientDied),
      PatientDischarged = first(PatientDischarged),
      surv_icu_status = first(surv_icu_status),
      daysToEvent = first(event),
      DaysInICU = first(DaysInICU),
      eventDay = first(eventDay),
      lastDayInICU = first(lastDayInICU),
      Days_EnteralNut = sum(EN),
      totalCalories = sum(caloriesIntake),
      DaysRecorded = max(Study_Day),
      .groups = "drop"
    )
  
  # correct wrongly labeled surv_icu_status for some patients
  data <- data %>%
    mutate(surv_icu_status = case_when(
      PatientDischarged == 0 & PatientDied == 0 ~ 0,   # PatientHospital
      PatientDischarged == 1 & PatientDied == 0 ~ 1,   # PatientDischarged
      PatientDischarged == 0 & PatientDied == 1 ~ 2    # PatientDied
    ))
  data <- data %>%
    mutate(surv_icu_status_exp = case_when(
      surv_icu_status == "0" ~ "PatientHospital",
      surv_icu_status == "1" ~ "PatientDischarged",
      surv_icu_status == "2" ~ "PatientDied",
    ))
  # can check with this:
  #unique(data[(data$PatientDied == 0 & data$PatientDischarged == 0 & data$surv_icu_status != 0), ]$CombinedID)
  #unique(data[(data$PatientDied == 0 & data$PatientDischarged == 1 & data$surv_icu_status != 1), ]$CombinedID)
  #unique(data[(data$PatientDied == 1 & data$PatientDischarged == 0 & data$surv_icu_status != 2), ]$CombinedID)
  
  return(data)
}



clean_data <- function() {
  # read the Rds file in data folder
  mergedAndCleaned <- readRDS("data/mergedAndCleanedData.Rds")
  
  # filter patients by criteria given by Prof. Hartl but keep all Study Days
  data <- mergedAndCleaned %>%
    filter(Age >= 18, BMI > 13 , DaysInICU >= 7) %>%
    mutate(ProteinIntakeBelow30 = ifelse(proteinAdjustedPercentage < 30, 1, 0))
  
  # remove columns with "2_4"
  data <- data[, !grepl("2_4", colnames(data))]
  
  # get the Event day by rounding up the days to event value
  data$eventDay <- ceiling(data$event)
  data$lastDayInICU <- ceiling(data$DaysInICU)
  #nrow(data[data$Study_Day > data$lastDay, ])
  # > there's 2409 observations where the patient was already dead or discharged
  data <- data[data$Study_Day <= data$eventDay, ]
  
  # rename columns
  colnames(data)[colnames(data) == "Gender"] <- "Sex"
  colnames(data)[colnames(data) == "DiagID2"] <- "LeadAdmDiag"
  colnames(data)[colnames(data) == "event"] <- "daysToEvent"
  colnames(data)[colnames(data) == "PN"] <- "ParenteralNut"
  colnames(data)[colnames(data) == "EN"] <- "EnteralNut"
  
  # correct wrongly labeled surv_icu_status for 12 patients
  data <- data %>%
    mutate(surv_icu_status = case_when(
      PatientDischarged == 0 & PatientDied == 0 ~ 0,   # PatientHospital
      PatientDischarged == 1 & PatientDied == 0 ~ 1,   # PatientDischarged
      PatientDischarged == 0 & PatientDied == 1 ~ 2    # PatientDied
    ))
  data <- data %>%
    mutate(surv_icu_status_exp = case_when(
      surv_icu_status == "0" ~ "PatientHospital",
      surv_icu_status == "1" ~ "PatientDischarged",
      surv_icu_status == "2" ~ "PatientDied",
    ))
  # again can check with this:
  #unique(data[(data$PatientDied == 0 & data$PatientDischarged == 0 & data$surv_icu_status != 0), ]$CombinedID)
  #unique(data[(data$PatientDied == 0 & data$PatientDischarged == 1 & data$surv_icu_status != 1), ]$CombinedID)
  #unique(data[(data$PatientDied == 1 & data$PatientDischarged == 0 & data$surv_icu_status != 2), ]$CombinedID)
  
  # select needed columns
  data <- data[, c("CombinedID", "CombinedicuID", "icuByDummy", "Year", "Age", "BMI", "ApacheIIScore",
                   "Sex", "AdmCatID", "LeadAdmDiag", "OralIntake", "ParenteralNut", "ProteinIntakeBelow30",
                   "inMV", "DaysMechVent", "Propofol", "PropofolCal", "Study_Day",
                   "PatientDied", "PatientDischarged", "surv_icu_status",
                   "surv_icu_status_exp", "daysToEvent", "DaysInICU", "eventDay", "lastDayInICU",
                   "EnteralNut", "caloriesIntake")]
  
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
