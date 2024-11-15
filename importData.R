# setzt automatisch richtigen Pfad
if (!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# einlesen der Rds Dateien
daily <- readRDS("data/daily.Rds")
ICU <- readRDS("data/ICU.Rds")
mergedAndCleaned <- readRDS("data/mergedAndCleanedData.Rds")
patient <- readRDS("data/patient.Rds")

head(daily)
head(patient)
head(ICU)
head(mergedAndCleaned)
str(daily)
str(patient)
str(ICU)
str(mergedAndCleaned)
summary(daily)
summary(patient)
summary(ICU)
summary(mergedAndCleaned)


library(dplyr)
mergedAndCleaned %>%
  group_by(CombinedID) %>%
  summarize(day_count = n_distinct(Study_Day)) %>%
  filter(day_count != 11)
# Das heißt jeder Patient hat 11 Rows und 11 Study Days, auch die die in <11 Tagen gestorben sind


# Patienten total
length(unique(mergedAndCleaned$CombinedID))
nrow(mergedAndCleaned) / 11
# bestätigt dass jeder Patient 11 Zeilen hat (siehe oben)

# Anz Patienten überlebt
nrow(mergedAndCleaned[mergedAndCleaned$PatientDied == 0, ]) / 11
# gestorbene
nrow(mergedAndCleaned[mergedAndCleaned$PatientDied == 1, ]) / 11

# Patienten die nach min 30 Tagen noch gestorben sind
nrow(mergedAndCleaned[mergedAndCleaned$PatientDied == 1 & mergedAndCleaned$surv_icu0to60 >= 30,]) / 11

# Größe der Subgruppen: älter 65 Jahre, weibliche und männliche Patienten
nrow(mergedAndCleaned[mergedAndCleaned$Age >= 65, ]) / 11
nrow(mergedAndCleaned[mergedAndCleaned$Gender == "Female", ]) / 11
nrow(mergedAndCleaned[mergedAndCleaned$Gender == "Male", ]) / 11

# Anz Patienten, mit Aufenthalt min. 7 Tage in ICU
nrow(mergedAndCleaned[mergedAndCleaned$surv_icu0to60 >= 7, ]) / 11
