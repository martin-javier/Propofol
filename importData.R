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
