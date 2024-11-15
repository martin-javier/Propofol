library(pammtools)
library(tidyverse)
library(mgcv)

data <- readRDS("data\\mergedAndCleanedData.Rds")

data_EK <- data_EK <- data %>%
  filter(Age >= 18, BMI > 13 , DaysInICU >= 7, Study_Day <= 7)
# min(data$BMI = 13.06431 zweiter Filter nicht benötigt
# length(unique(data_EK$CombinedID)) = 12474 Patienten in dem data set mit Einschlusskriterium

data_EK <- data_EK %>%
  mutate(row_id = row_number())

ped <- as_ped(
  data = data_EK,
  formula = Surv(Study_Day, PatientDied) ~ Age,
  id = "row_id"
)
# 
# model_age <- gam(
#   ped_status ~ s(Age, bs = "ps"),
#   data = ped,
#   family = poisson(),
#   offset = offset
# )
data_EK <- data_EK %>%
  group_by(CombinedID) %>%
  mutate(tstart = Study_Day - 1,   # Beispielhafte Zeitintervall-Definition
         tend = Study_Day) %>%
  ungroup()

model_age <- gam(
    ped_status ~ s(Age, bs = "ps"),
    data = ped1,
    family = poisson(),
    offset = offset
  )
  # Freiheitsgrade k hängen von der uniquen anzahl der Werte in tend ab tend = {1-7} also kann k max = 7 sein

# Komplexes Cox Modell mit Pamms

ped1 <- as_ped(
  data = data_EK,
  formula = Surv(tstart, tend, PatientDied) ~ Age + BMI + ApacheIIScore + 
    DaysMechVent + OralIntake + PN +  
    Gender + Year + AdmCatID + DiagID2,
  id = "row_id"
) 
# FEHLT: protein intake <30% of Target (zur classification of protein intake vgl angehängte 
# Dokumente #4 und #5) zwischen Tag 0 und Tag 7 nach Aufnahme Intensivstation
# Geht auch maybe mit Surv(Study_Day, PatientDied) gibt aber warning  über tend
ped1 %>% select(row_id, tstart, tend, interval)


model_confounders <- gam(
  ped_status ~ s(Age, bs = "ps") + s(BMI, bs = "ps") + s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + s(OralIntake, bs = "ps") + 
    s(PN, bs = "ps") +
    factor(Gender) + factor(Year) + factor(AdmCatID) + factor(DiagID2),
  data = ped1,  # Ped-Daten, die im Format `as_ped()` erstellt wurden
  family = poisson(),  # Poisson-Verteilung für Ereignismodelle
  offset = offset  # Berücksichtigt das Offset
)
summary(model_confounders)
