library(pammtools)

data(patient)
data(daily)

head(patient)
str(patient)
summary(patient)
daily <- as.data.frame(daily)
head(daily)
str(daily)
summary(daily)

# Simulate Propofol intake
set.seed(24)
# Bender meinte Propofol Variable in daily ist 1 wenn patient propofol bekommen hat, 0 wenn nicht
freq <- 0.25 # heißt, dass Patient in 25% der Fälle Propofol bekommen hat
daily$propofol_taken <- rbinom(n = nrow(daily), size = 1, prob = freq)

# Jetzt müssten wir schauen, wie wir damit weitermachen könnten, dass es Sinn macht im Modell
# z.B. wir schauen an welchen Tagen ein Patient Propofol bekommen hat und speichern das iwie
# in einer oder mehreren Variablen, die wir dann als Einflussgröße benutzen können

# Das wäre z.B. ein ansatz, hier ist aber nicht drin, an welchem Tag der Patient Propofol bekommen hat
merged <- merge(daily, patient, by = "CombinedID")
# die Variable sagt aus, ob der Patient jemals Propofol genommen hat, 1 = ja, 0 = nein
merged$took_propofol <- ave(merged$propofol_taken, merged$CombinedID, FUN = max)
model <- glm(PatientDied ~ propofol_taken + Gender + Age + AdmCatID + ApacheIIScore + BMI + Study_Day, 
             data = merged)
summary(model)
# denke es könnte schon wichtig sein, an welchen Tagen die Patienten Propofol bekommen haben
# das Modell ist ziehmlicher freestyle btw die Ergebnisse kann man eig ignorieren

# kumulierter Propofol intake, also wenn Patient an 3 Tagen bekommen hat ist Variable = 3, etc.
library(dplyr)
merged <- merged %>%
  group_by(CombinedID) %>%
  mutate(cumulated_propofol = sum(propofol_taken)) %>%
  ungroup()


unique(merged$Study_Day)
# wir haben daten von max den ersten 12 Tagen, anderer Ansatz wäre wenn wir 12 Variablen
# hinzufügen Propofol_on_day1, Propofol_on_day2, ..., Propofol_on_day12
# die sind dann immer 1 wenn ja un 0 wenn nein
# Dann hätten wir berücksichtigt an welchem Tag Propofol genommen wurde aber das Modell hat halt 
# viele Einflussgrößen
