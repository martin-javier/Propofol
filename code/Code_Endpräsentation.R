# Erstellen der Datensatz-Grafiken (Tabelle)
View(manualPED_death)
library(tidyverse)
library(gt)

data_presentation_ped <- manualPED_death
data_presentation_ped <- data_presentation_ped %>% 
  rename(ID = CombinedID)
df_select <- data_presentation_ped %>% 
  select(ID, tstart, tend, interval, offset, ped_status, Study_Day, surv_icu_status_exp, Age, Propofol, PropofolCal, daysToEvent)

df_select[1:4, ] %>% 
  gt() %>% 
  tab_header(title = "PED Format")

df_2 <- data_long
df_2 <- df_2 %>% 
  rename(ID = CombinedID)
df_select2 <- df_2 %>% 
  select(ID, Age, Study_Day, PatientDied, PatientDischarged, surv_icu_status_exp, Age, Propofol, PropofolCal, daysToEvent)
df_select2[1:4, ] %>% 
  gt() %>% 
  tab_header(title = "Dataset Before Transformation")


summary(model_death_propDays_calsAbove70pct_reICU)
summary(model_disc_propCals_calsAbove16_reICU)

plot(model_death_propCals_calsAbove70pct_reICU, select = 1)

# Exp-Werte für Interpretationen (Modell 1)
summary(model_death_propDays_calsAbove70pct_reICU)
model1 <- model_death_propDays_calsAbove70pct_reICU
# Ähnliche Interpretation wie bei Survival Analysis Übung 6 (Seite 15)
exp(coef(model1)["inMV1"])
exp(coef(model1)["ParenteralNut1"])
1/exp(coef(model1)["OralIntake1"])
1/exp(coef(model1)["Propofol1"])

# Exp-Werte für Interpretationen (Modell 2)
summary(model_disc_propCals_calsAbove16_reICU)
model2 <- model_disc_propCals_calsAbove16_reICU
#Interpretationen
exp(coef(model2)["inMV1"])
exp(coef(model2)["ParenteralNut1"])
1/exp(coef(model2)["OralIntake1"])
exp(coef(model2)["PropofolCal"])

# Splines für Modelle 1 und 2
par(mfrow = c(1, 1))
plot(model1, select = 1, se = TRUE, ylim = c(-2, 2))
plot(model1, select = 2, se = TRUE, ylim = c(-2, 2))
plot(model1, select = 3, se = TRUE, ylim = c(-2, 2))

plot(model2, select = 1, se = TRUE, ylim = c(-1, 0.4))
plot(model2, select = 2, se = TRUE, ylim = c(-1, 0.4))
plot(model2, select = 3, se = TRUE, ylim = c(-1, 0.4))

# Forest Plot für Modelle 1 und 2
tidy_fixed(model1)
plot1 <- gg_fixed(model1)
plot1 + ggtitle("Forrest Plot of fixed Coefficients")

plot2 <- gg_fixed(model2)
plot2 + ggtitle("Forrest Plot of fixed Coefficients")
