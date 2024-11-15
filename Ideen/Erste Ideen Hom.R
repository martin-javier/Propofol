library(tidyverse)
library(survival)
library(survminer)

# Wie oft ein Patient Propofol in 11 Tagen bekommen hat
# mergedAndCleaned %>% 
#   filter(DaysInICU > 7, Age >= 18, BMI > 13) %>% 
#   group_by(CombinedID) %>% 
#   summarise(count_propofol = sum(Propofol == 1))

# Removing Study-Days to just get the count of propofol (Only One Row for each patient)
# Protein Intake noch weggelassen
filtered_data <- mergedAndCleaned %>% 
  filter(DaysInICU > 7, Age >= 18, BMI > 13) %>% 
  dplyr::select(CombinedID, Age, BMI, ApacheIIScore, Gender, Year, AdmCatID,DiagID2, DaysInICU, DaysMechVent,Surv0To60, Disc0To60, event, Propofol, PatientDied, PatientDischarged) %>% 
  group_by(CombinedID) %>% 
  mutate(count_propofol = sum(Propofol == 1)) %>% 
  filter(row_number() == 1) %>% 
  select(-c(Propofol))
  
# Model mit den Filtered Data (ohne Splines)
# Model A: Death
cox_model <- coxph(formula = Surv(event, PatientDied) ~ Year + DiagID2 + AdmCatID + Gender + ApacheIIScore + BMI + count_propofol,
                   data = filtered_data)
summary(cox_model) 

# Einfache Visualisierung 
ggsurvplot(survfit(cox_model), data = filtered_data, color = "#2E9FDF", ggtheme = theme_minimal())


# Kopiert von Lukas
# Generate the survival plot directly from the fitted Cox model
ggsurvplot(survfit(cox_model), data = filtered_data, 
           risk.table = TRUE,             # Shows number of patients at risk
           pval = TRUE,                   # Shows p-value of the log-rank test
           conf.int = TRUE,               # Adds confidence intervals for survival curves
           xlab = "Time (days)",          # Label for x-axis
           ylab = "Survival Probability", # Label for y-axis
           title = "Survival Probability based on Cox Model",
           ggtheme = theme_minimal())     # Apply minimal theme for cleaner look