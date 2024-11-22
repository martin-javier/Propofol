library(tidyverse)
library(survival)
library(survminer)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

# Wie oft ein Patient Propofol in 11 Tagen bekommen hat
# mergedAndCleaned %>% 
#   filter(DaysInICU > 7, Age >= 18, BMI > 13) %>% 
#   group_by(CombinedID) %>% 
#   summarise(count_propofol = sum(Propofol == 1))

# Removing Study-Days to just get the count of propofol (Only One Row for each patient)
FilteredData <- mergedAndCleaned %>% 
  filter(DaysInICU > 7, Age >= 18, BMI > 13, Study_Day <= 7) %>% 
  dplyr::select(CombinedID, Year, DiagID2, AdmCatID, Gender, ApacheIIScore, BMI, PatientDied, PatientDischarged, 
                Age, Surv0To60, Disc0To60, event, DaysInICU, DaysMechVent, Study_Day, Propofol) %>% 
  group_by(CombinedID) %>% 
  mutate(CountPropofol = sum(Propofol == 1)) %>% 
  filter(row_number() == 1) %>% 
  select(-c(Propofol, Study_Day))

# Ideen von dieser Quelle: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
# Allgemeine Survival Analysis mit den Daten (event, PatientDied) (analog: event, PatientDischarged)
# Subject 1 was censored at time 26.083, Subject 2 was censored at 19.958 (Patient didn't die)
# Subject 7 had an event (event: Death of Patient) at time 24.83889
Surv(FilteredData$event, FilteredData$PatientDied)[1:10]

# survfit() & Kaplan-Meier Plots
s1 <- survfit(Surv(event, PatientDied) ~ 1, data = FilteredData)
str(s1)

survfit2(Surv(event, PatientDied) ~ 1, data = FilteredData) %>% 
  ggsurvfit() +
  labs(x = "Days",
       y = "Overall survival probability") +
  add_confidence_interval() +
  add_risktable()

# Estimating the x-days survival 
# 60-days probability of survival in this study is 0.555 (in general)
summary(survfit(Surv(event, PatientDied) ~ 1, data = FilteredData), times = 60)

survfit(Surv(event, PatientDied) ~ 1, data = FilteredData) %>% 
  tbl_survfit(
    times = 60,
    label_header = "**60-days survival (95% CI)**"
  )

# 60-days probability of survival in this study with CountPropofol (univariate)
summary(survfit(Surv(event, PatientDied) ~ CountPropofol, data = FilteredData), times = 60)

survfit(Surv(event, PatientDied) ~ CountPropofol, data = FilteredData) %>% 
  tbl_survfit(
    times = 60,
    label_header = "**60-days survival (95% CI)**"
  )


# Estimating median survival time
# You get NA values for median and the CI
survfit(Surv(event, PatientDied) ~ 1, data = FilteredData)


# Comparing survival times between groups
# Log-Rank Test to conduct between-group siginificance tests

survdiff(Surv(event, PatientDied) ~ Gender, data = FilteredData)

# Cox-Regression Model (with gender at first)
coxph(Surv(event, PatientDied) ~ Gender, data = FilteredData)
coxph(Surv(event, PatientDied) ~ Gender, data = FilteredData) %>% 
  tbl_regression(exp = TRUE)

# A HR < 1 indicates reduced hazard of death whereas a HR > 1 indicates an increased hazard of death.
# So the HR = 0.94 implies that 0.94 times as many males are dying than females at any given time



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


# Model A: Death (mit Gender)
# n = 12459 (number of observations)
# number of events = 3190 (number of deaths)
cox_model_gender <- coxph(formula = Surv(event, PatientDied) ~ count_propofol + Gender, data = filtered_data) 





