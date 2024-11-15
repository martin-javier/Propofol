# Load necessary libraries
library(dplyr)
library(pammtools)
library(survival)
library(splines)
library(tidyr)

# Load patient and daily datasets
data("patient")
data("daily")

# Simulate propofol use in the first 7 days by creating a dummy variable in the daily dataset
set.seed(42)
daily <- daily %>%
  mutate(propofol_used = ifelse(Study_Day <= 7, sample(c(0, 1), n(), replace = TRUE, prob = c(0.7, 0.3)), 0))

# Calculate Days_with_Propofol based on the simulated variable for the first 7 days
daily_7_days <- daily %>%
  filter(Study_Day <= 7) %>%
  group_by(CombinedID) %>%
  summarize(Days_with_Propofol = sum(propofol_used, na.rm = TRUE))

# Add another variable for "number of days" ranging from 0 to 6 as a confounder
daily <- daily %>%
  mutate(days_other_medication = sample(0:6, n(), replace = TRUE))

# Aggregate `days_other_medication` for each patient within the first 7 days
days_other_7_days <- daily %>%
  filter(Study_Day <= 7) %>%
  group_by(CombinedID) %>%
  summarize(days_other_medication = sum(days_other_medication, na.rm = TRUE))

# Merge Days_with_Propofol and days_other_medication into the patient dataset
patient_over_65 <- patient %>%
  filter(Age > 65) %>%
  left_join(daily_7_days, by = "CombinedID") %>%
  left_join(days_other_7_days, by = "CombinedID") %>%
  mutate(
    Days_with_Propofol = replace_na(Days_with_Propofol, 0),
    days_other_medication = replace_na(days_other_medication, 0)
  )

# Create splines for Age and BMI with 3 degrees of freedom
patient_over_65 <- patient_over_65 %>%
  mutate(
    Age_spline = bs(Age, df = 3),
    BMI_spline = bs(BMI, df = 3)
  )

# Define the interaction term between Age and Days_with_Propofol
patient_over_65 <- patient_over_65 %>%
  mutate(Age_Propofol_interaction = Age * Days_with_Propofol)

# Fit the Cox proportional hazards model with confounders
cox_model <- coxph(
  Surv(Survdays, PatientDied) ~ Age_spline + BMI_spline + Days_with_Propofol + 
    Age_Propofol_interaction + days_other_medication,
  data = patient_over_65
)

# Output the summary of the Cox model
summary(cox_model)

# Load necessary library for survival plot
library(survminer)

# Generate the survival plot directly from the fitted Cox model
ggsurvplot(survfit(cox_model), data = patient_over_65, 
           risk.table = TRUE,             # Shows number of patients at risk
           pval = TRUE,                   # Shows p-value of the log-rank test
           conf.int = TRUE,               # Adds confidence intervals for survival curves
           xlab = "Time (days)",          # Label for x-axis
           ylab = "Survival Probability", # Label for y-axis
           title = "Survival Probability based on Cox Model",
           ggtheme = theme_minimal())     # Apply minimal theme for cleaner look
