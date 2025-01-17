# Modell Vorgabe Prof. Hartl ####

## Patient Died ####
# prepare data
data_death <- data_summed_Day0To7 %>%
  mutate(daysToEvent = if_else(PatientDischarged == 1, 61L, daysToEvent))

### Propofol Days ####
# with Calorie Variable: Days where Calories where lower than 16 kcal/kg
cox_died_propDays <- coxph(
  Surv(daysToEvent, PatientDied) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsAbove16kcalPerKG + Days_Propofol,
  data = data_death
)
summary(cox_died_propDays)
plot(survfit(cox_died_propDays))

# with Calore Variable: Days where Calorie Intake was below 70% of Target
cox_died_propDays2 <- coxph(
  Surv(daysToEvent, PatientDied) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsPercentageAbove70 + Days_Propofol,
  data = data_death
)
summary(cox_died_propDays2)
plot(survfit(cox_died_propDays2))

### Propofol Cals ####
# with Calorie Variable: Days where Calories where lower than 16 kcal/kg
cox_died_propCals <- coxph(
  Surv(daysToEvent, PatientDied) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsAbove16kcalPerKG + totalPropofolCal,
  data = data_death
)
summary(cox_died_propCals)
plot(survfit(cox_died_propCals))

# with Calore Variable: Days where Calorie Intake was below 70% of Target
cox_died_propCals2 <- coxph(
  Surv(daysToEvent, PatientDied) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsPercentageAbove70 + totalPropofolCal,
  data = data_death
)
summary(cox_died_propCals2)
plot(survfit(cox_died_propCals2))



## Patient Discharged ####
# prep data
data_disc <- data_summed_Day0To7 %>%
  mutate(daysToEvent = if_else(PatientDied == 1, 61L, daysToEvent))

### Propofol Days ####
# with Calorie Variable: Days where Calories where lower than 16 kcal/kg
cox_disc_propDays <- coxph(
  Surv(daysToEvent, PatientDischarged) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsAbove16kcalPerKG + Days_Propofol,
  data = data_disc
)
summary(cox_disc_propDays)
plot(survfit(cox_disc_propDays))

# with Calore Variable: Days where Calorie Intake was below 70% of Target
cox_disc_propDays2 <- coxph(
  Surv(daysToEvent, PatientDischarged) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsPercentageAbove70 + Days_Propofol,
  data = data_disc
)
summary(cox_disc_propDays2)
plot(survfit(cox_disc_propDays2))


### Propofol Cals ####
# with Calorie Variable: Days where Calories where lower than 16 kcal/kg
cox_disc_propCals <- coxph(
  Surv(daysToEvent, PatientDischarged) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsAbove16kcalPerKG + totalPropofolCal,
  data = data_disc
)
summary(cox_disc_propCals)
plot(survfit(cox_disc_propCals))

# with Calore Variable: Days where Calorie Intake was below 70% of Target
cox_disc_propCals2 <- coxph(
  Surv(daysToEvent, PatientDischarged) ~ Age + BMI + ApacheIIScore + Sex + Year + AdmCat +
    LeadAdmDiag + inMV0To7 + Days_OralIntake + Days_ParenteralNut + Days_ProtBelow0.8GperKG +
    Days_CalsPercentageAbove70 + totalPropofolCal,
  data = data_disc
)
summary(cox_disc_propCals2)
plot(survfit(cox_disc_propCals2))