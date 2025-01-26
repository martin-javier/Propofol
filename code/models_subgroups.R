# Aufsummerte Daten (Vorgabe Hartl) ####


## Alter > 65 Jahre ####

### Patient Died ####

data_death <- data_summed_Day0To7 %>%
  mutate(daysToEvent = if_else(PatientDischarged == 1, 61L, daysToEvent))
ped_death <- as_ped(
  data = data_death,
  Surv(daysToEvent, PatientDied) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)
ped_death_older65 <- ped_death[ped_death$Age > 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_older65_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_older65_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_older65_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_older65_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

ped_disc <- as_ped(
  data = data_summed_Day0To7,
  Surv(daysToEvent, PatientDischarged) ~ .,
  cut = 0:60, id = "CombinedID" # Zeitintervall für 60 Tage (1 Tag-Schritte)
)
ped_disc_older65 <- ped_disc[ped_disc$Age > 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_older65_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_older65_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_older65_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_older65_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_older65_propDays_16kcal,
        "models/subgroups/model_death_older65_propDays_16kcal.rds")
saveRDS(model_death_older65_propDays_70pct,
        "models/subgroups/model_death_older65_propDays_70pct.rds")
saveRDS(model_death_older65_propCals_16kcal,
        "models/subgroups/model_death_older65_propCals_16kcal.rds")
saveRDS(model_death_older65_propCals_70pct,
        "models/subgroups/model_death_older65_propCals_70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_older65_propDays_16kcal,
        "models/subgroups/model_disc_older65_propDays_16kcal.rds")
saveRDS(model_disc_older65_propDays_70pct,
        "models/subgroups/model_disc_older65_propDays_70pct.rds")
saveRDS(model_disc_older65_propCals_16kcal,
        "models/subgroups/model_disc_older65_propCals_16kcal.rds")
saveRDS(model_disc_older65_propCals_70pct,
        "models/subgroups/model_disc_older65_propCals_70pct.rds")



## Alter <= 65 Jahre ####

### Patient Died ####

ped_death_younger66 <- ped_death[ped_death$Age <= 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_younger66_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_younger66_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_younger66_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_younger66_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

ped_disc_younger66 <- ped_disc[ped_disc$Age <= 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_younger66_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_younger66_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_younger66_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_younger66_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_younger66_propDays_16kcal,
        "models/subgroups/model_death_younger66_propDays_16kcal.rds")
saveRDS(model_death_younger66_propDays_70pct,
        "models/subgroups/model_death_younger66_propDays_70pct.rds")
saveRDS(model_death_younger66_propCals_16kcal,
        "models/subgroups/model_death_younger66_propCals_16kcal.rds")
saveRDS(model_death_younger66_propCals_70pct,
        "models/subgroups/model_death_younger66_propCals_70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_younger66_propDays_16kcal,
        "models/subgroups/model_disc_younger66_propDays_16kcal.rds")
saveRDS(model_disc_younger66_propDays_70pct,
        "models/subgroups/model_disc_younger66_propDays_70pct.rds")
saveRDS(model_disc_younger66_propCals_16kcal,
        "models/subgroups/model_disc_younger66_propCals_16kcal.rds")
saveRDS(model_disc_younger66_propCals_70pct,
        "models/subgroups/model_disc_younger66_propCals_70pct.rds")



## Weibliche Patienten ####

### Patient Died ####

ped_death_female <- ped_death[ped_death$Sex == "Female", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_female_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_female_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_female_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_female_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

ped_disc_female <- ped_disc[ped_disc$Sex == "Female", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_female_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_female_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_female_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_female_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_female_propDays_16kcal,
        "models/subgroups/model_death_female_propDays_16kcal.rds")
saveRDS(model_death_female_propDays_70pct,
        "models/subgroups/model_death_female_propDays_70pct.rds")
saveRDS(model_death_female_propCals_16kcal,
        "models/subgroups/model_death_female_propCals_16kcal.rds")
saveRDS(model_death_female_propCals_70pct,
        "models/subgroups/model_death_female_propCals_70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_female_propDays_16kcal,
        "models/subgroups/model_disc_female_propDays_16kcal.rds")
saveRDS(model_disc_female_propDays_70pct,
        "models/subgroups/model_disc_female_propDays_70pct.rds")
saveRDS(model_disc_female_propCals_16kcal,
        "models/subgroups/model_disc_female_propCals_16kcal.rds")
saveRDS(model_disc_female_propCals_70pct,
        "models/subgroups/model_disc_female_propCals_70pct.rds")



## Männliche Patienten ####

### Patient Died ####

ped_death_male <- ped_death[ped_death$Sex == "Male", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_male_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_male_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_male_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_male_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

ped_disc_male <- ped_disc[ped_disc$Sex == "Male", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_male_propDays_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_male_propDays_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(Days_Propofol, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_male_propCals_16kcal <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsAbove16kcalPerKG, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_male_propCals_70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    s(inMV0To7, bs = "ps", k = 5) +
    s(Days_ParenteralNut, bs = "ps", k = 5) +
    s(Days_OralIntake, bs = "ps", k = 5) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCat) +
    factor(LeadAdmDiag) +
    s(totalPropofolCal, bs = "ps", k = 5) +
    s(Days_CalsPercentageAbove70, bs = "ps", k = 5) +
    s(Days_ProtBelow0.8GperKG, bs = "ps", k = 5) +
    s(CombinedicuID, bs = "re"),
  data = ped_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_male_propDays_16kcal,
        "models/subgroups/model_death_male_propDays_16kcal.rds")
saveRDS(model_death_male_propDays_70pct,
        "models/subgroups/model_death_male_propDays_70pct.rds")
saveRDS(model_death_male_propCals_16kcal,
        "models/subgroups/model_death_male_propCals_16kcal.rds")
saveRDS(model_death_male_propCals_70pct,
        "models/subgroups/model_death_male_propCals_70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_male_propDays_16kcal,
        "models/subgroups/model_disc_male_propDays_16kcal.rds")
saveRDS(model_disc_male_propDays_70pct,
        "models/subgroups/model_disc_male_propDays_70pct.rds")
saveRDS(model_disc_male_propCals_16kcal,
        "models/subgroups/model_disc_male_propCals_16kcal.rds")
saveRDS(model_disc_male_propCals_70pct,
        "models/subgroups/model_disc_male_propCals_70pct.rds")



# Manual PED als Datenbasis ####


## Alter > 65 Jahre ####

### Patient Died ####

manualPED_death_older65 <- manualPED_death[manualPED_death$Age > 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_older65_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_older65_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_older65_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal + 
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_older65_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

manualPED_disc_older65 <- manualPED_disc[manualPED_disc$Age > 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_older65_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_older65_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_older65_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_older65_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_older65,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_older65_propDays_calsAbove16,
        "models/subgroups/manualPED/model_death_older65_propDays_calsAbove16.rds")
saveRDS(model_death_older65_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_death_older65_propDays_calsAbove70pct.rds")
saveRDS(model_death_older65_propCals_calsAbove16,
        "models/subgroups/manualPED/model_death_older65_propCals_calsAbove16.rds")
saveRDS(model_death_older65_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_death_older65_propCals_calsAbove70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_older65_propDays_calsAbove16,
        "models/subgroups/manualPED/model_disc_older65_propDays_calsAbove16.rds")
saveRDS(model_disc_older65_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_older65_propDays_calsAbove70pct.rds")
saveRDS(model_disc_older65_propCals_calsAbove16,
        "models/subgroups/manualPED/model_disc_older65_propCals_calsAbove16.rds")
saveRDS(model_disc_older65_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_older65_propCals_calsAbove70pct.rds")



## Alter <= 65 Jahre ####

### Patient Died ####

manualPED_death_younger66 <- manualPED_death[manualPED_death$Age <= 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_younger66_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_younger66_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_younger66_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_younger66_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

manualPED_disc_younger66 <- manualPED_disc[manualPED_disc$Age <= 65, ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_younger66_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_younger66_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_younger66_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_younger66_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_younger66,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_younger66_propDays_calsAbove16,
        "models/subgroups/manualPED/model_death_younger66_propDays_calsAbove16.rds")
saveRDS(model_death_younger66_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_death_younger66_propDays_calsAbove70pct.rds")
saveRDS(model_death_younger66_propCals_calsAbove16,
        "models/subgroups/manualPED/model_death_younger66_propCals_calsAbove16.rds")
saveRDS(model_death_younger66_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_death_younger66_propCals_calsAbove70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_younger66_propDays_calsAbove16,
        "models/subgroups/manualPED/model_disc_younger66_propDays_calsAbove16.rds")
saveRDS(model_disc_younger66_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_younger66_propDays_calsAbove70pct.rds")
saveRDS(model_disc_younger66_propCals_calsAbove16,
        "models/subgroups/manualPED/model_disc_younger66_propCals_calsAbove16.rds")
saveRDS(model_disc_younger66_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_younger66_propCals_calsAbove70pct.rds")



## Weibliche Patienten ####

### Patient Died ####

manualPED_death_female <- manualPED_death[manualPED_death$Sex == "Female", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_female_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_female_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_female_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_female_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

manualPED_disc_female <- manualPED_disc[manualPED_disc$Sex == "Female", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_female_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_female_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_female_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_female_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_female,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_female_propDays_calsAbove16,
        "models/subgroups/manualPED/model_death_female_propDays_calsAbove16.rds")
saveRDS(model_death_female_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_death_female_propDays_calsAbove70pct.rds")
saveRDS(model_death_female_propCals_calsAbove16,
        "models/subgroups/manualPED/model_death_female_propCals_calsAbove16.rds")
saveRDS(model_death_female_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_death_female_propCals_calsAbove70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_female_propDays_calsAbove16,
        "models/subgroups/manualPED/model_disc_female_propDays_calsAbove16.rds")
saveRDS(model_disc_female_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_female_propDays_calsAbove70pct.rds")
saveRDS(model_disc_female_propCals_calsAbove16,
        "models/subgroups/manualPED/model_disc_female_propCals_calsAbove16.rds")
saveRDS(model_disc_female_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_female_propCals_calsAbove70pct.rds")



## Männliche Patienten ####

### Patient Died ####

manualPED_death_male <- manualPED_death[manualPED_death$Sex == "Male", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_male_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_male_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_male_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_male_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

### Patient Discharged ####

manualPED_disc_male <- manualPED_disc[manualPED_disc$Sex == "Male", ]

#### Propofol Days ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_male_propDays_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_male_propDays_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

#### Propofol Cals ####

# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_male_propCals_calsAbove16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsAbove16kcalPerKG +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_male_propCals_calsAbove70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc_male,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Modelle für "Patient Died"
saveRDS(model_death_male_propDays_calsAbove16,
        "models/subgroups/manualPED/model_death_male_propDays_calsAbove16.rds")
saveRDS(model_death_male_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_death_male_propDays_calsAbove70pct.rds")
saveRDS(model_death_male_propCals_calsAbove16,
        "models/subgroups/manualPED/model_death_male_propCals_calsAbove16.rds")
saveRDS(model_death_male_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_death_male_propCals_calsAbove70pct.rds")

# Modelle für "Patient Discharged"
saveRDS(model_disc_male_propDays_calsAbove16,
        "models/subgroups/manualPED/model_disc_male_propDays_calsAbove16.rds")
saveRDS(model_disc_male_propDays_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_male_propDays_calsAbove70pct.rds")
saveRDS(model_disc_male_propCals_calsAbove16,
        "models/subgroups/manualPED/model_disc_male_propCals_calsAbove16.rds")
saveRDS(model_disc_male_propCals_calsAbove70pct,
        "models/subgroups/manualPED/model_disc_male_propCals_calsAbove70pct.rds")
