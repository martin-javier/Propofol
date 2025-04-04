# Models fit on manually created ped


## Patient Died ####

### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_propDays_calsAbove16 <- bam(
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
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_propDays_calsAbove70pct <- bam(
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
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)


### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_propCals_calsAbove16 <- bam(
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
    s(PropofolCal, bs = "ps") + 
    CalsAbove16kcalPerKG + 
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_propCals_calsAbove70pct <- bam(
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
    s(PropofolCal, bs = "ps") +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)


## Patient Discharged ####

### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_propDays_calsAbove16 <- bam(
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
  data = manualPED_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_propDays_calsAbove70pct <- bam(
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
  data = manualPED_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)


### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_propCals_calsAbove16 <- bam(
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
    s(PropofolCal, bs = "ps") + 
    CalsAbove16kcalPerKG + 
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_propCals_calsAbove70pct <- bam(
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
    s(PropofolCal, bs = "ps") +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG +
    s(CombinedicuID, bs = "re"),
  data = manualPED_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)


# Save Models ####

# Patient Died models
saveRDS(model_death_propDays_calsAbove16,
        "models/manualPED/model_death_propDays_calsAbove16.rds")
saveRDS(model_death_propDays_calsAbove70pct,
        "models/manualPED/model_death_propDays_calsAbove70pct.rds")
saveRDS(model_death_propCals_calsAbove16,
        "models/manualPED/model_death_propCals_calsAbove16.rds")
saveRDS(model_death_propCals_calsAbove70pct,
        "models/manualPED/model_death_propCals_calsAbove70pct.rds")

# Patient Discharged models
saveRDS(model_disc_propDays_calsAbove16,
        "models/manualPED/model_disc_propDays_calsAbove16.rds")
saveRDS(model_disc_propDays_calsAbove70pct,
        "models/manualPED/model_disc_propDays_calsAbove70pct.rds")
saveRDS(model_disc_propCals_calsAbove16,
        "models/manualPED/model_disc_propCals_calsAbove16.rds")
saveRDS(model_disc_propCals_calsAbove70pct,
        "models/manualPED/model_disc_propCals_calsAbove70pct.rds")
