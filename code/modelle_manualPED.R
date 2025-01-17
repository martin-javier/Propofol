## Patient Died ####


### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_propDays_calsLower16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV + 
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    Propofol + 
    CalsAbove16kcalPerKG + 
    ProteinBelow0.8GperKG,
    # s(CombinedicuID, bs = "re"),
  data = manualPED_death,
  family = poisson(),
  offset = offset
  # discrete = TRUE # Läuft nur so durch mit Random Effekt (verbessert Laufzeit)
)
# mit random effekt devianz = -12.1%, ohne devianz = 5.37%

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_propDays_calsLower70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG,
  data = manualPED_death,
  family = poisson(),
  offset = offset
)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_death_propCals_calsLower16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV + 
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    PropofolCal + 
    CalsAbove16kcalPerKG + 
    ProteinBelow0.8GperKG,
  data = manualPED_death,
  family = poisson(),
  offset = offset
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_death_propCals_calsLower70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG,
  data = manualPED_death,
  family = poisson(),
  offset = offset
)


## Patient Discharged ####

### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_propDays_calsLower16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV + 
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    Propofol + 
    CalsAbove16kcalPerKG + 
    ProteinBelow0.8GperKG,
  data = manualPED_disc,
  family = poisson(),
  offset = offset
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_propDays_calsLower70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    Propofol +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG,
  data = manualPED_disc,
  family = poisson(),
  offset = offset
)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
model_disc_propCals_calsLower16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV + 
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    PropofolCal + 
    CalsAbove16kcalPerKG + 
    ProteinBelow0.8GperKG,
  data = manualPED_disc,
  family = poisson(),
  offset = offset
)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
model_disc_propCals_calsLower70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps") +
    s(BMI, bs = "ps") +
    s(ApacheIIScore, bs = "ps") +
    inMV +
    ParenteralNut +
    OralIntake +
    Sex +
    Year +
    AdmCatID +
    LeadAdmDiag +
    PropofolCal +
    CalsPercentageAbove70 +
    ProteinBelow0.8GperKG,
  data = manualPED_disc,
  family = poisson(),
  offset = offset
)
