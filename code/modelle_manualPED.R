## Patient Died ####


### Propofol Days ####
# with Calorie Variable: Days where Calories where lower than 16 kcal/kg
model_death_propDays_calsLower16 <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) + 
    s(BMI, bs = "ps", k = 5) + 
    s(ApacheIIScore, bs = "ps", k = 5) + 
    factor(inMV) + 
    factor(ParenteralNut) +
    factor(OralIntake) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    factor(Propofol) + 
    factor(CalsAbove16kcalPerKG) + 
    factor(ProteinBelow0.8GperKG) +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  discrete = TRUE # Läuft nur so durch mit Random Effekt (verbessert Laufzeit)
)

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
model_death_propDays_calsLower70pct <- bam(
  formula = ped_status ~ s(Age, bs = "ps", k = 5) +
    s(BMI, bs = "ps", k = 5) +
    s(ApacheIIScore, bs = "ps", k = 5) +
    factor(inMV) +
    factor(ParenteralNut) +
    factor(OralIntake) +
    factor(Sex) +
    factor(Year) +
    factor(AdmCatID) +
    factor(LeadAdmDiag) +
    factor(Propofol) +
    factor(CalsPercentageAbove70) +
    factor(ProteinBelow0.8GperKG) +
    s(CombinedicuID, bs = "re"),
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  discrete = TRUE # Läuft nur so durch mit Random Effekt (verbessert Laufzeit)
)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were lower than 16 kcal/kg
# model_death_propCals_calsLower16 <- bam(
#   formula = ped_status ~ 
# )

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
# model_death_propCals_calsLower70pct <- bam(
#   formula = ped_status ~ 
# )

## Patient Discharged ####
# with Calorie Variable: Days where Calories where lower than 16 kcal/kg
# model_disc_propDays_calsLower16 <- bam(
#   formula = ped_status ~ 
# )

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
# model_disc_propDays_calsLower70pct <- bam(
#   formula = ped_status ~
# )

### Propofol Cals ####
# with Calorie Variable: Days where Calories were lower than 16 kcal/kg
# model_disc_propCals_calsLower16 <- bam(
#   formula = ped_status ~
# )

# with Calorie Variable: Days where Calorie Intake was below 70% of Target
# model_disc_propCals_calsLower70pct <- bam(
#   formula = ped_status ~ 
# )