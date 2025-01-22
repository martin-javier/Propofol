## Patient Died ####


### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
# model_death_propDays_calsAbove16 <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV + 
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     Propofol + 
#     CalsAbove16kcalPerKG + 
#     ProteinBelow0.8GperKG,
#   data = manualPED_death,
#   family = poisson(),
#   offset = offset
# )

# same model as above but with random effect for ICU
model_death_propDays_calsAbove16_reICU <- bam(
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
    s(CombinedicuID, bs = "re", by = icuByDummy),
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)

# Document ICC calculation (Ergebnis macht keinen Sinn!)
# vcomp <- gam.vcomp(model_death_propDays_calsAbove16_reICU)
# var_between_groups <- vcomp["s(CombinedicuID):icuByDummy", "std.dev"]^2
# icc <- var_between_groups / (var_between_groups + mean(fitted(model_death_propDays_calsAbove16_reICU)))
# icc
# This would imply ~95% of response variance is explained by group differences (ICUs),
# but this makes no sense as the model with random effect performs worse.

# Likelihood-Ratio-Test:
# anova(model_death_propDays_calsAbove16, model_death_propDays_calsAbove16_reICU)

# Observing residual deviance: 
# The second model (with random effect) has a larger residual deviance.
# This suggests that the random effect worsens the fit on the data.
# As the random effect does not improve the fit, the LRT cannot calculate
# a meaningful p-value.

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
# model_death_propDays_calsAbove70pct <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV +
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     Propofol +
#     CalsPercentageAbove70 +
#     ProteinBelow0.8GperKG,
#   data = manualPED_death,
#   family = poisson(),
#   offset = offset
# )
# same model as above but with random effect for ICU
model_death_propDays_calsAbove70pct_reICU <- bam(
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

# Likelihood-Ratio-Test:
# anova(model_death_propDays_calsAbove70pct, model_death_propDays_calsAbove70pct_reICU)


### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
# model_death_propCals_calsAbove16 <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV + 
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     PropofolCal + 
#     CalsAbove16kcalPerKG + 
#     ProteinBelow0.8GperKG,
#   data = manualPED_death,
#   family = poisson(),
#   offset = offset
# )
# same model as above but with random effect for ICU
model_death_propCals_calsAbove16_reICU <- bam(
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
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)
# Likelihood-Ratio-Test:
# anova(model_death_propCals_calsAbove16, model_death_propCals_calsAbove16_reICU)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
# model_death_propCals_calsAbove70pct <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV +
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     PropofolCal +
#     CalsPercentageAbove70 +
#     ProteinBelow0.8GperKG,
#   data = manualPED_death,
#   family = poisson(),
#   offset = offset
# )
# same model as above but with random effect for ICU
model_death_propCals_calsAbove70pct_reICU <- bam(
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
  data = manualPED_death,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)
# Likelihood-Ratio-Test:
# anova(model_death_propCals_calsAbove70pct, model_death_propCals_calsAbove70pct_reICU)

## Patient Discharged ####

### Propofol Days ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
# model_disc_propDays_calsAbove16 <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV + 
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     Propofol + 
#     CalsAbove16kcalPerKG + 
#     ProteinBelow0.8GperKG,
#   data = manualPED_disc,
#   family = poisson(),
#   offset = offset
# )
# same model as above but with random effect for ICU
model_disc_propDays_calsAbove16_reICU <- bam(
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
# Likelihood-Ratio-Test:
# anova(model_disc_propDays_calsAbove16, model_disc_propDays_calsAbove16_reICU)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
# model_disc_propDays_calsAbove70pct <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV +
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     Propofol +
#     CalsPercentageAbove70 +
#     ProteinBelow0.8GperKG,
#   data = manualPED_disc,
#   family = poisson(),
#   offset = offset
# )
# same model as above but with random effect for ICU
model_disc_propDays_calsAbove70pct_reICU <- bam(
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
# Likelihood-Ratio-Test:
# anova(model_disc_propDays_calsAbove70pct, model_disc_propDays_calsAbove70pct_reICU)

### Propofol Cals ####
# with Calorie Variable: Days where Calories were above 16 kcal/kg
# model_disc_propCals_calsAbove16 <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV + 
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     PropofolCal + 
#     CalsAbove16kcalPerKG + 
#     ProteinBelow0.8GperKG,
#   data = manualPED_disc,
#   family = poisson(),
#   offset = offset
# )
# same model as above but with random effect for ICU
model_disc_propCals_calsAbove16_reICU <- bam(
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
  data = manualPED_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)
# Likelihood-Ratio-Test:
# anova(model_disc_propCals_calsAbove16, model_disc_propCals_calsAbove16_reICU)

# with Calorie Variable: Days where Calorie Intake was above 70% of Target
# model_disc_propCals_calsAbove70pct <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") +
#     s(BMI, bs = "ps") +
#     s(ApacheIIScore, bs = "ps") +
#     inMV +
#     ParenteralNut +
#     OralIntake +
#     factor(Sex) +
#     factor(Year) +
#     factor(AdmCatID) +
#     factor(LeadAdmDiag) +
#     PropofolCal +
#     CalsPercentageAbove70 +
#     ProteinBelow0.8GperKG,
#   data = manualPED_disc,
#   family = poisson(),
#   offset = offset
# )
# same model as above but with random effect for ICU
model_disc_propCals_calsAbove70pct_reICU <- bam(
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
  data = manualPED_disc,
  family = poisson(),
  offset = offset,
  nthreads = parallel::detectCores()
)
# Likelihood-Ratio-Test:
# anova(model_disc_propCals_calsAbove70pct, model_disc_propCals_calsAbove70pct_reICU)
