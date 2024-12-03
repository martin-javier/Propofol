library(pammtools)
library(tidyverse)
library(mgcv)
library(survival)
library(gratia)  # For GAM visualization
library(ggplot2) # For additional plot customization

# Load the data
data <- readRDS("data/mergedAndCleanedData.Rds")

# Filter data based on inclusion criteria
data_EK <- data %>%
  filter(Age >= 18, BMI > 13 , DaysInICU >= 7, Study_Day <= 7)

# Add row_id
data_EK <- data_EK %>%
  mutate(row_id = row_number())

# Create binary variable for Protein Intake <30%
data_EK <- data_EK %>%
  mutate(ProteinIntakeBelow30 = ifelse(proteinAdjustedPercentage < 30, 1, 0))

# Transform data to piece-wise exponential format
ped <- as_ped(
  data = data_EK,
  formula = Surv(Study_Day, PatientDied) ~ Age,
  id = "row_id"
)

# Create tstart and tend for piece-wise exponential modeling
data_EK <- data_EK %>%
  group_by(CombinedID) %>%
  mutate(tstart = Study_Day - 1,   # Time interval definition
         tend = Study_Day) %>%
  ungroup()

# Create PED data
ped_EK <- as_ped(
  data = data_EK,
  formula = Surv(tstart, tend, PatientDied) ~ Age + BMI + ApacheIIScore + 
    DaysMechVent + OralIntake + PN + Gender + Year + AdmCatID + DiagID2 + 
    ProteinIntakeBelow30,
  id = "row_id"
)

# Model 1: Age-only model
model_age <- gam(
  ped_status ~ s(Age, bs = "ps"),
  data = ped_EK,
  family = poisson(),
  offset = offset
)

# Model 2: Full confounder model
model_confounders <- gam(
  ped_status ~ s(Age, bs = "ps") + s(BMI, bs = "ps") + s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + s(OralIntake, bs = "ps") + 
    s(PN, bs = "ps") + factor(Gender) + factor(Year) + 
    factor(AdmCatID) + factor(DiagID2) + factor(ProteinIntakeBelow30),
  data = ped_EK,
  family = poisson(),
  offset = offset
)

# Subgroup Analysis: Female patients
data_female <- data_EK %>%
  filter(Gender == "Female") %>%
  mutate(row_id = row_number())

ped_female <- as_ped(
  data = data_female,
  formula = Surv(tstart, tend, PatientDied) ~ Age + BMI + ApacheIIScore + 
    DaysMechVent + OralIntake + PN + Year + AdmCatID + DiagID2 + 
    ProteinIntakeBelow30,
  id = "row_id"
)

model_female <- gam(
  ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + 
    s(ApacheIIScore, bs = "ps", k = 5) + s(DaysMechVent, bs = "ps", k = 5) + 
    s(OralIntake, bs = "ps", k = 5) + s(PN, bs = "ps", k = 5) + 
    factor(Year) + factor(AdmCatID) + factor(DiagID2) + factor(ProteinIntakeBelow30),
  data = ped_female,
  family = poisson(),
  offset = offset
)

# Model 3: Including Propofol Calories
data_EK_b <- data_EK %>%
  group_by(CombinedID) %>%
  mutate(TotalCalories = sum(Calories, na.rm = TRUE)) %>%
  ungroup()

ped_b <- as_ped(
  data = data_EK_b,
  formula = Surv(tstart, tend, PatientDied) ~ Age + BMI + ApacheIIScore + 
    DaysMechVent + OralIntake + PN + Propofol2_4 + TotalCalories + 
    Gender + Year + AdmCatID + DiagID2 + ProteinIntakeBelow30,
  id = "row_id"
)

model_b <- gam(
  ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + 
    s(ApacheIIScore, bs = "ps", k = 5) + s(DaysMechVent, bs = "ps", k = 5) + 
    s(OralIntake, bs = "ps", k = 5) + s(PN, bs = "ps", k = 5) + 
    s(TotalCalories, bs = "ps", k = 5) + factor(Gender) + 
    factor(Year) + factor(AdmCatID) + factor(DiagID2) + factor(ProteinIntakeBelow30),
  data = ped_b,
  family = poisson(),
  offset = offset
)


# Summarize all models
summary(model_age)
summary(model_confounders)
summary(model_female)
summary(model_b)

            # > summary(model_age)
            # 
            # Family: poisson 
            # Link function: log 
            # 
            # Formula:
            #   ped_status ~ s(Age, bs = "ps")
            # 
            # Parametric coefficients:
            #   Estimate Std. Error z value Pr(>|z|)    
            # (Intercept) -1.421880   0.007115  -199.8   <2e-16 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # Approximate significance of smooth terms:
            #   edf Ref.df Chi.sq p-value    
            # s(Age) 8.229  8.666   2362  <2e-16 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # R-sq.(adj) =  0.0384   Deviance explained = 4.12%
            # UBRE = -0.3309  Scale est. = 1         n = 87318
            # > summary(model_confounders)
            # 
            # Family: poisson 
            # Link function: log 
            # 
            # Formula:
            #   ped_status ~ s(Age, bs = "ps") + s(BMI, bs = "ps") + s(ApacheIIScore, 
            #                                                          bs = "ps") + s(DaysMechVent, bs = "ps") + s(OralIntake, bs = "ps") + 
            #   s(PN, bs = "ps") + factor(Gender) + factor(Year) + factor(AdmCatID) + 
            #   factor(DiagID2) + factor(ProteinIntakeBelow30)
            # 
            # Parametric coefficients:
            #   Estimate Std. Error z value Pr(>|z|)    
            # (Intercept)                         -1.42733    0.02859 -49.929  < 2e-16 ***
            #   factor(Gender)Male                   0.03066    0.01390   2.205 0.027445 *  
            #   factor(Year)2008                    -0.09142    0.02454  -3.725 0.000195 ***
            #   factor(Year)2009                    -0.11391    0.02466  -4.619 3.86e-06 ***
            #   factor(Year)2011                    -0.13443    0.02310  -5.819 5.91e-09 ***
            #   factor(Year)2013                    -0.09006    0.02320  -3.882 0.000104 ***
            #   factor(Year)2014                    -0.05742    0.02343  -2.450 0.014270 *  
            #   factor(AdmCatID)Surgical/Elective   -0.31591    0.02633 -11.999  < 2e-16 ***
            #   factor(AdmCatID)Surgical/Emeregency -0.23430    0.02014 -11.632  < 2e-16 ***
            #   factor(DiagID2)Cardio-Vascular      -0.01120    0.02576  -0.435 0.663661    
            # factor(DiagID2)Respiratory          -0.12342    0.02473  -4.990 6.04e-07 ***
            #   factor(DiagID2)Gastrointestinal      0.03313    0.02802   1.182 0.237165    
            # factor(DiagID2)Neurologic           -0.08150    0.02812  -2.899 0.003748 ** 
            #   factor(DiagID2)Sepsis               -0.01034    0.02763  -0.374 0.708181    
            # factor(DiagID2)Orthopedic/Trauma    -0.40416    0.03371 -11.991  < 2e-16 ***
            #   factor(DiagID2)Metabolic            -0.33737    0.06905  -4.886 1.03e-06 ***
            #   factor(DiagID2)Renal                 0.22593    0.06629   3.408 0.000654 ***
            #   factor(ProteinIntakeBelow30)1        0.09173    0.01448   6.335 2.37e-10 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # Approximate significance of smooth terms:
            #   edf Ref.df  Chi.sq  p-value    
            # s(Age)           7.893  8.365 1613.22  < 2e-16 ***
            #   s(BMI)           7.868  8.083  213.81  < 2e-16 ***
            #   s(ApacheIIScore) 4.804  5.532  649.09  < 2e-16 ***
            #   s(DaysMechVent)  8.990  9.000 2430.73  < 2e-16 ***
            #   s(OralIntake)    1.000  1.000   17.68 2.66e-05 ***
            #   s(PN)            1.000  1.000   25.20 5.15e-07 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # R-sq.(adj) =  0.119   Deviance explained = 14.8%
            # UBRE = -0.40479  Scale est. = 1         n = 87318
            # > summary(model_female)
            # 
            # Family: poisson 
            # Link function: log 
            # 
            # Formula:
            #   ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + 
            #   s(ApacheIIScore, bs = "ps", k = 5) + s(DaysMechVent, bs = "ps", 
            #                                          k = 5) + s(OralIntake, bs = "ps", k = 5) + s(PN, bs = "ps", 
            #                                                                                       k = 5) + factor(Year) + factor(AdmCatID) + factor(DiagID2) + 
            #   factor(ProteinIntakeBelow30)
            # 
            # Parametric coefficients:
            #   Estimate Std. Error z value Pr(>|z|)    
            # (Intercept)                         -1.327874   0.042234 -31.441  < 2e-16 ***
            #   factor(Year)2008                    -0.082396   0.039458  -2.088 0.036781 *  
            #   factor(Year)2009                    -0.145704   0.038763  -3.759 0.000171 ***
            #   factor(Year)2011                    -0.160553   0.036205  -4.435 9.23e-06 ***
            #   factor(Year)2013                    -0.060654   0.036334  -1.669 0.095048 .  
            # factor(Year)2014                    -0.029854   0.037608  -0.794 0.427302    
            # factor(AdmCatID)Surgical/Elective   -0.287398   0.041824  -6.872 6.35e-12 ***
            #   factor(AdmCatID)Surgical/Emeregency -0.225786   0.033238  -6.793 1.10e-11 ***
            #   factor(DiagID2)Cardio-Vascular       0.041064   0.041301   0.994 0.320093    
            # factor(DiagID2)Respiratory          -0.145527   0.039194  -3.713 0.000205 ***
            #   factor(DiagID2)Gastrointestinal      0.036697   0.045824   0.801 0.423230    
            # factor(DiagID2)Neurologic           -0.047748   0.043120  -1.107 0.268153    
            # factor(DiagID2)Sepsis                0.008499   0.043159   0.197 0.843883    
            # factor(DiagID2)Orthopedic/Trauma    -0.199934   0.058537  -3.416 0.000637 ***
            #   factor(DiagID2)Metabolic            -0.583562   0.118723  -4.915 8.86e-07 ***
            #   factor(DiagID2)Renal                 0.322177   0.095554   3.372 0.000747 ***
            #   factor(ProteinIntakeBelow30)1        0.076115   0.023145   3.289 0.001007 ** 
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # Approximate significance of smooth terms:
            #   edf Ref.df  Chi.sq  p-value    
            # s(Age)           3.850  3.984 568.075  < 2e-16 ***
            #   s(BMI)           3.958  3.998  51.207  < 2e-16 ***
            #   s(ApacheIIScore) 3.907  3.994 245.232  < 2e-16 ***
            #   s(DaysMechVent)  3.995  4.000 901.577  < 2e-16 ***
            #   s(OralIntake)    1.000  1.000  12.865 0.000335 ***
            #   s(PN)            1.000  1.000   7.599 0.005840 ** 
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # R-sq.(adj) =  0.0984   Deviance explained = 11.6%
            # UBRE = -0.37751  Scale est. = 1         n = 33390
            # > summary(model_b)
            # 
            # Family: poisson 
            # Link function: log 
            # 
            # Formula:
            #   ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + 
            #   s(ApacheIIScore, bs = "ps", k = 5) + s(DaysMechVent, bs = "ps", 
            #                                          k = 5) + s(OralIntake, bs = "ps", k = 5) + s(PN, bs = "ps", 
            #                                                                                       k = 5) + s(TotalCalories, bs = "ps", k = 5) + factor(Gender) + 
            #   factor(Year) + factor(AdmCatID) + factor(DiagID2) + factor(ProteinIntakeBelow30)
            # 
            # Parametric coefficients:
            #   Estimate Std. Error z value Pr(>|z|)    
            # (Intercept)                         -1.371903   0.028050 -48.910  < 2e-16 ***
            #   factor(Gender)Male                   0.028892   0.013892   2.080 0.037540 *  
            #   factor(Year)2008                    -0.079396   0.024521  -3.238 0.001205 ** 
            #   factor(Year)2009                    -0.105960   0.024651  -4.298 1.72e-05 ***
            #   factor(Year)2011                    -0.135430   0.023101  -5.862 4.56e-09 ***
            #   factor(Year)2013                    -0.088787   0.023175  -3.831 0.000128 ***
            #   factor(Year)2014                    -0.059448   0.023475  -2.532 0.011328 *  
            #   factor(AdmCatID)Surgical/Elective   -0.310637   0.026273 -11.823  < 2e-16 ***
            #   factor(AdmCatID)Surgical/Emeregency -0.228814   0.020156 -11.352  < 2e-16 ***
            #   factor(DiagID2)Cardio-Vascular      -0.003380   0.025816  -0.131 0.895822    
            # factor(DiagID2)Respiratory          -0.126605   0.024757  -5.114 3.16e-07 ***
            #   factor(DiagID2)Gastrointestinal      0.044873   0.028030   1.601 0.109401    
            # factor(DiagID2)Neurologic           -0.078940   0.028131  -2.806 0.005013 ** 
            #   factor(DiagID2)Sepsis               -0.007601   0.027678  -0.275 0.783605    
            # factor(DiagID2)Orthopedic/Trauma    -0.408937   0.033746 -12.118  < 2e-16 ***
            #   factor(DiagID2)Metabolic            -0.338839   0.069051  -4.907 9.24e-07 ***
            #   factor(DiagID2)Renal                 0.202504   0.066417   3.049 0.002296 ** 
            #   factor(ProteinIntakeBelow30)1        0.086118   0.014463   5.954 2.61e-09 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # Approximate significance of smooth terms:
            #   edf Ref.df  Chi.sq  p-value    
            # s(Age)           3.816  3.976 1574.97  < 2e-16 ***
            #   s(BMI)           3.814  3.968   83.40  < 2e-16 ***
            #   s(ApacheIIScore) 3.859  3.985  648.92  < 2e-16 ***
            #   s(DaysMechVent)  3.995  4.000 2548.24  < 2e-16 ***
            #   s(OralIntake)    1.000  1.000   16.50 4.92e-05 ***
            #   s(PN)            1.000  1.000   25.85 3.69e-07 ***
            #   s(TotalCalories) 3.635  3.892   17.72  0.00076 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # R-sq.(adj) =  0.114   Deviance explained = 13.7%
            # UBRE = -0.39713  Scale est. = 1         n = 87318