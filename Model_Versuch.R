library(pammtools)
library(tidyverse)
library(mgcv)
library(survival)
library(gratia)  # For GAM visualization
library(ggplot2) # For additional plot customization

data <- readRDS("data\\mergedAndCleanedData.Rds")

data_EK <- data %>%
  filter(Age >= 18, BMI > 13 , DaysInICU >= 7, Study_Day <= 7)
# min(data$BMI = 13.06431 zweiter Filter nicht benötigt
# length(unique(data_EK$CombinedID)) = 12474 Patienten in dem data set mit Einschlusskriterium

data_EK <- data_EK %>%
  mutate(row_id = row_number())

ped <- as_ped(
  data = data_EK,
  formula = Surv(Study_Day, PatientDied) ~ Age,
  id = "row_id"
)
# 
# model_age <- gam(
#   ped_status ~ s(Age, bs = "ps"),
#   data = ped,
#   family = poisson(),
#   offset = offset
# )
data_EK <- data_EK %>%
  group_by(CombinedID) %>%
  mutate(tstart = Study_Day - 1,   # Beispielhafte Zeitintervall-Definition
         tend = Study_Day) %>%
  ungroup()

model_age <- gam(
    ped_status ~ s(Age, bs = "ps"),
    data = ped_EK,
    family = poisson(),
    offset = offset
  )
  # Freiheitsgrade k hängen von der uniquen anzahl der Werte in tend ab tend = {1-7} also kann k max = 7 sein

# Komplexes Cox Modell mit Pamms

ped_EK <- as_ped(
  data = data_EK,
  formula = Surv(tstart, tend, PatientDied) ~ Age + BMI + ApacheIIScore + 
    DaysMechVent + OralIntake + PN +  
    Gender + Year + AdmCatID + DiagID2,
  id = "row_id"
) 
# FEHLT: protein intake <30% of Target (zur classification of protein intake vgl angehängte 
# Dokumente #4 und #5) zwischen Tag 0 und Tag 7 nach Aufnahme Intensivstation
# Geht auch maybe mit Surv(Study_Day, PatientDied) gibt aber warning  über tend
ped_EK %>% select(row_id, tstart, tend, interval)


model_confounders <- gam(
  ped_status ~ s(Age, bs = "ps") + s(BMI, bs = "ps") + s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + s(OralIntake, bs = "ps") + 
    s(PN, bs = "ps") +
    factor(Gender) + factor(Year) + factor(AdmCatID) + factor(DiagID2),
  data = ped_EK,  # Ped-Daten, die im Format `as_ped()` erstellt wurden
  family = poisson(),  # Poisson-Verteilung für Ereignismodelle
  offset = offset  # Berücksichtigt das Offset
)

summary(model_confounders)

            # Family: poisson 
            # Link function: log 
            # 
            # Formula:
            #   ped_status ~ s(Age, bs = "ps") + s(BMI, bs = "ps") + s(ApacheIIScore, 
            #                                                          bs = "ps") + s(DaysMechVent, bs = "ps") + s(OralIntake, bs = "ps") + 
            #   s(PN, bs = "ps") + factor(Gender) + factor(Year) + factor(AdmCatID) + 
            #   factor(DiagID2)
            # 
            # Parametric coefficients:
            #   Estimate Std. Error z value Pr(>|z|)    
            # (Intercept)                         -1.396515   0.028143 -49.622  < 2e-16 ***
            #   factor(Gender)Male                   0.029898   0.013902   2.151 0.031508 *  
            #   factor(Year)2008                    -0.091467   0.024543  -3.727 0.000194 ***
            #   factor(Year)2009                    -0.113684   0.024665  -4.609 4.04e-06 ***
            #   factor(Year)2011                    -0.135634   0.023102  -5.871 4.33e-09 ***
            #   factor(Year)2013                    -0.089312   0.023203  -3.849 0.000118 ***
            #   factor(Year)2014                    -0.056262   0.023434  -2.401 0.016356 *  
            #   factor(AdmCatID)Surgical/Elective   -0.308225   0.026304 -11.718  < 2e-16 ***
            #   factor(AdmCatID)Surgical/Emeregency -0.227824   0.020132 -11.317  < 2e-16 ***
            #   factor(DiagID2)Cardio-Vascular      -0.004535   0.025738  -0.176 0.860151    
            # factor(DiagID2)Respiratory          -0.127563   0.024721  -5.160 2.47e-07 ***
            #   factor(DiagID2)Gastrointestinal      0.042805   0.028008   1.528 0.126440    
            # factor(DiagID2)Neurologic           -0.088424   0.028097  -3.147 0.001649 ** 
            #   factor(DiagID2)Sepsis               -0.008381   0.027625  -0.303 0.761599    
            # factor(DiagID2)Orthopedic/Trauma    -0.405098   0.033708 -12.018  < 2e-16 ***
            #   factor(DiagID2)Metabolic            -0.337912   0.069044  -4.894 9.87e-07 ***
            #   factor(DiagID2)Renal                 0.222558   0.066294   3.357 0.000788 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # Approximate significance of smooth terms:
            #   edf Ref.df  Chi.sq  p-value    
            # s(Age)           7.862  8.335 1609.75  < 2e-16 ***
            #   s(BMI)           7.870  8.086  230.42  < 2e-16 ***
            #   s(ApacheIIScore) 4.848  5.571  658.79  < 2e-16 ***
            #   s(DaysMechVent)  8.990  9.000 2410.22  < 2e-16 ***
            #   s(OralIntake)    1.000  1.000   12.39 0.000431 ***
            #   s(PN)            1.000  1.000   16.36 5.28e-05 ***
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # R-sq.(adj) =  0.118   Deviance explained = 14.8%
            # UBRE = -0.40435  Scale est. = 1         n = 87318





#Subgruppenanalyse
data_female <- data_EK %>%
  filter(Gender == "Female")

data_female <- data_female %>%
  group_by(CombinedID) %>%
  mutate(tstart = Study_Day - 1,   # Beispielhafte Zeitintervall-Definition
         tend = Study_Day) %>%
  ungroup()

data_female <- data_female %>%
  mutate(row_id = row_number())

ped_female <- as_ped(
  data = data_female,
  formula = Surv(tstart, tend, PatientDied) ~ Age + BMI + ApacheIIScore + 
    DaysMechVent + OralIntake + PN +  
    Year + AdmCatID + DiagID2,
  id = "row_id"
) 


model_female <- gam(
  ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + s(ApacheIIScore, bs = "ps", k = 5) + 
    s(DaysMechVent, bs = "ps", k = 5) + s(OralIntake, bs = "ps", k = 5) + 
    s(PN, bs = "ps", k = 5) + factor(Year) + factor(AdmCatID) + factor(DiagID2),
  data = ped_female,
  family = poisson(),
  offset = offset
)
  # geht nicht anders wenn ich die Freiheitsgrade = 5 weglasse läuft der code nicht durch
# Warnmeldungen:
  # 1: In smooth.construct.ps.smooth.spec(object, dk$data, dk$knots) :
  # Basisdimension ist größer als die Zahl der unterschiedlichen Kovariaten
  # 2: In smooth.construct.ps.smooth.spec(object, dk$data, dk$knots) :
  # Basisdimension ist größer als die Zahl der unterschiedlichen Kovariaten
summary(model_female)
    
          # Family: poisson 
          # Link function: log 
          # 
          # Formula:
          #   ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + 
          #   s(ApacheIIScore, bs = "ps", k = 5) + s(DaysMechVent, bs = "ps", 
          #                                          k = 5) + s(OralIntake, bs = "ps", k = 5) + s(PN, bs = "ps", 
          #                                                                                       k = 5) + factor(Year) + factor(AdmCatID) + factor(DiagID2)
          # 
          # Parametric coefficients:
          #   Estimate Std. Error z value Pr(>|z|)    
          # (Intercept)                         -1.30122    0.04141 -31.420  < 2e-16 ***
          #   factor(Year)2008                    -0.08237    0.03946  -2.087 0.036845 *  
          #   factor(Year)2009                    -0.14651    0.03876  -3.780 0.000157 ***
          #   factor(Year)2011                    -0.16286    0.03620  -4.499 6.83e-06 ***
          #   factor(Year)2013                    -0.05989    0.03634  -1.648 0.099293 .  
          # factor(Year)2014                    -0.03101    0.03761  -0.824 0.409668    
          # factor(AdmCatID)Surgical/Elective   -0.28118    0.04179  -6.729 1.71e-11 ***
          #   factor(AdmCatID)Surgical/Emeregency -0.22019    0.03321  -6.630 3.37e-11 ***
          #   factor(DiagID2)Cardio-Vascular       0.04581    0.04127   1.110 0.267015    
          # factor(DiagID2)Respiratory          -0.14966    0.03917  -3.821 0.000133 ***
          #   factor(DiagID2)Gastrointestinal      0.04282    0.04581   0.935 0.349977    
          # factor(DiagID2)Neurologic           -0.05391    0.04309  -1.251 0.210895    
          # factor(DiagID2)Sepsis                0.01129    0.04315   0.262 0.793544    
          # factor(DiagID2)Orthopedic/Trauma    -0.20104    0.05855  -3.434 0.000595 ***
          #   factor(DiagID2)Metabolic            -0.58201    0.11872  -4.902 9.47e-07 ***
          #   factor(DiagID2)Renal                 0.31624    0.09553   3.310 0.000932 ***
          #   ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # Approximate significance of smooth terms:
          #   edf Ref.df  Chi.sq p-value    
          # s(Age)           3.839  3.982 568.188 < 2e-16 ***
          #   s(BMI)           3.956  3.998  56.306 < 2e-16 ***
          #   s(ApacheIIScore) 3.911  3.994 247.908 < 2e-16 ***
          #   s(DaysMechVent)  3.994  4.000 895.189 < 2e-16 ***
          #   s(OralIntake)    1.000  1.000  10.481 0.00121 ** 
          #   s(PN)            1.000  1.000   5.152 0.02321 *  
          #   ---
          #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
          # 
          # R-sq.(adj) =  0.098   Deviance explained = 11.6%
          # UBRE = -0.37725  Scale est. = 1         n = 33390





# Propofol-Kalorien (z. B. Summe der Kalorien zwischen Tag 0 und Tag 7)
data_EK_b <- data_EK %>%
  group_by(CombinedID) %>%
  mutate(TotalCalories = sum(Calories, na.rm = TRUE)) %>%
  ungroup()

# PED-Format für Modell B erstellen (Summe der Kalorien über die ersten 7 Tage nach Aufnahme)
ped_b <- as_ped(
  data = data_EK_b,
  formula = Surv(tstart, tend, PatientDied) ~ Age + BMI + ApacheIIScore + 
    DaysMechVent + OralIntake + PN + Propofol2_4 + TotalCalories +  
    Gender + Year + AdmCatID + DiagID2,
  id = "row_id"
)

# Modell erstellen (Poisson-Verteilung für Ereignismodelle)
model_b <- gam(
  ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + s(ApacheIIScore, bs = "ps", k = 5) + 
    s(DaysMechVent, bs = "ps", k = 5) + s(OralIntake, bs = "ps", k = 5) + 
    s(PN, bs = "ps", k = 5) + s(TotalCalories, bs = "ps", k = 5) +  # Kalorienaufnahme mit k=5
    factor(Gender) + factor(Year) + factor(AdmCatID) + factor(DiagID2),
  data = ped_b,  # PED-Daten für Modell B
  family = poisson(),  # Poisson-Verteilung für Ereignismodelle
  offset = offset  # Berücksichtigt das Offset
)

# Zusammenfassung des Modells
summary(model_b)

            # Family: poisson 
            # Link function: log 
            # 
            # Formula:
            #   ped_status ~ s(Age, bs = "ps", k = 5) + s(BMI, bs = "ps", k = 5) + 
            #   s(ApacheIIScore, bs = "ps", k = 5) + s(DaysMechVent, bs = "ps", 
            #                                          k = 5) + s(OralIntake, bs = "ps", k = 5) + s(PN, bs = "ps", 
            #                                                                                       k = 5) + s(TotalCalories, bs = "ps", k = 5) + factor(Gender) + 
            #   factor(Year) + factor(AdmCatID) + factor(DiagID2)
            # 
            # Parametric coefficients:
            #   Estimate Std. Error z value Pr(>|z|)    
            # (Intercept)                         -1.343810   0.027617 -48.659  < 2e-16 ***
            #   factor(Gender)Male                   0.028278   0.013891   2.036 0.041772 *  
            #   factor(Year)2008                    -0.079454   0.024521  -3.240 0.001194 ** 
            #   factor(Year)2009                    -0.105623   0.024653  -4.284 1.83e-05 ***
            #   factor(Year)2011                    -0.136345   0.023102  -5.902 3.59e-09 ***
            #   factor(Year)2013                    -0.087808   0.023177  -3.789 0.000151 ***
            #   factor(Year)2014                    -0.058107   0.023477  -2.475 0.013322 *  
            #   factor(AdmCatID)Surgical/Elective   -0.303024   0.026246 -11.546  < 2e-16 ***
            #   factor(AdmCatID)Surgical/Emeregency -0.222749   0.020143 -11.058  < 2e-16 ***
            #   factor(DiagID2)Cardio-Vascular       0.002896   0.025794   0.112 0.910594    
            # factor(DiagID2)Respiratory          -0.130381   0.024745  -5.269 1.37e-07 ***
            #   factor(DiagID2)Gastrointestinal      0.053938   0.028012   1.926 0.054161 .  
            # factor(DiagID2)Neurologic           -0.085210   0.028113  -3.031 0.002437 ** 
            #   factor(DiagID2)Sepsis               -0.005413   0.027672  -0.196 0.844903    
            # factor(DiagID2)Orthopedic/Trauma    -0.409207   0.033744 -12.127  < 2e-16 ***
            #   factor(DiagID2)Metabolic            -0.338875   0.069048  -4.908 9.21e-07 ***
            #   factor(DiagID2)Renal                 0.200442   0.066403   3.019 0.002540 ** 
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # Approximate significance of smooth terms:
            #   edf Ref.df  Chi.sq  p-value    
            # s(Age)           3.820  3.977 1571.63  < 2e-16 ***
            #   s(BMI)           3.840  3.976   93.91  < 2e-16 ***
            #   s(ApacheIIScore) 3.846  3.983  658.31  < 2e-16 ***
            #   s(DaysMechVent)  3.998  4.000 2531.15  < 2e-16 ***
            #   s(OralIntake)    1.000  1.000   11.44 0.000718 ***
            #   s(PN)            1.000  1.000   17.51 2.93e-05 ***
            #   s(TotalCalories) 3.577  3.857   16.92 0.001047 ** 
            #   ---
            #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            # 
            # R-sq.(adj) =  0.114   Deviance explained = 13.7%
            # UBRE = -0.39675  Scale est. = 1         n = 87318

# plot(model_b) oder plot(model_b, select = 43)
# Der positive Anstieg bedeutet, dass der Effekt von TotalCalories mit zunehmender 
# Kalorienzufuhr steigt und damit das Risiko des Todes steigt. Das bedeutet, dass 
# mehr Kalorien durch Propofol zugeführt werden, desto höher wird das Risiko für den Tod.