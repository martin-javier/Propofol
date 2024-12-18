# Bibliotheken laden
library(dplyr)
library(pammtools) # Für PED-Erstellung
library(mgcv)

final_data_subset <- final_data %>%
  select(CombinedID, Study_Day , PropofolCal) # Keep only the relevant columns

ped_data_test <- ped_data %>%
  left_join(final_data_subset, 
            by = c("CombinedID" = "CombinedID", "tend" = "Study_Day")) %>%
  mutate(totalPropofolCal = ifelse(!is.na(PropofolCal), PropofolCal, totalPropofolCal)) %>%
  select(-PropofolCal) # Temporäre Spalte entfernen




# fehlt noch rando effekt für ICU mit CombinedicuID
model_final_calories <- bam(
  formula = ped_status ~ s(Age, bs = "ps") + 
    s(BMI, bs = "ps") + 
    s(ApacheIIScore, bs = "ps") + 
    s(DaysMechVent, bs = "ps") + 
    s(Days_ParenteralNut, bs = "ps") +
    s(Days_OralIntake, bs = "ps") +
    factor(Sex) + factor(Year) + factor(AdmCat) + 
    factor(LeadAdmDiag) + s(Days_ProtIntakeBelow30, bs = "ps") + 
    s(totalPropofolCal, bs = "ps"),
  data = ped_data_test,
  family = poisson(),
  offset = offset
)

summary(model_final_calories)

# VERWENDEN VON INTERAKTION ZWISCHEN DAYS UND KALORIEN SIGNIFIKANT 
# leichte verbesserung

# # Modell 1: Ohne Interaktion
# model_no_interaction <- bam(
#   ped_status ~ s(Days_Propofol, bs = "ps") + 
#     s(totalPropofolCal, bs = "ps"),
#   data = ped_data_test,
#   family = poisson(),
#   offset = offset
# )
# 
# # Modell 2: Mit Interaktion
# model_limited <- bam(
#   ped_status ~ s(Days_Propofol, bs = "ps", k = 5) + 
#     s(totalPropofolCal, bs = "ps", k = 5) +
#     ti(Days_Propofol, totalPropofolCal, bs = "ps", k = 5),
#   data = ped_data_test,
#   family = poisson(),
#   offset = offset
# )
# 
# # Modelle vergleichen
# cat("Vergleich der Modelle mittels AIC:\n")
# print(AIC(model_no_interaction, model_limited))
          # df      AIC
          # model_no_interaction 9.331825 40228.96
          # model_limited        9.966187 40207.77
# 
# cat("\nVergleich der Modelle mittels Anova:\n")
# print(anova(model_no_interaction, model_limited, test = "Chisq"))
        # Analysis of Deviance Table
        # 
        # Model 1: ped_status ~ s(Days_Propofol, bs = "ps") + s(totalPropofolCal, 
        #                                                       bs = "ps")
        # Model 2: ped_status ~ s(Days_Propofol, bs = "ps", k = 5) + s(totalPropofolCal, 
        # bs = "ps", k = 5) + ti(Days_Propofol, totalPropofolCal, bs = "ps", k = 5)
        # Resid. Df     Resid. Dev      Df   Deviance   Pr(>Chi)    
        # 1    630655        33778                               
        # 2    630654        33756 0.61653     22.453   8.132e-07 ***
        #   ---
        #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





# final_data <- final_data %>%totalPropofolCal
#   mutate(CombinedKey = paste0(CombinedID, "_", Study_Day))
# 
# ped_data_test <- as_ped(
#    data = final_data,
#      Surv(daysToEvent, PatientDied) ~ .,
#      cut = 0:1, id = "CombinedKey" # Zeitintervall für 60 Tage (1 Tag-Schritte)
#    )
# 
# 
# model_final_test <- bam(
#   formula = ped_status ~ s(Age, bs = "ps") + 
#     s(BMI, bs = "ps") + 
#     s(ApacheIIScore, bs = "ps") + 
#     s(DaysMechVent, bs = "ps") + 
#     s(ParenteralNut, bs = "ps") +
#     s(OralIntake, bs = "ps") +
#     factor(Sex) + factor(Year) + factor(AdmCatID) + 
#     factor(LeadAdmDiag) + s(ProteinIntakeBelow30, bs = "ps") + 
#     s(PropofolCal, bs = "ps"),
#   data = ped_data_test,
#   family = poisson(),
#   offset = offset
# )
