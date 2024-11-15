library(pammtools)
library(tidyverse)
library(mgcv)

data <- readRDS("data\\mergedAndCleanedData.Rds")

data_EK <- data_EK <- data %>%
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

model_age <- gam(
  ped_status ~ s(Age, bs = "ps"),
  data = ped,
  family = poisson(),
  offset = offset
)


