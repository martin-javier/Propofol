# Load necessary libraries
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(patchwork)

# Load the data
daily <- readRDS("data/daily.Rds")
ICU <- readRDS("data/ICU.Rds")
data <- readRDS("data/mergedAndCleanedData.Rds")
patient <- readRDS("data/patient.Rds")


# Alter
# Verteilung Alter
patient_unique <- patient %>%
  distinct(CombinedID, .keep_all = TRUE)

age_counts <- patient_unique %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(17, 30, 40, 50, 60, 70, 80, 90, 102), 
                        labels = c("18-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-102"))) %>% 
  count(AgeGroup)


# Alter
Alter <- ggplot(age_counts, aes(x = AgeGroup, y = n)) +
  geom_bar(stat = "identity", fill = "#56B4E9", color = "black", alpha = 0.7) +
  #geom_text(aes(label = n), vjust = -0.5, size = 3) +  
  labs(title = "Altersgruppen", x = "Altersgruppe", y = "Anzahl Patienten") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30))


# BMI
BMI <- ggplot(patient_unique, aes(x = BMI)) +
  geom_histogram(binwidth = 5, fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "BMI", x = "BMI", y = "Anzahl Patienten") +
  #scale_x_continuous(breaks = seq(10, 110, by = 5)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0))

# APACHE II
APACHE <- ggplot(patient_unique, aes(x = ApacheIIScore)) +
  geom_histogram(binwidth = 5, fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "Apache II Score", x = "Apache II Score", y = "Anzahl Patienten") +
  #scale_x_continuous(breaks = seq(10, 110, by = 5)) +
  xlim(1, 71) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0))

# Gender
Gender <- ggplot(patient_unique, aes(x = factor(Gender, labels = c("Female", "Male")))) +
  geom_histogram(stat = "count",fill = "#56B4E9", color = "black") +
  labs(title = "Geschlecht", x = "Geschlecht", y = "Anzahl Patienten") +
  #scale_x_continuous(breaks = seq(10, 110, by = 5)) +
  ylim(0, 11000) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0))

# Jahr
Jahr <- ggplot(patient_unique, aes(x = factor(Year, levels = c("2007", "2008", "2009", "2010", "2011", "2013", "2014")))) +
  geom_histogram(stat = "count",fill = "#56B4E9", color = "black") +
  labs(title = "Jahr der Behandlung", x = "Year", y = "Anzahl Patienten") +
  #scale_x_continuous(breaks = seq(10, 110, by = 5)) +
  ylim(0, 4000) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0))

# Admission
Admission <- ggplot(patient_unique, aes(x = factor(AdmCatID, levels = c("Medical", "Surgical/Elective", "Surgical/Emeregency")))) +
  geom_histogram(stat = "count",fill = "#56B4E9", color = "black") +
  labs(title = "Admission category", x = "admission category", y = "Anzahl Patienten") +
  #scale_x_continuous(breaks = seq(10, 110, by = 5)) +
  ylim(0, 12000) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 15))

# Combine plots with patchwork
combined_plot <- (Alter | BMI | APACHE) / (Gender | Jahr | Admission)

ggsave("confounders.png")




