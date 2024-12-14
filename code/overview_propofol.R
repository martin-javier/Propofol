### Create data_long with main.Rmd first


# overview over Propofol and Propofol Calories
binary_overview <- data_long %>%
  group_by(Study_Day) %>%
  summarise(
    PatientsOnMedication = sum(as.numeric(as.character(Propofol)), na.rm = TRUE),  
    TotalPatients = n(),  
    PercentOnMedication = 100 * mean(as.numeric(as.character(Propofol)), na.rm = TRUE)
  )

# Overview for the PropofolCals variable
cals_overview <- data_long %>%
  group_by(Study_Day) %>%
  summarise(
    AverageCals = mean(PropofolCal, na.rm = TRUE),
    MedianCals = median(PropofolCal, na.rm = TRUE),
    CalsRange = paste0(min(PropofolCal, na.rm = TRUE), " - ", max(PropofolCal, na.rm = TRUE))
  )

# filter out patients that have PropofolCal = 0 for all Study Days
pat_w_PropofolCals <- data_long %>%
  group_by(CombinedID) %>%
  filter(any(PropofolCal != 0)) %>%
  ungroup()
# length(unique(pat_w_PropofolCals$CombinedID)) = 6018
# => die hälfte der Patienten hat überhaupt einmal Propofol bekommen

cals_overview_no0s <- pat_w_PropofolCals %>%
  group_by(Study_Day) %>%
  summarise(
    AverageCals = mean(PropofolCal, na.rm = TRUE),
    MedianCals = median(PropofolCal, na.rm = TRUE),
    CalsRange = paste0(min(PropofolCal, na.rm = TRUE), " - ", max(PropofolCal, na.rm = TRUE))
  )


## Plots

ggplot(binary_overview, aes(x = Study_Day, y = PercentOnMedication)) +
  geom_line() + geom_point() +
  labs(title = "Percentage of Patients Receiving Propofol Over Time",
       x = "Study Day", y = "Percentage (%)") +
  scale_x_continuous(breaks = seq(1, 11, by = 1)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme_bw()

# this makes litte sense
ggplot(cals_overview, aes(x = Study_Day, y = AverageCals)) +
  geom_line() + geom_point() +
  labs(title = "Average Calories Over Time", x = "Study Day",
       y = "Average Calories") +
  scale_x_continuous(breaks = seq(1, 11, by = 1)) +
  theme_bw()

# take 20 random patients (that have taken Propofol at least once)
# 6 times and plot the evolution of Propofol Cals as lines
#set.seed(24)
unique_patients <- unique(pat_w_PropofolCals$CombinedID)
# Randomly sample 10 patients, repeated 6 times
sampled_patients <- split(sample(unique_patients, 60), 1:6)
par(mfrow = c(2, 3))
for (i in seq_along(sampled_patients)) {
  group <- sampled_patients[[i]]
  subset_data <- data_long[data_long$CombinedID %in% group, ]
  
  plot(NULL, xlim = c(1, 11), ylim = range(subset_data$PropofolCal, na.rm = TRUE),
       xlab = "Study Day", ylab = "PropofolCal",
       main = paste("Group", i, "- Random 10 Patients"))
  
  for (patient in group) {
    patient_data <- subset_data[subset_data$CombinedID == patient, ]
    lines(patient_data$Study_Day, patient_data$PropofolCal,
          col = sample(colors(), 1), lwd = 1.5)
  }
}
