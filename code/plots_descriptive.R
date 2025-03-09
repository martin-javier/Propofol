# All descriptive plots (some used in presentation slides)

# Custom ggplot theme for consistency in the plots
theme.main <- theme_stata(scheme = "s1color")
theme.adjusted <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 22),
                        axis.title.x = element_text(margin = margin(t = 20), size = 32), 
                        axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 22, angle = 0),
                        axis.title.y = element_text(margin = margin(r = 20), size = 32),
                        title = element_text(color = "black"),
                        plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
                        panel.grid.major = element_line(color = "darkgray", linewidth = 0.2), 
                        panel.grid.minor = element_line(color = "gray", linewidth  = 0.1),
                        plot.background = element_rect(fill = "white", color = NA))



# Age ####

## Barplot (Age split into groups):
# prepare data
age_counts <- data_summed_Day0To11 %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(17, 30, 40, 50, 60, 70, 80, 90, 102), 
                        labels = c("18-30", "31-40", "41-50", "51-60", "61-70",
                                   "71-80", "81-90", "91-102"))) %>% 
  count(AgeGroup)

dist_age_groups <- ggplot(age_counts, aes(x = AgeGroup, y = n)) +
  geom_bar(stat = "identity", fill = "#56B4E9", color = "black", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.3, size = 6) +  # Anzahl der Patienten über den Balken
  labs(title = "Verteilung der Altersgruppen", x = "Altersgruppe", y = "Anzahl Patienten") +
  theme.main + 
  theme.adjusted + 
  theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 18, angle = 0))

## histogram with density:
dist_age_hist <- data_summed_Day0To11 %>% select(Age) %>% ggplot(aes(Age)) +
  geom_histogram(aes(y = ..density..), fill = "#74C1E9", colour = 1, binwidth = 1, alpha = 0.8) +
  labs(title = "Verteilung Alter (Balkenbreite = 1 Jahr)", x = "Alter", y = "Dichte") +
  geom_density(color = "orange", lwd = 1.2, linetype = 1, ) + 
  theme.main + theme.adjusted

## Violin Plot:
# Unlike box plots that can only show summary statistics, 
# violin plots depict summary statistics and the density of each variable.

counts <- data_summed_Day0To11 %>%
  group_by(Sex) %>%
  summarize(Count = n())
means <- data_summed_Day0To11 %>%
  group_by(Sex) %>%
  summarize(Mean = mean(Age))

violin_age <- data_summed_Day0To11 %>% 
  select(Age, Sex) %>%
  ggplot(aes(x = Sex, y = Age, fill = Sex)) +
  geom_violin(trim = FALSE, alpha = 0.5, adjust = 0.7, scale = "count", color = "black") + 
  geom_boxplot(varwidth = TRUE, width = 0.2, outlier.shape = 16, outlier.size = 2,
               outlier.color = "black") +
  annotate(
    "text", 
    x = 1.13, y = min(data_summed_Day0To11$Age) - 5.5,
    label = paste0("n = ", counts$Count[counts$Sex == "Female"]),
    hjust = 0, vjust = 0.5,
    size = 8, color = "black"
  ) +
  annotate(
    "text", 
    x = 2.13, y = min(data_summed_Day0To11$Age) - 5.5,
    label = paste0("n = ", counts$Count[counts$Sex == "Male"]),
    hjust = 0, vjust = 0.5,
    size = 8, color = "black"
  ) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "black") +
  scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +
  scale_x_discrete(labels = c("Female" = "Frauen", "Male" = "Männer")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    title = "Verteilung des Alters nach Geschlecht",
    x = "Geschlecht",
    y = "Alter"
  ) +
  theme.main +
  theme.adjusted +
  theme(legend.position = "none")


# Sex ####

barpl_sex <- ggplot(data_summed_Day0To11, aes(x = Sex, fill = Sex)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(title = "Geschlechterverteilung", x = NULL, y = "Anzahl Patienten") +
  scale_x_discrete(labels = c("Weiblich", "Männlich")) +
  scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +
  theme.main + theme.adjusted + theme(legend.position = "none")


# BMI ####

## Barplot (BMI split into groups):
barpl_bmi_groups <- ggplot(
  data_summed_Day0To11, aes(x = cut(BMI, breaks = c(-Inf, 18.5, 25, 30, 35, Inf),
                                    labels = c("Untergewicht", "Normalgewicht", "Übergewicht",
                                               "Adipositas I", "Adipositas II")),
                            fill = cut(BMI, breaks = c(-Inf, 18.5, 25, 30, 35, Inf),
                                       labels = c("Untergewicht", "Normalgewicht", "Übergewicht",
                                                  "Adipositas I", "Adipositas II")))) +
  geom_bar(color = "black", alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, color = "black", size = 6) +
  labs(
    title = "Verteilung der Patienten nach BMI-Kategorien",
    x = "BMI-Kategorie",
    y = "Anzahl Patienten"
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(labels = c(
    "Untergewicht" = "Untergewicht\n(BMI < 18.5)",
    "Normalgewicht" = "Normalgewicht\n(BMI 18.5–24.9)",
    "Übergewicht" = "Übergewicht\n(BMI 25–29.9)",
    "Adipositas I" = "Adipositas I\n(BMI 30–34.9)",
    "Adipositas II" = "Adipositas II\n(BMI ≥ 35)"
  )) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 0.5)) +
  theme.main +
  theme.adjusted +
  theme(legend.position = "none")

## Boxplot:
# normal BMI range for Men and Women provided by WHO
normal_weight_men <- c(18.5, 24.9)
normal_weight_women <- c(18.5, 24.9)

boxplt_bmi <- data_summed_Day0To11 %>% 
  select(BMI, Sex) %>%
  ggplot(aes(x = Sex, y = BMI, fill = Sex)) +
  geom_boxplot(varwidth = TRUE, width = 0.4, outlier.shape = 16, outlier.size = 2,
               outlier.color = "black") +
  geom_segment(
    aes(x = 0.845, xend = 1.162, y = normal_weight_women[1], yend = normal_weight_women[1]),
    color = "black", linetype = "dashed", size = 1
  ) +
  geom_segment(
    aes(x = 0.845, xend = 1.162, y = normal_weight_women[2], yend = normal_weight_women[2]),
    color = "black", linetype = "dashed", size = 1
  ) +
  geom_segment(
    aes(x = 1.8, xend = 2.2, y = normal_weight_men[1], yend = normal_weight_men[1]),
    color = "black", linetype = "dashed", size = 1
  ) +
  geom_segment(
    aes(x = 1.8, xend = 2.2, y = normal_weight_men[2], yend = normal_weight_men[2]),
    color = "black", linetype = "dashed", size = 1
  ) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "black") +
  annotate(
    "text",
    x = 0.7,
    y = normal_weight_women[2] - 1.8,
    label = "Normalgewicht: 18.5-24.9",
    hjust = -1.25, size = 6, color = "black"
  ) +
  annotate(
    "text", 
    x = 1.729, 
    y = normal_weight_men[2] - 2,
    label = "Normalgewicht: 18.5-24.9", 
    hjust = -1.25, size = 6, color = "black"
  ) +
  scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +
  scale_x_discrete(labels = c("Female" = "Frauen", "Male" = "Männer")) +
  scale_y_continuous(breaks = seq(10, 110, by = 5)) +
  labs(
    title = "BMI-Verteilung nach Geschlecht",
    x = "Geschlecht",
    y = "BMI"
  ) +
  theme.main +
  theme.adjusted +
  theme(legend.position = "none")

# Perfect alignment of normal weight intervals for martin (code above works for others):

# data_summed_Day0To11 %>%
#   select(BMI, Sex) %>%
#   ggplot(aes(x = Sex, y = BMI, fill = Sex)) +
#   geom_boxplot(varwidth = TRUE, width = 0.4, outlier.shape = 16, outlier.size = 2,
#                outlier.color = "black") +
#   geom_segment(
#     aes(x = 0.845, xend = 1.162, y = normal_weight_women[1], yend = normal_weight_women[1]),
#     color = "black", linetype = "dashed", size = 1
#   ) +
#   geom_segment(
#     aes(x = 0.845, xend = 1.162, y = normal_weight_women[2], yend = normal_weight_women[2]),
#     color = "black", linetype = "dashed", size = 1
#   ) +
#   # Normalgewicht-Linien für Männer
#   geom_segment(
#     aes(x = 1.8, xend = 2.2, y = normal_weight_men[1], yend = normal_weight_men[1]),
#     color = "black", linetype = "dashed", size = 1
#   ) +
#   geom_segment(
#     aes(x = 1.8, xend = 2.2, y = normal_weight_men[2], yend = normal_weight_men[2]),
#     color = "black", linetype = "dashed", size = 1
#   ) +
#   stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "black") +
#   annotate(
#     "text",
#     x = 0.725,
#     y = normal_weight_women[2] - 2.9,
#     label = "Normalgewicht: 18.5 - 24.9",
#     hjust = -1.25, size = 6, color = "black"
#   ) +
#   # Notiz für Normalgewicht Männer
#   annotate(
#     "text",
#     x = 1.77,
#     y = normal_weight_men[2] - 2.9,
#     label = "Normalgewicht: 18.5 - 24.9",
#     hjust = -1.25, size = 6, color = "black"
#   ) +
#   scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +
#   scale_x_discrete(labels = c("Female" = "Frauen", "Male" = "Männer")) +
#   scale_y_continuous(breaks = seq(10, 110, by = 5)) +
#   labs(title = "BMI-Verteilung nach Geschlecht", x = "Geschlecht", y = "BMI") +
#   theme.main + theme.adjusted + theme(legend.position = "none")


# Events & right-censored ####
barpl_events <- ggplot(data_summed_Day0To11, aes(x = surv_icu_status_exp, fill = surv_icu_status_exp)) +
  geom_bar(color = "black", alpha = 0.8) +
  scale_x_discrete(labels = c("PatientDied" = "Verstorbene",
                              "PatientDischarged" = "Entlassene",
                              "PatientHospital" = "Rechtszensiert")) +
  labs(title = "Verteilung Patienten",
       x = NULL, 
       y = "Anzahl Patienten") +
  scale_fill_manual(values = c("PatientDied" = "#0072B2", 
                               "PatientDischarged" = "#E69F00",
                               "PatientHospital" = "#CC79A7")) +
  theme.main + 
  theme.adjusted + 
  theme(legend.position = "none")


# Propofol Intake Binary ####
prop_days <- ggplot(data_summed_Day0To11, aes(x = factor(Days_Propofol))) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "Verteilung Patienten",
    x = "Anzahl der Propofol-Tage",
    y = "Anzahl der Patienten"
  ) + theme.main + theme.adjusted


# Propofol Calories ####

# Filter only the days with Propofol administration
propofol_data <- subset(data_long, Propofol == 1)

## Propofol Calories per Study Day
boxplts_propcals <- ggplot(propofol_data, aes(x = as.factor(Study_Day), y = PropofolCal)) +
  geom_boxplot(fill = "#56B4E9", color = "black", outlier.size = 2, outlier.color = "black") +
  scale_y_continuous(breaks = seq(0, max(propofol_data$PropofolCal, na.rm = TRUE) + 500, 500)) +
  labs(title = "Verteilung der Propofol-Kalorien",
    x = "Beobachtungstag",
    y = "Propofol-Kalorien (kcal)"
  ) + theme.main + theme.adjusted


## Propofol Calories / Patients Weight (in kg)
prop_plt_data <- propofol_data %>% 
  mutate(PropCal_per_kg = PropofolCal / Weight)

daily_stats <- prop_plt_data %>%
  group_by(Study_Day) %>%
  summarise(
    median = median(PropCal_per_kg, na.rm = TRUE),
    q1 = quantile(PropCal_per_kg, 0.25, na.rm = TRUE),
    q3 = quantile(PropCal_per_kg, 0.75, na.rm = TRUE)
  )

propCals_per_kg <- ggplot(daily_stats, aes(x = Study_Day, y = median)) +
  geom_line(color = "#56B4E9", linewidth = 1) +
  geom_point(color = "#56B4E9", size = 3) +
  # Main errorbars with standard caps
  geom_errorbar(aes(ymin = q1, ymax = q3), 
                width = 0.2, 
                color = "#56B4E9",
                linewidth = 1) +
  # Special errorbar caps for first and last day
  geom_errorbar(data = filter(daily_stats, Study_Day %in% c(1, 11)),
                aes(ymin = q1, ymax = q3),
                width = 0.2,  # Keep original width
                color = "#56B4E9",
                linewidth = 1,
                lineend = "square") +  # Force square line ends
  # Axis adjustments
  scale_x_continuous(
    breaks = 1:11,
    limits = c(0.8, 11.2),  # Extended limits
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Tägliche Propofol-assoziierte Kalorienzufuhr pro kg Körpergewicht",
    x = "Beobachtungstag", 
    y = "kcal/kg  (Median + IQR)"
  ) +
  theme.main + theme.adjusted

## Quartile Distribution (seperated for each Study day)
# Calculate global quartile thresholds (based on all data)
quartile_breaks <- quantile(prop_plt_data$PropCal_per_kg, 
                            probs = seq(0, 1, 0.25), 
                            na.rm = TRUE)

# Create daily quartile counts
daily_quartiles <- prop_plt_data %>%
  mutate(quartile = cut(PropCal_per_kg,
                        breaks = quartile_breaks,
                        include.lowest = TRUE,
                        labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  count(Study_Day, quartile) %>%
  complete(Study_Day = 1:11, quartile, fill = list(n = 0))  # Ensure all days appear

# Create stacked bar plot
quart_dist_barplt <- ggplot(daily_quartiles, aes(x = factor(Study_Day), y = n, fill = quartile)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  scale_x_discrete(breaks = 1:11, expand = expansion(0)) +
  labs(title = "Tägliche Verteilung der Propofol-Kalorienquartile",
       x = "Beobachtungstag", y = "Anzahl der Patient*innen",
       fill = "Quartilgruppe") +
  theme.main + theme.adjusted +
  guides(fill = guide_legend(reverse = TRUE))  # Q1 at bottom in legend


# Admission Categories ####
barpl_admCats <- ggplot(data_summed_Day0To11, aes(x = AdmCat)) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  scale_x_discrete(labels =c("Medical" = "Medizinischer Eingriff",
                             "Surgical/Emeregency" = "Notfalloperation",
                             "Surgical/Elective" = "Geplante Operation")) +
  labs(title = "Verteilung Admissionsgründe", x = NULL, y = "Anzahl Patienten") +
  theme.main + theme.adjusted


# Kaplan-Meier Plots ####

## Kaplan-Meier survival probability:

km_data_death <- data_summed_Day0To11 %>%
  mutate(daysToEvent = if_else(PatientDischarged == 1, 61L, daysToEvent),
         roundedDaysToEvent = round(daysToEvent))

### KM with constant time variable:
km_death <- survfit(Surv(daysToEvent, PatientDied) ~ 1, data = km_data_death)
km_death_plot <- ggsurvplot(km_death, legend = "none",
           xlab = "Tage", ylab = "Überlebenswahrscheinlichkeit",
           title = "Kaplan-Meier Kurve Überlebenswahrscheinlichkeit",
           censor = FALSE, ggtheme = (theme.main + theme.adjusted))
km_death_plot <- km_death_plot$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_death$daysToEvent), by = 10)) +
  geom_line(size = 1.2, color = "#0072B2")

### KM with rounded time variable:
km_death_rounded <- survfit(Surv(roundedDaysToEvent, PatientDied) ~ 1, data = km_data_death)
km_death_plot_rounded <- ggsurvplot(
  km_death_rounded, legend = "none",
  xlab = "Tage (gerundet)", ylab = "Überlebenswahrscheinlichkeit",
  title = "Kaplan-Meier Kurve Überlebenswahrscheinlichkeit",
  censor = FALSE, ggtheme = (theme.main + theme.adjusted),
  lwd = 1.2, palette = "#0072B2"
)
km_death_plot_rounded <- km_death_plot_rounded$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_death$roundedDaysToEvent), by = 10))


## Kaplan Meier probability to reamin in ICU:
km_data_disc <- data_summed_Day0To11 %>%
  mutate(daysToEvent = if_else(PatientDied == 1, 61L, daysToEvent),
         roundedDaysToEvent = round(daysToEvent))

### KM with constant time variable:
km_disc <- survfit(Surv(daysToEvent, PatientDischarged) ~ 1, data = km_data_disc)
km_disc_plot <- ggsurvplot(km_disc, legend = "none",
           xlab = "Tage", ylab = "Verbleibswahrscheinlichkeit",
           title = "Kaplan-Meier Kurve Nicht-Entlassung",
           censor = FALSE, ggtheme = (theme.main + theme.adjusted))
km_disc_plot <- km_disc_plot$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_disc$daysToEvent), by = 10)) +
  geom_line(size = 1.2, color = "#E69F00")

### KM with rounded time variable:
km_disc_rounded <- survfit(Surv(roundedDaysToEvent, PatientDischarged) ~ 1, data = km_data_disc)
km_disc_plot_rounded <- ggsurvplot(
  km_disc_rounded, legend = "none",
  xlab = "Tage (gerundet)", ylab = "Verbleibswahrscheinlichkeit",
  title = "Kaplan-Meier Kurve Nicht-Entlassung",
  censor = FALSE, ggtheme = (theme.main + theme.adjusted),
  lwd = 1.2, palette = "#E69F00"
)
km_disc_plot_rounded <- km_disc_plot_rounded$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_disc$roundedDaysToEvent), by = 10))


# Cumulative Incidence Functions ####
# prepare data
cumu_data <- data_summed_Day0To11[data_summed_Day0To11$surv_icu_status != 0, ]
km <- survfit(formula = Surv(daysToEvent) ~ surv_icu_status,
              data = cumu_data)
cumu_inc <- ggsurvplot(km,
           data = data_summed_Day0To11,
#           risk.table = TRUE, # adds table below plot
           legend.labs = c("Entlassung", "Tod"),
           xlim = c(0,60),
           break.time.by = 10,
           ylim = c(0,1),
title = "Kumulative Inzidenzen für Tod und Entlassung",
           xlab = "Beobachtungstag",
           ylab = "Event Wahrscheinlichkeit",
           legend.title = "",
           palette = c("#E69F00", "#0072B2"),
           fun = "event",
           ggtheme = (theme.main + theme.adjusted +
                        theme(legend.text = element_text(size = 18),
                              legend.key.size = unit(0.8, "cm")))
)



# Autosave helpers ####

desc_plots <- list(
  dist_age_groups, dist_age_hist, violin_age, barpl_sex, barpl_bmi_groups,
  boxplt_bmi, barpl_events, prop_days, boxplts_propcals, propCals_per_kg,
  quart_dist_barplt, barpl_admCats, km_death_plot, km_death_plot_rounded,
  km_disc_plot, km_disc_plot_rounded, cumu_inc$plot
)
desc_plot_names <- c(
  "dist_age_groups", "dist_age_hist", "violin_age", "barpl_sex", "barpl_bmi_groups",
  "boxplt_bmi", "barpl_events", "prop_days", "boxplts_propcals", "propCals_per_kg",
  "quart_dist_barplt", "barpl_admCats","km_death_plot", "km_death_plot_rounded",
  "km_disc_plot", "km_disc_plot_rounded","cumu_inc"
)

