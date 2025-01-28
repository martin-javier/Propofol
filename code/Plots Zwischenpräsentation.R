library(ggplot2)
library(survival)
library(survminer)
library(ggthemes)
library(cmprsk)

# data_EK erstellen in Model_Versuch.R

theme.main <- theme_stata(scheme = "s1color")
theme.adjusted <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
                        axis.title.x = element_text(margin = margin(t = 20), size = 22), 
                        axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
                        axis.title.y = element_text(margin = margin(r = 20), size = 22),
                        title = element_text(color = "black"),
                        plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
                        panel.grid.major = element_line(color = "darkgray", linewidth = 0.2), 
                        panel.grid.minor = element_line(color = "gray", linewidth  = 0.1),
                        plot.background = element_rect(fill = "beige", color = NA))

# Altersgruppen mit Counts erstellen
age_counts <- data_summed_Day0To11 %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(17, 30, 40, 50, 60, 70, 80, 90, 102), 
                        labels = c("18-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-102"))) %>% 
  count(AgeGroup)


# Verteilung Alter ####
# Barplot mit Count-Anzeige
ggplot(age_counts, aes(x = AgeGroup, y = n)) +
  geom_bar(stat = "identity", fill = "#56B4E9", color = "black", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +  # Anzahl der Patienten über den Balken
  labs(title = "Verteilung der Altersgruppen", x = "Altersgruppe", y = "Anzahl Patienten") +
  theme.main + 
  theme.adjusted + 
  theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 18, angle = 0))
  ## TO DO: Make the x-achse labels obliquely

# Histogramm
ggplot(data_summed_Day0To11, aes(x = Age)) +
  geom_histogram(binwidth = 4, fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "Verteilung Alter", x = "Alter", y = "Anzahl Patienten") +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  theme.main + theme.adjusted

# Histogramm mit Dichte
data_summed_Day0To11 %>% select(Age) %>% ggplot(aes(Age)) +
  geom_histogram(aes(y = ..density..), fill = "#74C1E9", colour = 1, binwidth = 1, alpha = 0.8) +
  labs(title = "Verteilung Alter (Binwidth = 1)", x = "Alter", y = "Dichte") +
  geom_density(color = "orange", lwd = 1.2, linetype = 1, ) + 
  theme.main + theme.adjusted


# ApacheIIScore ####
# Boxplot
ggplot(data_summed_Day0To11, aes(x = "", y = ApacheIIScore)) +
  # Boxplot zeichnen
  geom_boxplot(fill = "darkgrey", color = "black", coef = 1.5, alpha = 0.7, outlier.size = 3, outlier.shape = 16, lwd = 0.8) +
  
  # Mittelwert als roten Punkt anzeigen
  stat_summary(fun = mean, geom = "point", shape = 16, size = 7, color = "#FF6666") +
  
  # Median als Text anzeigen
  stat_summary(fun = median, geom = "text", aes(label = ..y..), vjust = -1.2, size = 5.2, color = "black") +
  labs(
    title = "Verteilung des ApacheIIScore",        # Haupttitel
    x = "",                                       # Leere X-Achse
    y = "ApacheIIScore")+
  scale_y_continuous(breaks = seq(0, max(data_summed_Day0To11$ApacheIIScore, na.rm = TRUE), by = 10)) +
  theme.main + 
  theme.adjusted

# oder Histogram
# binwidth = 2 bedeutet, dass jede Klasse (bin) eine Breite von 2 Einheiten hat.
# [10, 12), [12, 14), [14, 16), ..., [58, 60]
# ggplot(data_summed_Day0To11, aes(x = ApacheIIScore)) +
#   geom_histogram(binwidth = 2, fill = "#56B4E9", color = "black", alpha = 0.7) +
#   scale_x_continuous(breaks = seq(0, 80, by = 10)) +
#   labs(title = "Verteilung des ApacheIIScore", 
#        x = "ApacheIIScore", 
#        y = "Häufigkeit") +
#   theme.main + 
#   theme.adjusted


# Balkendiagramm für Gender ####
ggplot(data_summed_Day0To11, aes(x = Sex, fill = Sex)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(title = "Geschlechterverteilung", x = NULL, y = "Anzahl Patienten") +
  scale_x_discrete(labels = c("Weiblich", "Männlich")) +
  scale_fill_manual(values = c("Female" = "tomato", "Male" = "skyblue")) +
  theme.main + theme.adjusted + theme(legend.position = "none")


# Verstorbene vs. Entlassene vs. im Krankenhaus ####
ggplot(data_summed_Day0To11, aes(x = surv_icu_status_exp, fill = surv_icu_status_exp)) +
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


# Beziehung zwischen Alter und ApacheIIScore ####
# Scatterplot: Zeigt die Beziehung zwischen Alter und ApacheIIScore.
ggplot(data_summed_Day0To11, aes(x = Age, y = ApacheIIScore)) +
  geom_point(alpha = 0.5, color = "black") +  # Punkte plotten
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Lineare Trendlinie mit Konfidenzintervall
  labs(
    title = "Alter vs. ApacheIIScore",
    x = "Alter",
    y = "ApacheIIScore"
  ) +
  scale_x_continuous(
    breaks = seq(20, 100, by = 10),  
    labels = seq(20, 100, by = 10)) +
  theme.main + 
  theme.adjusted


# Plot Propofol-Tage ####
ggplot(data_summed_Day0To11, aes(x = factor(Days_Propofol))) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(
    title = "Verteilung der Propofol-Nutzung",
    x = "Anzahl der Propofol-Tage",
    y = "Anzahl der Patienten"
  ) + theme.main + theme.adjusted

# Verteilung Admission Categories ####
ggplot(data_summed_Day0To11, aes(x = AdmCat)) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  scale_x_discrete(labels =c("Medical" = "Medizinisch",
                             "Surgical/Emeregency" = "Notfalloperation",
                             "Surgical/Elective" = "Geplante Operation")) +
  labs(title = "Verteilung Admissionsgründe", x = NULL, y = "Anzahl Patienten") +
  theme.main + theme.adjusted


# Verteilung Leading admission Diagnosis ####
ggplot(data_summed_Day0To11, aes(x = LeadAdmDiag)) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "Verteilung Erstdiagnose", x = NULL, y = "Anzahl Patienten") +
  theme.main + theme.adjusted

# BMI Plot ####
ggplot(data_summed_Day0To11, aes(x = cut(BMI, 
                               breaks = c(-Inf, 18.5, 25, 30, 35, Inf),
                               labels = c("Untergewicht", "Normalgewicht", "Übergewicht", 
                                          "Adipositas I", "Adipositas II+")),
                       fill = cut(BMI, 
                                  breaks = c(-Inf, 18.5, 25, 30, 35, Inf),
                                  labels = c("Untergewicht", "Normalgewicht", "Übergewicht", 
                                             "Adipositas I", "Adipositas II+")))) +
  geom_bar(color = "black", alpha = 0.8) +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, color = "black") +
  labs(
    title = "Verteilung der Patienten nach BMI-Kategorien",
    x = "BMI-Kategorie",
    y = "Anzahl Patienten"
  ) +
  scale_fill_brewer(palette = "Set3") + # Farbpalette geeignet für Sehschwächen
  scale_x_discrete(labels = c(
    "Untergewicht" = "Untergewicht\n(BMI < 18.5)",
    "Normalgewicht" = "Normalgewicht\n(BMI 18.5–24.9)",
    "Übergewicht" = "Übergewicht\n(BMI 25–29.9)",
    "Adipositas I" = "Adipositas I\n(BMI 30–34.9)",
    "Adipositas II+" = "Adipositas II+\n(BMI ≥ 35)"
  )) +
  theme(
    legend.position = "none", # Entfernt die Legende
    axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 0.5)) + # Verbessert die Lesbarkeit der x-Achse 
  theme.main + 
  theme.adjusted + 
  theme(legend.position = "none")


# Violin Plot Alter ####
# Brief description of Violin Graph:
# A violin plot is a hybrid of a box plot and a kernel density plot, which shows peaks in the data. 
# It is used to visualize the distribution of numerical data. 
#Unlike a box plot that can only show summary statistics, 
#violin plots depict summary statistics and the density of each variable.

counts <- data_summed_Day0To11 %>%
  group_by(Sex) %>%
  summarize(Count = n())
means <- data_summed_Day0To11 %>%
  group_by(Sex) %>%
  summarize(Mean = mean(Age))

# Violinplot mit individueller Annotation pro Geschlecht
data_summed_Day0To11 %>% 
  select(Age, Sex) %>%
  ggplot(aes(x = Sex, y = Age, fill = Sex)) +
  geom_violin(trim = FALSE, alpha = 0.5, adjust = 0.7, scale = "count", color = "black") + 
  geom_boxplot(
    varwidth = TRUE, 
    width = 0.2, 
    outlier.shape = 16,  # Punkte als Kreise
    outlier.size = 2,    # Größe der Punkte
    outlier.color = "black" # Farbe der Punkte
  ) +
  # Annotation für Female
  annotate(
    "text", 
    x = 1.13, y = min(data_summed_Day0To11$Age) - 5.5,  # Position unter dem Female-Plot
    label = paste0("n = ", counts$Count[counts$Sex == "Female"]),
    hjust = 0, vjust = 0.5,
    size = 8, color = "black"
  ) +
  # Annotation für Male
  annotate(
    "text", 
    x = 2.13, y = min(data_summed_Day0To11$Age) - 5.5,  # Position unter dem Male-Plot
    label = paste0("n = ", counts$Count[counts$Sex == "Male"]),
    hjust = 0, vjust = 0.5,
    size = 8, color = "black"
  ) +
  # Mittelwert als Punkt
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "black") +
  # Mittelwert als Textbeschriftung
  # stat_summary(
  #   fun = mean, 
  #   geom = "text", 
  #   aes(label = paste0("Mean: ", round(..y.., 1))),  # Beschriftung des Mittelwerts
  #   vjust = 0, size = 4, color = "black", hjust = -1
  # ) +
  scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +  # Farben manuell anpassen
  scale_x_discrete(labels = c("Female" = "Frauen", "Male" = "Männer")) +  # X-Achsen-Beschriftung anpassen
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    title = "Verteilung des Alters nach Geschlecht",
    x = "Geschlecht",
    y = "Alter"
  ) +
  theme.main +
  theme.adjusted +
  theme(legend.position = "none")
        


# Boxplot BMI ####
# Definiere die Normalgewichtsbereiche (in BMI) für Männer und Frauen
normal_weight_men <- c(18.5, 24.9)    # Normalgewicht für Männer (BMI 20-25)
normal_weight_women <- c(18.5, 24.9) # Normalgewicht für Frauen (BMI 19-24)

# Boxplot erstellen mit Normalgewichts-Intervallen
data_summed_Day0To11 %>% 
  select(BMI, Sex) %>%
  ggplot(aes(x = Sex, y = BMI, fill = Sex)) +
  geom_boxplot(
    varwidth = TRUE, 
    width = 0.4, 
    outlier.shape = 16, 
    outlier.size = 2, 
    outlier.color = "black"
  ) +
  # Normalgewicht-Linien für Frauen
  geom_segment(
    aes(x = 0.845, xend = 1.162, y = normal_weight_women[1], yend = normal_weight_women[1]),
    color = "black", linetype = "dashed", size = 1
  ) +
  geom_segment(
    aes(x = 0.845, xend = 1.162, y = normal_weight_women[2], yend = normal_weight_women[2]),
    color = "black", linetype = "dashed", size = 1
  ) +
  # Normalgewicht-Linien für Männer
  geom_segment(
    aes(x = 1.8, xend = 2.2, y = normal_weight_men[1], yend = normal_weight_men[1]),
    color = "black", linetype = "dashed", size = 1
  ) +
  geom_segment(
    aes(x = 1.8, xend = 2.2, y = normal_weight_men[2], yend = normal_weight_men[2]),
    color = "black", linetype = "dashed", size = 1
  ) +
  # Mittelwert als Punkt hinzufügen
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 16,
    size = 3,
    color = "black"
  ) +
  # Notiz für Normalgewicht Frauen
  annotate(
    "text",
    x = 0.7,
    y = normal_weight_women[2] - 1.8,  # Oberhalb des Normalgewicht-Intervalls
    label = "Normalgewicht: 18.5-24.9",
    hjust = -1.25, size = 6, color = "black"
  ) +
  # Notiz für Normalgewicht Männer
  annotate(
    "text", 
    x = 1.729, 
    y = normal_weight_men[2] - 2,  # Oberhalb des Normalgewicht-Intervalls
    label = "Normalgewicht: 18.5-24.9", 
    hjust = -1.25, size = 6, color = "black"
  ) +
  # Achsen- und Designanpassungen
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

# # Mittelwert als Text hinzufügen
# stat_summary(
#   fun = mean,
#   geom = "text",
#   aes(label = paste0("Mean: ", round(..y.., 1))),
#   vjust = -0.8,
#   hjust = 0.5,
#   size = 4,
#   color = "black"
# ) +

# perfekte Ausrichtung von Normalgewicht für Martin:
# data_summed_Day0To11 %>% 
#   select(BMI, Sex) %>%
#   ggplot(aes(x = Sex, y = BMI, fill = Sex)) +
#   geom_boxplot(
#     varwidth = TRUE,
#     width = 0.4,
#     outlier.shape = 16,
#     outlier.size = 2,
#     outlier.color = "black"
#   ) +
#   # Normalgewicht-Linien für Frauen
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
#   # Mittelwert als Punkt hinzufügen
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 16,
#     size = 3,
#     color = "black"
#   ) +
#   # Notiz für Normalgewicht Frauen
#   annotate(
#     "text",
#     x = 0.725,
#     y = normal_weight_women[2] - 2.9,  # Oberhalb des Normalgewicht-Intervalls
#     label = "Normalgewicht: 18.5 - 24.9",
#     hjust = -1.25, size = 6, color = "black"
#   ) +
#   # Notiz für Normalgewicht Männer
#   annotate(
#     "text",
#     x = 1.77,
#     y = normal_weight_men[2] - 2.9,  # Oberhalb des Normalgewicht-Intervalls
#     label = "Normalgewicht: 18.5 - 24.9",
#     hjust = -1.25, size = 6, color = "black"
#   ) +
#   # Achsen- und Designanpassungen
#   scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +
#   scale_x_discrete(labels = c("Female" = "Frauen", "Male" = "Männer")) +
#   scale_y_continuous(breaks = seq(10, 110, by = 5)) +
#   labs(title = "BMI-Verteilung nach Geschlecht", x = "Geschlecht", y = "BMI") +
#   theme.main + theme.adjusted + theme(legend.position = "none")


# Kaplan-Meier Plot ####
# Kaplan-Meier Überlebenswahrscheinlichkeit
km_data_death <- data_summed_Day0To11 %>%
  mutate(daysToEvent = if_else(PatientDischarged == 1, 61L, daysToEvent),
         roundedDaysToEvent = round(daysToEvent))
km_death <- survfit(Surv(daysToEvent, PatientDied) ~ 1, data = km_data_death)
# KM Überelbenswsk (mit stetigen Tagen)
km_death_plot <- ggsurvplot(km_death, legend = "none",
           xlab = "Tage", ylab = "Überlebenswahrscheinlichkeit",
           title = "Kaplan-Meier Kurve Überlebenswahrscheinlichkeit",
           censor = FALSE, ggtheme = (theme.main + theme.adjusted))
km_death_plot$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_death$daysToEvent), by = 10)) +
  geom_line(size = 1.2, color = "#0072B2")
# KM Überelbenswsk (gerundet auf ganze Tagen)
km_death_rounded <- survfit(Surv(roundedDaysToEvent, PatientDied) ~ 1, data = km_data_death)
km_death_plot_rounded <- ggsurvplot(
  km_death_rounded, legend = "none",
  xlab = "Tage (gerundet)", ylab = "Überlebenswahrscheinlichkeit",
  title = "Kaplan-Meier Kurve Überlebenswahrscheinlichkeit",
  censor = FALSE, ggtheme = (theme.main + theme.adjusted),
  lwd = 1.2, palette = "#0072B2"
)
km_death_plot_rounded$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_death$roundedDaysToEvent), by = 10))

# Kaplan Meier für nicht-entlassung Wahrscheinlichkeit
km_data_disc <- data_summed_Day0To11 %>%
  mutate(daysToEvent = if_else(PatientDied == 1, 61L, daysToEvent),
         roundedDaysToEvent = round(daysToEvent))
km_disc <- survfit(Surv(daysToEvent, PatientDischarged) ~ 1, data = km_data_disc)
# stetige Tage
km_disc_plot <- ggsurvplot(km_disc, legend = "none",
           xlab = "Tage", ylab = "Wahrscheinlichkeit - Patient wird nicht entlassen",
           title = "Kaplan-Meier Kurve Nicht-Entlassungen",
           censor = FALSE, ggtheme = (theme.main + theme.adjusted))
km_disc_plot$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_disc$daysToEvent), by = 10)) +
  geom_line(size = 1.2, color = "#E69F00")
# gerundete Tage
km_disc_rounded <- survfit(Surv(roundedDaysToEvent, PatientDischarged) ~ 1, data = km_data_disc)
km_disc_plot_rounded <- ggsurvplot(
  km_disc_rounded, legend = "none",
  xlab = "Tage (gerundet)", ylab = "Wahrscheinlichkeit - Patient wird nicht entlassen",
  title = "Kaplan-Meier Kurve Nicht-Entlassungen",
  censor = FALSE, ggtheme = (theme.main + theme.adjusted),
  lwd = 1.2, palette = "#E69F00"
)
km_disc_plot_rounded$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_disc$roundedDaysToEvent), by = 10))


# Verteilung Propofol Cals pro Tag ####
# Filter only the days with Propofol administration

propofol_data <- subset(data_long, Propofol == 1)

# Create the boxplot
ggplot(propofol_data, aes(x = as.factor(Study_Day), y = PropofolCal)) +
  geom_boxplot(fill = "#56B4E9", color = "black", outlier.size = 2, outlier.color = "black") +
  scale_y_continuous(breaks = seq(0, max(propofol_data$PropofolCal, na.rm = TRUE) + 500, 500)) +
  labs(
    title = "Daily Distribution of Propofol Calories",
    x = "Study Day",
    y = "Propofol Calories (kcal)"
  ) + theme.main + theme.adjusted


# Cumulative Incidenc Functions ####
# prep data
cumu_data <- data_summed_Day0To11[data_summed_Day0To11$surv_icu_status != 0, ]
km <- survfit(formula = Surv(daysToEvent) ~ surv_icu_status,
              data = cumu_data)
ggsurvplot(km,
           data = data_summed_Day0To11,
#           risk.table = TRUE, # adds table below plot
           legend.labs = c("Discharge", "Death"),
           title = "Cumulative Incidences for Death and Discharge",
           xlim = c(0,60),
           break.time.by = 10,
           ylim = c(0,1),
           xlab = "Days",
           ylab = "Event Probability",
           legend.title = "",
           palette = c("#E69F00", "#0072B2"),
           fun = "event",
           ggtheme = (theme.main + theme.adjusted +
                        theme(legend.text = element_text(size = 18),
                              legend.key.size = unit(0.8, "cm")))
)
