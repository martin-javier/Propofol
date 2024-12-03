library(ggplot2)
library(survival)
library(survminer)
library(ggthemes)

# data_EK erstellen in Model_Versuch.R

theme.main <- theme_stata(scheme = "s1color")
theme.adjusted <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 5), size = 18),
                        axis.title.x = element_text(margin = margin(t = 20), size = 22), 
                        axis.text.y = element_text(hjust = 1, margin = margin(r = 10), size = 15, angle = 0),
                        axis.title.y = element_text(margin = margin(r = 20), size = 22),
                        title = element_text(color = "black"),
                        plot.title = element_text(size = 28, color = "black", face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 17, color = "black", face = "italic"),
                        panel.grid.major = element_line(color = "black", linewidth = 0.1), 
                        panel.grid.minor = element_line(color = "gray", linewidth  = 0.1),
                        plot.background = element_rect(fill = "beige", color = NA))

# Altersgruppen mit Counts erstellen
age_counts <- model_data %>%
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

# mit model_data (ein Balken zeigt Anz. Patienten in 2er Gruppen -> 18&19 Jährige ein Balken, usw.):
ggplot(model_data, aes(x = Age)) +
  geom_histogram(binwidth = 4, fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "Verteilung Alter", x = "Alter", y = "Anzahl Patienten") +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  theme.main + theme.adjusted


# ApacheIIScore ####
# Boxplot
ggplot(model_data, aes(x = "", y = ApacheIIScore)) +
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
  scale_y_continuous(breaks = seq(0, max(data_EK$ApacheIIScore, na.rm = TRUE), by = 10)) +
  theme.main + 
  theme.adjusted

# oder Histogram
# binwidth = 2 bedeutet, dass jede Klasse (bin) eine Breite von 2 Einheiten hat.
# [10, 12), [12, 14), [14, 16), ..., [58, 60]
# ggplot(model_data, aes(x = ApacheIIScore)) +
#   geom_histogram(binwidth = 2, fill = "#56B4E9", color = "black", alpha = 0.7) +
#   scale_x_continuous(breaks = seq(0, 80, by = 10)) +
#   labs(title = "Verteilung des ApacheIIScore", 
#        x = "ApacheIIScore", 
#        y = "Häufigkeit") +
#   theme.main + 
#   theme.adjusted

# Balkendiagramm für Gender ####
ggplot(unique_patients, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(title = "Geschlechterverteilung", 
       x = "Geschlecht", 
       y = "Anzahl Patienten") +
  scale_fill_manual(
    values = c("Female" = "tomato", "Male" = "skyblue")) +
  guides(fill = guide_legend(override.aes = list(shape = 15, size = 8))) +
  theme.main + 
  theme.adjusted +
  theme(legend.text = element_text(size = 20), 
        legend.title = element_text(size = 25, face = "bold"), 
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
        legend.position = "bottom")

# überarbeitet (fand legende und x-Achsenlabel unnötig)
ggplot(model_data, aes(x = Sex, fill = Sex)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(title = "Geschlechterverteilung", x = NULL, y = "Anzahl Patienten") +
  scale_x_discrete(labels = c("Weiblich", "Männlich")) +
  scale_fill_manual(values = c("Female" = "tomato", "Male" = "skyblue")) +
  theme.main + theme.adjusted + theme(legend.position = "none")


# Verstorbene vs. Entlassene vs. im Krankenhaus ####
ggplot(model_data, aes(x = surv_icu_status_exp, fill = surv_icu_status_exp)) +
  geom_bar(color = "black", alpha = 0.8) +
  scale_x_discrete(labels = c("PatientDied" = "Verstorbene",
                              "PatientDischarged" = "Entlassene",
                              "PatientHospital" = "Im Krankenhaus")) +
  labs(title = "Verteilung Patienten", 
       x = NULL, 
       y = "Anzahl Patienten") +
  scale_fill_manual(values = c("PatientDied" = "#0072B2", 
                               "PatientDischarged" = "#E69F00",
                               "PatientHospital" = "#CC79A7")) +
  theme.main + 
  theme.adjusted + 
  theme(legend.position = "none")


# absolute Zahlen stimmen mit outcome_counts überein
# Prozente im plot manuell nachgeprüft, stimmen
# nrow(model_data[model_data$surv_icu_status_exp == "PatientDied",]) / nrow(model_data)
# nrow(model_data[model_data$surv_icu_status_exp == "PatientDischarged",]) / nrow(model_data)
# nrow(model_data[model_data$surv_icu_status_exp == "PatientHospital",]) / nrow(model_data)


# Kaplan-Meier-Modell erstellen FEHLT

              # surv_icu_status == 2:
              #   Tod in der ICU innerhalb von 60 Tagen.
              # 
              # Wenn ein Todeszeitpunkt (surv_icu0to60) gegeben ist und dieser < 60 Tage beträgt, wird das Ereignis als Tod innerhalb der ICU klassifiziert.
              # 
              # surv_icu_status == 1:
              #   Entlassung aus der ICU vor dem Tod (konkurrierendes Risiko).
              # 
              # Wenn ein Entlassungszeitpunkt (discharge_icu) angegeben ist und dieser vor einem eventuellen Tod liegt, wird das Ereignis als Entlassung klassifiziert.
              # Wenn kein Todeszeitpunkt bekannt ist, wird die Entlassung trotzdem als Ereignis gezählt.
              # 
              # surv_icu_status == 0:
              #   Zensiert (kein Tod oder Entlassung innerhalb des Beobachtungszeitraums von 60 Tagen).
              # 
              # Patienten, die bis zum Ende der Beobachtungszeit nicht entlassen oder gestorben sind, werden als zensiert betrachtet.
              # Administrative Zensierung: Alle Patienten mit Ereigniszeit (surv_icu0to60) > 60 Tage werden ebenfalls als zensiert klassifiziert.
              # surv_object <- Surv(time = data_EK$Surv0To60, event = data_EK$surv_icu_status)

# Daten auf eindeutige Patienten filtern (erstes Ereignis pro Patient)
unique_patients_kaplan <- data_EK %>%
  group_by(CombinedID) %>%
  summarise(
    surv_icu0to60 = min(surv_icu0to60, na.rm = TRUE),  # Zeit bis zum ersten Ereignis
    surv_icu_status = first(surv_icu_status)          # Status für das erste Ereignis
  ) %>%
  ungroup()

# Tod als Ereignis definieren: 2 = Tod, andere Status (0 = zensiert, 1 = Entlassung) werden als zensiert behandelt
unique_patients_kaplan$status_tod <- ifelse(unique_patients_kaplan$surv_icu_status == 2, 1, 0)

# Surv-Objekt für Tod in der ICU erstellen
surv_object_tod <- Surv(time = unique_patients_kaplan$surv_icu0to60, event = unique_patients_kaplan$status_tod)

# Kaplan-Meier-Modell für Tod anpassen
km_fit_tod <- survfit(surv_object_tod ~ 1)

# ggsurvplot ####
ggsurvplot(
  km_fit_tod,
  data = unique_patients_kaplan,
  conf.int = TRUE,                              # Konfidenzintervall anzeigen
  pval = TRUE,                                  # P-Wert anzeigen
  xlab = "Tage",                                # X-Achsentitel
  ylab = "Überlebenswahrscheinlichkeit (Tod in der ICU)", # Y-Achsentitel
  title = "Kaplan-Meier-Kurve: Tod in der ICU", # Titel
  risk.table = FALSE,                            # Risikotabelle anzeigen
  conf.int.style = "step",                      # Konfidenzintervall als Schrittlinie
  palette = c("#56B4E9"),            # Farbschema für Kurven und Konfidenzintervalle
  ggtheme = theme_minimal() +                   # Minimalistisches Theme mit Anpassungen
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16), # Zentrierter, fetter Titel
      axis.title = element_text(size = 14),                            # Größere Achsentitel
      axis.text = element_text(size = 12),                             # Größere Achsenbeschriftung
      panel.grid.major = element_line(color = "darkgrey"),               # Hellgraue Gitterlinien
      panel.grid.minor = element_blank(),                              # Keine kleinen Gitterlinien
      plot.background = element_rect(fill = "white", color = NA)       # Beiger Hintergrund
    ),
  legend.labs = c("Tod in der ICU"),          # Legende anpassen
  legend.title = "Status",                    # Legendentitel
  tables.theme = theme.main + theme.adjusted 
)


# Beziehung zwischen Alter und ApacheIIScore ####
# Scatterplot: Zeigt die Beziehung zwischen Alter und ApacheIIScore.
ggplot(model_data, aes(x = Age, y = ApacheIIScore)) +
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
ggplot(model_data, aes(x = factor(Days_Propofol))) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(
    title = "Propofol-Nutzung pro Patient",
    x = "Anzahl der Propofol-Tage",
    y = "Anzahl der Patienten"
  ) + theme.main + theme.adjusted

# Verteilung Admission Categories ####
ggplot(model_data, aes(x = AdmCat)) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  scale_x_discrete(labels =c("Medical" = "Medizinisch",
                             "Surgical/Emeregency" = "Notfalloperation",
                             "Surgical/Elective" = "Geplante Operation")) +
  labs(title = "Verteilung Admissionsgründe", x = NULL, y = "Anzahl Patienten") +
  theme.main + theme.adjusted

# Verteilung Leading admission Diagnosis ####
ggplot(model_data, aes(x = LeadAdmDiag)) +
  geom_bar(fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "Verteilung Erstdiagnose", x = NULL, y = "Anzahl Patienten") +
  theme.main + theme.adjusted

# BMI Plot ####
ggplot(model_data, aes(x = cut(BMI, 
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
  # Brief descreption of Violin Graph:

# A violin plot is a hybrid of a box plot and a kernel density plot, which shows peaks in the data. 
# It is used to visualize the distribution of numerical data. 
#Unlike a box plot that can only show summary statistics, 
#violin plots depict summary statistics and the density of each variable.

mergedAndCleanedData %>% select(Age, Gender) %>%
  ggplot(aes(x = Gender, y = Age, fill = Gender)) + 
  geom_violin(trim = FALSE, alpha = 0.5) + 
  geom_boxplot(varwidth = TRUE, width = 0.2)+
  theme.adjusted +
  theme.main +
  theme(legend.position = "none")
  

# die Verteilung von Alter:

mergedAndCleanedData %>% select(Age) %>% ggplot(aes(Age)) +
  geom_histogram(aes(y = ..density..), fill = "#74C1E9", colour = 1, binwidth = 1) +
  geom_density(color = "orange", lwd = 1.2, linetype = 1, ) + 
  ggtitle("Bin size = 1") + theme.adjusted + theme.main
