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

# Histogramm
ggplot(model_data, aes(x = Age)) +
  geom_histogram(binwidth = 4, fill = "#56B4E9", color = "black", alpha = 0.8) +
  labs(title = "Verteilung Alter", x = "Alter", y = "Anzahl Patienten") +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  theme.main + theme.adjusted

# Histogramm mit Dichte
model_data %>% select(Age) %>% ggplot(aes(Age)) +
  geom_histogram(aes(y = ..density..), fill = "#74C1E9", colour = 1, binwidth = 1, alpha = 0.8) +
  labs(title = "Verteilung Alter (Binwidth = 1)", x = "Alter", y = "Dichte") +
  geom_density(color = "orange", lwd = 1.2, linetype = 1, ) + 
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
unique_patients_kaplan <- model_data %>%
  group_by(CombinedID) %>%
  summarise(
    surv_icu0to60 = min(surv_icu0to60, na.rm = TRUE),  # Zeit bis zum ersten Ereignis
    surv_icu_status = first(surv_icu_status)          # Status für das erste Ereignis
  ) %>%
  ungroup()

# Tod als Ereignis definieren: 2 = Tod, andere Status (0 = zensiert, 1 = Entlassung) werden als zensiert behandelt
unique_patients_kaplan$status_tod <- ifelse(unique_patients_kaplan$surv_icu_status == 2, 1, 0)
unique_patients_kaplan$status_discharge <- ifelse(unique_patients_kaplan$surv_icu_status == 1, 1, 0)

# Surv-Objekt für Tod in der ICU erstellen
surv_object_tod <- Surv(time = unique_patients_kaplan$surv_icu0to60, event = unique_patients_kaplan$status_tod)
surv_object_discarged <- Surv(time = unique_patients_kaplan$surv_icu0to60, event = unique_patients_kaplan$status_discharge)

# Kaplan-Meier-Modell für Tod anpassen
km_fit_tod <- survfit(surv_object_tod ~ 1)
km_fit_discharge <- survfit(surv_object_discarged ~ 1)

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
  palette = c("tomato"),            # Farbschema für Kurven und Konfidenzintervalle
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

# ggsurvplot für Entlassung
ggsurvplot(
  km_fit_discharge,
  data = unique_patients_kaplan,
  conf.int = TRUE,                              # Konfidenzintervall anzeigen
  pval = TRUE,                                  # P-Wert anzeigen
  xlab = "Tage",                                # X-Achsentitel
  ylab = "Entlassungswahrscheinlichkeit", # Y-Achsentitel
  title = "Kaplan-Meier-Kurve: Entlassung aus der ICU", # Titel
  risk.table = FALSE,                           # Risikotabelle ausschalten
  conf.int.style = "step",                      # Konfidenzintervall als Schrittlinie
  palette = c("steelblue"),                     # Farbschema
  ggtheme = theme_minimal() +                   # Minimalistisches Theme mit Anpassungen
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16), # Zentrierter, fetter Titel
      axis.title = element_text(size = 14),                            # Größere Achsentitel
      axis.text = element_text(size = 12),                             # Größere Achsenbeschriftung
      panel.grid.major = element_line(color = "darkgrey"),             # Hellgraue Gitterlinien
      panel.grid.minor = element_blank(),                              # Keine kleinen Gitterlinien
      plot.background = element_rect(fill = "white", color = NA)       # Weißer Hintergrund
    ),
  legend.labs = c("Entlassung aus der ICU"),    # Legende anpassen
  legend.title = "Status"                       # Legendentitel
)

library(broom)
library(ggplot2)

# Überlebensdaten aus dem Kaplan-Meier-Modell extrahieren
km_data <- broom::tidy(km_fit_discharge)

# Manuell Startpunkt hinzufügen
km_data_clean <- km_data %>%
  bind_rows(data.frame(time = 0, estimate = 1, conf.low = 1, conf.high = 1)) %>%
  arrange(time)  # Sortieren, damit 0 am Anfang steht

# Kaplan-Meier-Plot mit ggplot2 erstellen
ggplot(km_data_clean, aes(x = time, y = estimate)) +
  geom_step(color = "steelblue", size = 1.2, linetype = "solid") +   # Schrittlinie
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),                # Konfidenzintervall
              fill = "lightblue", alpha = 0.4) +
  scale_x_continuous(
    breaks = seq(0, 60, by = 10),                                    # Achsen-Breaks alle 10 Tage
    limits = c(0, 60)                                                # Bereich der x-Achse
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),                                    # Achsen-Breaks alle 0.1
    limits = c(0, 1)                                                 # Bereich der y-Achse
  ) +
  labs(
    title = "Kaplan-Meier-Kurve: Entlassung aus der ICU",
    x = "Tage",
    y = "Verbleib in der ICU"
  ) +
  theme.main + 
  theme.adjusted

# Überlebensdaten für Tod in der ICU extrahieren
km_data_tod <- broom::tidy(km_fit_tod)

# Manuell Startpunkt hinzufügen
km_data_tod_clean <- km_data_tod %>%
  bind_rows(data.frame(time = 0, estimate = 1, conf.low = 1, conf.high = 1)) %>%
  arrange(time)  # Sortieren, damit 0 am Anfang steht

# Kaplan-Meier-Plot für Tod in der ICU erstellen
ggplot(km_data_tod_clean, aes(x = time, y = estimate)) +
  geom_step(color = "red", size = 1.2, linetype = "solid") +      # Schrittlinie
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),                # Konfidenzintervall
              fill = "mistyrose", alpha = 0.4) +
  scale_x_continuous(
    breaks = seq(0, 60, by = 10),                                    # Achsen-Breaks alle 10 Tage
    limits = c(0, 60)                                                # Bereich der x-Achse
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),                                    # Achsen-Breaks alle 0.1
    limits = c(0, 1)                                                 # Bereich der y-Achse
  ) +
  labs(
    title = "Kaplan-Meier-Kurve: Tod in der ICU",
    x = "Tage",
    y = "Überlebenswahrscheinlichkeit"
  ) +
  theme.main +
  theme.adjusted


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

# Violin Plot Alter ####
# Brief description of Violin Graph:
# A violin plot is a hybrid of a box plot and a kernel density plot, which shows peaks in the data. 
# It is used to visualize the distribution of numerical data. 
#Unlike a box plot that can only show summary statistics, 
#violin plots depict summary statistics and the density of each variable.

counts <- model_data %>%
  group_by(Sex) %>%
  summarize(Count = n())
means <- model_data %>%
  group_by(Sex) %>%
  summarize(Mean = mean(Age))

# Violinplot mit individueller Annotation pro Geschlecht
model_data %>% 
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
    x = 1.13, y = min(model_data$Age) - 5,  # Position unter dem Female-Plot
    label = paste0("n = ", counts$Count[counts$Sex == "Female"]),
    hjust = 0, vjust = 0.5,
    size = 5, color = "black"
  ) +
  # Annotation für Male
  annotate(
    "text", 
    x = 2.13, y = min(model_data$Age) - 5,  # Position unter dem Male-Plot
    label = paste0("n = ", counts$Count[counts$Sex == "Male"]),
    hjust = 0, vjust = 0.5,
    size = 5, color = "black"
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
normal_weight_men <- c(20, 25)    # Normalgewicht für Männer (BMI 20-25)
normal_weight_women <- c(19, 24) # Normalgewicht für Frauen (BMI 19-24)

# Boxplot erstellen mit Normalgewichts-Intervallen
model_data %>% 
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
    x = 0.97, 
    y = normal_weight_women[2] - 1.5,  # Oberhalb des Normalgewicht-Intervalls
    label = "Normalgewicht: 19-24", 
    hjust = -1.25, size = 4, color = "black"
  ) +
  # Notiz für Normalgewicht Männer
  annotate(
    "text", 
    x = 2, 
    y = normal_weight_men[2] - 2,  # Oberhalb des Normalgewicht-Intervalls
    label = "Normalgewicht: 20-25", 
    hjust = -1.25, size = 4, color = "black"
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


# Kaplan-Meier Plot ####
# Kaplan-Meier Überlebenswahrscheinlichkeit
km_data_death <- model_data %>%
  mutate(daysToEvent = if_else(PatientDischarged == 1, 61L, daysToEvent))
km_death <- survfit(Surv(daysToEvent, PatientDied) ~ 1, data = km_data_death)
km_death_plot <- ggsurvplot(km_death, conf.int = FALSE, legend = "none",
           xlab = "Tage", ylab = "Überlebenswahrscheinlichkeit",
           title = "Kaplan-Meier Kurve Überlebenswahrscheinlichkeit",
           censor = FALSE, ggtheme = (theme.main + theme.adjusted))
km_death_plot$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_death$daysToEvent), by = 10))

# Kaplan Meier für nicht-entlassung Wahrscheinlichkeit
km_data_disch <- model_data %>%
  mutate(daysToEvent = if_else(PatientDied == 1, 61L, daysToEvent))
km_disch <- survfit(Surv(daysToEvent, PatientDischarged) ~ 1, data = km_data_disch)
km_disch_plot <- ggsurvplot(km_disch, conf.int = FALSE, legend = "none",
           xlab = "Tage", ylab = "Wahrscheinlichkeit - Patient wird nicht entlassen",
           title = "Kaplan-Meier Kurve Nicht-Entlassungen",
           censor = FALSE, ggtheme = (theme.main + theme.adjusted))
km_disch_plot$plot +
  scale_x_continuous(breaks = seq(0, max(km_data_death$daysToEvent), by = 10))


# Plot Vergelich Sterbe- und Entlassungswahrscheinlichkeit ####
# Create competing risks data | Event type: 1 = Discharged, 2 = Died
km_data_discharge <- model_data %>%
  mutate(event = case_when(
    PatientDischarged == 1 ~ 1,  # Discharge
    PatientDied == 1 ~ 2,       # Death
    TRUE ~ 0                    # Censored
  ))
# use cuminc form cmprsk package
fit_cr <- cuminc(
  ftime = km_data_discharge$daysToEvent,
  fstatus = km_data_discharge$event,
  cencode = 0 # for censored data
)
# Plot cumulative function for death and discharge
ggcompetingrisks(
  fit_cr, curvetype = "cuminc", conf.int = FALSE,
  title = "Vergelich kumulierte Sterbe- und Entlassungswahrscheinlichkeit",
  xlab = "Tage",  ylab = "Kumulierte Wahrscheinlichkeit für Event",
  ggtheme = (theme.main + theme.adjusted +
               theme(legend.text = element_text(face = "bold")))) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Entlassen", "Verstorben")
  ) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  scale_x_continuous(
    breaks = seq(0, max(km_data_discharge$daysToEvent), by = 10)
  ) +
  guides(color = guide_legend(title = NULL)) + facet_null()

