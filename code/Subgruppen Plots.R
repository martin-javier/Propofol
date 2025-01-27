# Subgruppen Forest Plots

# Subgruppen
# Frauen, Death, Propofol/PropofolCal
summary(model_death_female_propDays_calsAbove16) #niedrigster AIC von den Frauen
#summary(model_death_female_propCals_calsAbove16)
#"model_death_female_propDays_calsAbove16"


# Frauen, Discharged, Propofol/PropofolCal
#summary(model_disc_female_propDays_calsAbove70pct)
#summary(model_disc_female_propCals_calsAbove16)

# Ältere Patienten über 65, Death, Propofol/PropofolCal
summary(model_death_older65_propDays_calsAbove70pct) # niedrigster AIC von den Älter 65
#summary(model_death_older65_propCals_calsAbove70pct)
#"model_death_older65_propDays_calsAbove70pct"


# Ältere Patienten über 65, Discharged, Propofol/PropofolCal
#summary(model_disc_older65_propDays_calsAbove70pct)
#summary(model_disc_older65_propCals_calsAbove70pct)



# Berechnung von den kleinsten AICs pro Subgruppe
# Modelle für Frauen
aic_female_death_days <- AIC(model_death_female_propDays_calsAbove16)
aic_female_death_cals <- AIC(model_death_female_propCals_calsAbove16)
aic_female_disc_days <- AIC(model_disc_female_propDays_calsAbove70pct)
aic_female_disc_cals <- AIC(model_disc_female_propCals_calsAbove16)

# Modelle für Ältere Patienten über 65
aic_older65_death_days <- AIC(model_death_older65_propDays_calsAbove70pct)
aic_older65_death_cals <- AIC(model_death_older65_propCals_calsAbove70pct)
aic_older65_disc_days <- AIC(model_disc_older65_propDays_calsAbove70pct)
aic_older65_disc_cals <- AIC(model_disc_older65_propCals_calsAbove70pct)


# Für Frauen
aic_female <- c(
  model_death_female_propDays_calsAbove16 = AIC(model_death_female_propDays_calsAbove16),
  model_death_female_propCals_calsAbove16 = AIC(model_death_female_propCals_calsAbove16),
  model_disc_female_propDays_calsAbove70pct = AIC(model_disc_female_propDays_calsAbove70pct),
  model_disc_female_propCals_calsAbove16 = AIC(model_disc_female_propCals_calsAbove16)
)

# Für Ältere Patienten über 65
aic_older65 <- c(
  model_death_older65_propDays_calsAbove70pct = AIC(model_death_older65_propDays_calsAbove70pct),
  model_death_older65_propCals_calsAbove70pct = AIC(model_death_older65_propCals_calsAbove70pct),
  model_disc_older65_propDays_calsAbove70pct = AIC(model_disc_older65_propDays_calsAbove70pct),
  model_disc_older65_propCals_calsAbove70pct = AIC(model_disc_older65_propCals_calsAbove70pct)
)

# Für Frauen
lowest_aic_female <- names(sort(aic_female)[1:2])

# Für Ältere Patienten über 65
lowest_aic_older65 <- names(sort(aic_older65)[1:2])

# Zeige die Modellnamen an
lowest_aic_female
lowest_aic_older65
