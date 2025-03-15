# Model Comparision based on AIC for subgroup models

# Calculation of model fit for Discharge models
scores <- list(
  older65_propDays_calsAbove16_disc = AIC(model_disc_older65_propDays_calsAbove16),
  older65_propDays_calsAbove70pct_disc = AIC(model_disc_older65_propDays_calsAbove70pct),
  older65_propCals_calsAbove16_disc = AIC(model_disc_older65_propCals_calsAbove16),
  older65_propCals_calsAbove70pct_disc = AIC(model_disc_older65_propCals_calsAbove70pct),
  
  younger66_propDays_calsAbove16_disc = AIC(model_disc_younger66_propDays_calsAbove16),
  younger66_propDays_calsAbove70pct_disc = AIC(model_disc_younger66_propDays_calsAbove70pct),
  younger66_propCals_calsAbove16_disc = AIC(model_disc_younger66_propCals_calsAbove16),
  younger66_propCals_calsAbove70pct_disc = AIC(model_disc_younger66_propCals_calsAbove70pct),
  
  male_propDays_calsAbove16_disc = AIC(model_disc_male_propDays_calsAbove16),
  male_propDays_calsAbove70pct_disc = AIC(model_disc_male_propDays_calsAbove70pct),
  male_propCals_calsAbove16_disc = AIC(model_disc_male_propCals_calsAbove16),
  male_propCals_calsAbove70pct_disc = AIC(model_disc_male_propCals_calsAbove70pct),
  
  female_propDays_calsAbove16_disc = AIC(model_disc_female_propDays_calsAbove16),
  female_propDays_calsAbove70pct_disc = AIC(model_disc_female_propDays_calsAbove70pct),
  female_propCals_calsAbove16_disc = AIC(model_disc_female_propCals_calsAbove16),
  female_propCals_calsAbove70pct_disc = AIC(model_disc_female_propCals_calsAbove70pct)
)

# Selecting the best models
best_older65_propDays_disc <- ifelse(
  which.min(c(scores$older65_propDays_calsAbove16_disc, scores$older65_propDays_calsAbove70pct_disc)) == 1,
  "model_disc_older65_propDays_calsAbove16_disc",
  "model_disc_older65_propDays_calsAbove70pct_disc"
)
best_older65_propCals_disc <- ifelse(
  which.min(c(scores$older65_propCals_calsAbove16_disc, scores$older65_propCals_calsAbove70pct_disc)) == 1,
  "model_disc_older65_propCals_calsAbove16_disc",
  "model_disc_older65_propCals_calsAbove70pct_disc"
)

best_younger66_propDays_disc <- ifelse(
  which.min(c(scores$younger66_propDays_calsAbove16_disc, scores$younger66_propDays_calsAbove70pct_disc)) == 1,
  "model_disc_younger66_propDays_calsAbove16_disc",
  "model_disc_younger66_propDays_calsAbove70pct_disc"
)
best_younger66_propCals_disc <- ifelse(
  which.min(c(scores$younger66_propCals_calsAbove16_disc, scores$younger66_propCals_calsAbove70pct_disc)) == 1,
  "model_disc_younger66_propCals_calsAbove16_disc",
  "model_disc_younger66_propCals_calsAbove70pct_disc"
)

best_male_propDays_disc <- ifelse(
  which.min(c(scores$male_propDays_calsAbove16_disc, scores$male_propDays_calsAbove70pct_disc)) == 1,
  "model_disc_male_propDays_calsAbove16_disc",
  "model_disc_male_propDays_calsAbove70pct_disc"
)
best_male_propCals_disc <- ifelse(
  which.min(c(scores$male_propCals_calsAbove16_disc, scores$male_propCals_calsAbove70pct_disc)) == 1,
  "model_disc_male_propCals_calsAbove16_disc",
  "model_disc_male_propCals_calsAbove70pct_disc"
)

best_female_propDays_disc <- ifelse(
  which.min(c(scores$female_propDays_calsAbove16_disc, scores$female_propDays_calsAbove70pct_disc)) == 1,
  "model_disc_female_propDays_calsAbove16_disc",
  "model_disc_female_propDays_calsAbove70pct_disc"
)
best_female_propCals_disc <- ifelse(
  which.min(c(scores$female_propCals_calsAbove16_disc, scores$female_propCals_calsAbove70pct_disc)) == 1,
  "model_disc_female_propCals_calsAbove16_disc",
  "model_disc_female_propCals_calsAbove70pct_disc"
)

# Display results
cat("Best model for older patients (>65 years, Propofol days):", best_older65_propDays_disc, "\n")
cat("Best model for older patients (>65 years, Propofol calories):", best_older65_propCals_disc, "\n")

cat("Best model for younger patients (<66 years, Propofol days):", best_younger66_propDays_disc, "\n")
cat("Best model for younger patients (<66 years, Propofol calories):", best_younger66_propCals_disc, "\n")

cat("Best model for male patients (Propofol days):", best_male_propDays_disc, "\n")
cat("Best model for male patients (Propofol calories):", best_male_propCals_disc, "\n")

cat("Best model for female patients (Propofol days):", best_female_propDays_disc, "\n")
cat("Best model for female patients (Propofol calories):", best_female_propCals_disc, "\n")

# For model names example: "model_disc_female_propCals_calsAbove16_disc" always remove _disc at the end


# Calculation of model fit for Death models
scores <- list(
  older65_propDays_calsAbove16_death = AIC(model_death_older65_propDays_calsAbove16),
  older65_propDays_calsAbove70pct_death = AIC(model_death_older65_propDays_calsAbove70pct),
  older65_propCals_calsAbove16_death = AIC(model_death_older65_propCals_calsAbove16),
  older65_propCals_calsAbove70pct_death = AIC(model_death_older65_propCals_calsAbove70pct),
  
  younger66_propDays_calsAbove16_death = AIC(model_death_younger66_propDays_calsAbove16),
  younger66_propDays_calsAbove70pct_death = AIC(model_death_younger66_propDays_calsAbove70pct),
  younger66_propCals_calsAbove16_death = AIC(model_death_younger66_propCals_calsAbove16),
  younger66_propCals_calsAbove70pct_death = AIC(model_death_younger66_propCals_calsAbove70pct),
  
  male_propDays_calsAbove16_death = AIC(model_death_male_propDays_calsAbove16),
  male_propDays_calsAbove70pct_death = AIC(model_death_male_propDays_calsAbove70pct),
  male_propCals_calsAbove16_death = AIC(model_death_male_propCals_calsAbove16),
  male_propCals_calsAbove70pct_death = AIC(model_death_male_propCals_calsAbove70pct),
  
  female_propDays_calsAbove16_death = AIC(model_death_female_propDays_calsAbove16),
  female_propDays_calsAbove70pct_death = AIC(model_death_female_propDays_calsAbove70pct),
  female_propCals_calsAbove16_death = AIC(model_death_female_propCals_calsAbove16),
  female_propCals_calsAbove70pct_death = AIC(model_death_female_propCals_calsAbove70pct)
)

# Selecting the best models
best_older65_propDays_death <- ifelse(
  which.min(c(scores$older65_propDays_calsAbove16_death, scores$older65_propDays_calsAbove70pct_death)) == 1,
  "model_death_older65_propDays_calsAbove16_death",
  "model_death_older65_propDays_calsAbove70pct_death"
)
best_older65_propCals_death <- ifelse(
  which.min(c(scores$older65_propCals_calsAbove16_death, scores$older65_propCals_calsAbove70pct_death)) == 1,
  "model_death_older65_propCals_calsAbove16_death",
  "model_death_older65_propCals_calsAbove70pct_death"
)

best_younger66_propDays_death <- ifelse(
  which.min(c(scores$younger66_propDays_calsAbove16_death, scores$younger66_propDays_calsAbove70pct_death)) == 1,
  "model_death_younger66_propDays_calsAbove16_death",
  "model_death_younger66_propDays_calsAbove70pct_death"
)
best_younger66_propCals_death <- ifelse(
  which.min(c(scores$younger66_propCals_calsAbove16_death, scores$younger66_propCals_calsAbove70pct_death)) == 1,
  "model_death_younger66_propCals_calsAbove16_death",
  "model_death_younger66_propCals_calsAbove70pct_death"
)

best_male_propDays_death <- ifelse(
  which.min(c(scores$male_propDays_calsAbove16_death, scores$male_propDays_calsAbove70pct_death)) == 1,
  "model_death_male_propDays_calsAbove16_death",
  "model_death_male_propDays_calsAbove70pct_death"
)
best_male_propCals_death <- ifelse(
  which.min(c(scores$male_propCals_calsAbove16_death, scores$male_propCals_calsAbove70pct_death)) == 1,
  "model_death_male_propCals_calsAbove16_death",
  "model_death_male_propCals_calsAbove70pct_death"
)

best_female_propDays_death <- ifelse(
  which.min(c(scores$female_propDays_calsAbove16_death, scores$female_propDays_calsAbove70pct_death)) == 1,
  "model_death_female_propDays_calsAbove16_death",
  "model_death_female_propDays_calsAbove70pct_death"
)
best_female_propCals_death <- ifelse(
  which.min(c(scores$female_propCals_calsAbove16_death, scores$female_propCals_calsAbove70pct_death)) == 1,
  "model_death_female_propCals_calsAbove16_death",
  "model_death_female_propCals_calsAbove70pct_death"
)

# Display results
cat("Best model for older patients (>65 years, Propofol days):", best_older65_propDays_death, "\n")
cat("Best model for older patients (>65 years, Propofol calories):", best_older65_propCals_death, "\n")

cat("Best model for younger patients (<66 years, Propofol days):", best_younger66_propDays_death, "\n")
cat("Best model for younger patients (<66 years, Propofol calories):", best_younger66_propCals_death, "\n")

cat("Best model for male patients (Propofol days):", best_male_propDays_death, "\n")
cat("Best model for male patients (Propofol calories):", best_male_propCals_death, "\n")

cat("Best model for female patients (Propofol days):", best_female_propDays_death, "\n")
cat("Best model for female patients (Propofol calories):", best_female_propCals_death, "\n")


# Subgroups
# Female patients, Death, Propofol/PropofolCal
summary(model_death_female_propDays_calsAbove16)
summary(model_death_female_propCals_calsAbove16)

# Female patients, Discharged, Propofol/PropofolCal
summary(model_disc_female_propDays_calsAbove70pct)
summary(model_disc_female_propCals_calsAbove16)

# Older patients over 65, Death, Propofol/PropofolCal
summary(model_death_older65_propDays_calsAbove70pct)
summary(model_death_older65_propCals_calsAbove70pct)

# Older patients over 65, Discharged, Propofol/PropofolCal
summary(model_disc_older65_propDays_calsAbove70pct)
summary(model_disc_older65_propCals_calsAbove70pct)
