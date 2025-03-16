# Installs (if not yet installed) and loads required packages

# List of required packages
required_packages <- c("tidyverse", "dplyr", "pammtools", "ggplot2", "pammtools", "survival",
                       "checkmate", "mgcv", "ggthemes", "survminer", "broom", "parallel")

# Install packages if not already installed
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Load packages
lapply(required_packages, library, character.only = TRUE)
