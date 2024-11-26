# List of required packages
required_packages <- c("tidyverse", "dplyr", "pammtools", "ggplot2")

# Install packages if not already installed
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Load packages using lapply
lapply(required_packages, library, character.only = TRUE)
