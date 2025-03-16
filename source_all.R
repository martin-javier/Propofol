# Prepares R environment for fitting models and plotting data

# Install and load packages
source("code/install_packages.R")

# Create needed dataframes by importing and manipulating dataset
source("code/import_and_clean.R")
data_summed_Day0To11 <- clean_and_summarise_Days0To11()
data_summed_Day0To7 <- clean_and_summarise_Days0To7()
data_long <- clean_data()
manualPED_death <- create_ped_manually(data_long, "death")
manualPED_disc <- create_ped_manually(data_long, "discharge")
# ignore NaNs warning - there aren't any in the dataframes

# Import all saved models
model_files <- list.files("models", pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)

for (file in model_files) {
  model_name <- tools::file_path_sans_ext(basename(file))
  assign(model_name, readRDS(file))
}

# Source rest of the needed code files
code_files <- list.files(path = "code", full.names = TRUE)
code_files <- code_files[code_files != "code/AIC_analysis_subgroups.R" &
                           code_files != "code/models_data_summed.R" &
                           code_files != "code/models_manualPED.R" &
                           code_files != "code/models_subgroups.R" &
                           code_files != "code/overview_propofol.R" &
                           code_files != "code/install_packages.R" &
                           code_files != "code/import_and_clean.R" &
                           code_files != "code/preproc"]
lapply(code_files, source)
