# Data Preparation Script
# Mean normalizes (standardizes) numeric variables

# Load libraries
library(readr)
library(dplyr)

# Set working directory to project root
setwd("C:/Users/sebas/dev/data_creation_project(for clustering)")

# Load data
file_path <- "data/career_scm_data.csv"
data <- read_csv(file_path)

cat("Data loaded successfully:", nrow(data), "rows,", ncol(data), "columns.\n\n")

# Mean normalize (z-score standardization) for specified columns
data_prep <- data %>%
  mutate(
    age = scale(age)[,1],
    career_length_years = scale(career_length_years)[,1],
    starting_salary = scale(starting_salary)[,1],
    current_salary = scale(current_salary)[,1],
    hierarchy_status = scale(hierarchy_status)[,1]
  )

# One-hot encode disc_style (D, I, S, C) and industry using model.matrix
disc_encoded <- as.data.frame(model.matrix(~disc_style - 1, data = data_prep))
names(disc_encoded) <- gsub("disc_style", "", names(disc_encoded))

industry_encoded <- as.data.frame(model.matrix(~industry - 1, data = data_prep))
names(industry_encoded) <- gsub("industry", "", names(industry_encoded))

# Remove original categorical columns and bind encoded versions
data_prep <- data_prep %>%
  select(-disc_style, -industry) %>%
  cbind(disc_encoded, industry_encoded)

# Display summary of normalized data
cat("Normalized Data Summary:\n")
print(summary(data_prep[, c("age", "career_length_years", "starting_salary", "current_salary")]))

# Save preprocessed data
output_path <- "data/career_scm_preprocessed.csv"
write_csv(data_prep, output_path)
cat("\nPreprocessed data saved to:", output_path, "\n")
