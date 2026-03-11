# data_prep_script.R
# Z-score normalises numeric features and one-hot encodes categorical ones.
# Input : data/career_scm_data.csv
# Output: data/career_scm_preprocessed.csv

prepare_data <- function(input_path  = "data/career_scm_data.csv",
                          output_path = "data/career_scm_preprocessed.csv") {

  library(readr)
  library(dplyr)

  cat("=== [2/3] Preparing Data ===\n")
  cat(sprintf("  Reading     : %s\n", input_path))

  data <- read_csv(input_path, show_col_types = FALSE)
  cat(sprintf("  Loaded      : %d rows, %d cols\n", nrow(data), ncol(data)))

  # --- Z-score standardisation of numeric columns ----------------------------
  data_prep <- data %>%
    mutate(
      age                 = scale(age)[, 1],
      career_length_years = scale(career_length_years)[, 1],
      starting_salary     = scale(starting_salary)[, 1],
      current_salary      = scale(current_salary)[, 1],
      hierarchy_status    = scale(hierarchy_status)[, 1]
    )

  # --- One-hot encoding: DiSC style ------------------------------------------
  disc_encoded <- as.data.frame(model.matrix(~ disc_style - 1, data = data_prep))
  names(disc_encoded) <- gsub("disc_style", "", names(disc_encoded))

  # --- One-hot encoding: industry --------------------------------------------
  industry_encoded <- as.data.frame(model.matrix(~ industry - 1, data = data_prep))
  names(industry_encoded) <- gsub("industry", "", names(industry_encoded))

  # --- Drop original categoricals, bind encoded columns ---------------------
  data_prep <- data_prep %>%
    select(-disc_style, -industry) %>%
    cbind(disc_encoded, industry_encoded)

  # --- Summary (numeric columns only) ---------------------------------------
  cat("\n  Normalised summary (numeric cols):\n")
  print(summary(data_prep[, c("age", "career_length_years",
                               "starting_salary", "current_salary")]))

  # --- Export ----------------------------------------------------------------
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(data_prep, output_path)
  cat(sprintf("\n  Saved       : %s  (%d rows, %d cols)\n\n",
              output_path, nrow(data_prep), ncol(data_prep)))

  invisible(data_prep)
}
