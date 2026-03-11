# =============================================================================
# SCM_data_script.R
# Generates synthetic career/SCM data and saves it to data/career_scm_data.csv
# =============================================================================

generate_scm_data <- function(n = 1000,
                               seed = 42,
                               output_path = "data/career_scm_data.csv") {

  set.seed(seed)
  cat("=== [1/3] Generating SCM Data ===\n")
  cat(sprintf("  Sample size : %d\n", n))

  # --- Age -------------------------------------------------------------------
  # Beta distribution scaled to realistic working-age range (22–65), skewed 30–50
  age <- round(22 + rbeta(n, shape1 = 3, shape2 = 4) * (65 - 22))

  # --- Industry --------------------------------------------------------------
  industries  <- c("Technology", "Finance", "Healthcare", "Education")
  industry    <- sample(industries, n, replace = TRUE,
                        prob = c(0.30, 0.25, 0.25, 0.20))

  # --- DiSC Style ------------------------------------------------------------
  disc_styles <- c("D", "I", "S", "C")
  disc_style  <- sample(disc_styles, n, replace = TRUE,
                        prob = c(0.25, 0.25, 0.25, 0.25))

  # --- Career Length ---------------------------------------------------------
  # Causally derived from age; capped at (age - 22), minimum 0
  career_length_years <- pmax(
    0,
    pmin(age - 22, (age - 22) - rnorm(n, mean = 2, sd = 1.5))
  )
  career_length_years <- round(career_length_years, 1)

  # --- Hierarchy Status (1 = Entry, 2 = Mid, 3 = Executive) -----------------
  # Driven by career length; D-style earns a small promotion boost
  hierarchy_status <- sapply(seq_len(n), function(i) {
    tenure     <- career_length_years[i]
    disc_boost <- ifelse(disc_style[i] == "D", 1.2, 1.0)

    if (tenure <= 5) {
      prob <- c(0.80, 0.15, 0.05) * c(1, disc_boost, disc_boost)
    } else if (tenure <= 15) {
      prob <- c(0.20, 0.70, 0.10) * c(1, 1, disc_boost)
    } else {
      prob <- c(0.05, 0.25, 0.70) * c(1, 1, disc_boost)
    }
    prob <- prob / sum(prob)
    sample(1:3, 1, prob = prob)
  })

  # --- Starting Salary (industry-specific baselines) -------------------------
  starting_salary <- round(sapply(industry, function(ind) {
    switch(ind,
      "Technology" = runif(1, 75000, 95000),
      "Finance"    = runif(1, 65000, 85000),
      "Healthcare" = runif(1, 55000, 75000),
      "Education"  = runif(1, 38000, 52000)
    )
  }), 0)

  # --- Current Salary --------------------------------------------------------
  # ~4 % compounded annual growth + hierarchy multiplier + noise
  current_salary <- round(sapply(seq_len(n), function(i) {
    growth      <- starting_salary[i] * (1.04 ^ career_length_years[i])
    status_mult <- switch(as.character(hierarchy_status[i]),
                          "2" = 1.25, "3" = 1.75, "1" = 1.0)
    final       <- (growth * status_mult) + rnorm(1, mean = 0, sd = 5000)
    max(final, starting_salary[i])   # floor at starting salary
  }), 0)

  # --- Compile & Export ------------------------------------------------------
  career_data <- data.frame(
    age                 = as.integer(age),
    career_length_years = career_length_years,
    industry            = industry,
    disc_style          = disc_style,
    hierarchy_status    = as.integer(hierarchy_status),
    starting_salary     = starting_salary,
    current_salary      = current_salary,
    stringsAsFactors    = FALSE
  )

  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  write.csv(career_data, output_path, row.names = FALSE)

  cat(sprintf("  Data saved  : %s  (%d rows, %d cols)\n\n",
              output_path, nrow(career_data), ncol(career_data)))

  invisible(career_data)
}
