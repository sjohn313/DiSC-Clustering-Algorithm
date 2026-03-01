# Set seed for reproducibility
set.seed(42)

# Define sample size
n <- 1000

# 1. Generate Age: Realistic working-age distribution (22-65)
# Using a beta distribution scaled to 22-65 to skew towards 30-50
age <- round(22 + rbeta(n, shape1 = 3, shape2 = 4) * (65 - 22))

# 2. Generate Industry: Realistic proportions
industries <- c("Technology", "Finance", "Healthcare", "Education")
industry <- sample(industries, n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2))

# 3. Generate DiSC Style: Roughly equal, assigned early for causal influence
disc_styles <- c("D", "I", "S", "C")
disc_style <- sample(disc_styles, n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))

# 4. Generate Career Length: Causally derived from age (max age - 22)
# Added noise but capped at (age - 22) and minimum 0
career_length_years <- pmax(0, pmin(age - 22, (age - 22) - rnorm(n, mean = 2, sd = 1.5)))
career_length_years <- round(career_length_years, 1)

# 5. Generate Hierarchy Status: Driven by career_length and DiSC (D-style boost)
# 1 = Entry, 2 = Mid, 3 = Executive
hierarchy_status <- sapply(1:n, function(i) {
  # Base logic for tenure
  tenure <- career_length_years[i]
  # D-style provides a slight multiplier to "promotion" probability
  disc_boost <- ifelse(disc_style[i] == "D", 1.2, 1.0)
  
  if (tenure <= 5) {
    prob <- c(0.8, 0.15, 0.05) * c(1, disc_boost, disc_boost)
  } else if (tenure <= 15) {
    prob <- c(0.2, 0.7, 0.1) * c(1, 1, disc_boost)
  } else {
    prob <- c(0.05, 0.25, 0.7) * c(1, 1, disc_boost)
  }
  prob <- prob / sum(prob)
  sample(1:3, 1, prob = prob)
})

# 6. Generate Starting Salary: Industry-specific baselines
starting_salary <- sapply(industry, function(ind) {
  if (ind == "Technology") return(runif(1, 75000, 95000))
  if (ind == "Finance")    return(runif(1, 65000, 85000))
  if (ind == "Healthcare") return(runif(1, 55000, 75000))
  if (ind == "Education")  return(runif(1, 38000, 52000))
})
starting_salary <- round(starting_salary, 0)

# 7. Generate Current Salary: Driven by starting, tenure, and hierarchy
# Growth multiplier: 3-5% annual increase + 20% jump for Mid + 50% jump for Exec
current_salary <- sapply(1:n, function(i) {
  years <- career_length_years[i]
  status <- hierarchy_status[i]
  
  # Compounded annual growth (approx 4%)
  growth <- starting_salary[i] * (1.04 ^ years)
  
  # Hierarchy multipliers
  status_mult <- ifelse(status == 2, 1.25, ifelse(status == 3, 1.75, 1.0))
  
  # Final calculation with random noise
  final <- (growth * status_mult) + rnorm(1, mean = 0, sd = 5000)
  
  # Floor at starting salary to ensure realism
  return(round(max(final, starting_salary[i]), 0))
})

# Compile Data Frame
career_data <- data.frame(
  age = as.integer(age),
  career_length_years = career_length_years,
  industry = industry,
  disc_style = disc_style,
  hierarchy_status = as.integer(hierarchy_status),
  starting_salary = starting_salary,
  current_salary = current_salary,
  stringsAsFactors = FALSE
)

# Export to CSV
write.csv(career_data, "career_scm_data.csv", row.names = FALSE)