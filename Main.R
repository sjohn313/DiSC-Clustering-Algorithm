# =============================================================================
# Main.R - Production Pipeline
# =============================================================================

# 1. Source all modular scripts
source("scripts/01_data_generation.R")
source("scripts/02_preprocessing.R")
source("scripts/03_clustering.R")

main <- function() {
  message("=== Starting Pipeline ===")
  
  # Step 1: Generate Data 
  # This saves a CSV to "data/career_scm_data.csv"
  generate_scm_data(n = 1000, seed = 42)
  
  # Step 2: Prepare Data
  # IMPORTANT: Do not pass 'raw_data' here. 
  # This function reads the file from the disk by default.
  prepare_data() 
  
  # Step 3: Run Clustering
  # This reads "data/career_scm_preprocessed.csv" by default.
  results <- run_clustering(k_range = 2:10)
  
  message("=== Pipeline Complete ===")
  return(results)
}

# Execute
main()