# clustering_script.R
# Runs k-means (k = 2–10), selects best k, exports plots + CSVs.
# Input : data/career_scm_preprocessed.csv
# Output: output/elbow_plot.png, output/silhouette_plot.png,
#         output/kmeans_error_table.csv, output/cluster_summary.csv,
#         output/career_scm_clustered.csv

run_clustering <- function(preprocessed_path = "data/career_scm_preprocessed.csv",
                            output_dir        = "output",
                            seed              = 42,
                            k_range           = 2:10) {

  library(cluster)
  library(ggplot2)
  library(dplyr)
  library(readr)

  set.seed(seed)
  cat("=== [3/3] Running Clustering ===\n")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Load preprocessed data -----------------------------------------------
  data_prep   <- read_csv(preprocessed_path, show_col_types = FALSE)
  scaled_data <- as.matrix(data_prep)
  cat(sprintf("  Loaded      : %d rows, %d cols\n", nrow(data_prep), ncol(data_prep)))

  # --- Evaluate k = 2 to 10 -------------------------------------------------
  cat("\n  Evaluating k:\n")
  wcss       <- numeric(length(k_range))
  sil_scores <- numeric(length(k_range))

  for (i in seq_along(k_range)) {
    k   <- k_range[i]
    km  <- kmeans(scaled_data, centers = k, nstart = 100,
                  iter.max = 300, algorithm = "Hartigan-Wong")
    wcss[i] <- km$tot.withinss

    ss          <- silhouette(km$cluster, dist(scaled_data))
    sil_scores[i] <- mean(ss[, 3])
    cat(sprintf("    k=%2d | WCSS: %10.2f | Silhouette: %.4f\n",
                k, wcss[i], sil_scores[i]))
  }

  # --- Optimal k selection --------------------------------------------------
  diff2_wcss <- diff(diff(wcss))
  elbow_k    <- k_range[which.max(diff2_wcss) + 1]
  sil_k      <- k_range[which.max(sil_scores)]
  best_k     <- sil_k   # silhouette is the primary criterion

  decision_msg <- if (elbow_k == sil_k) {
    sprintf("Both methods agree  → best k = %d", best_k)
  } else {
    sprintf("Methods disagree (Elbow: %d, Silhouette: %d). Using Silhouette → best k = %d",
            elbow_k, sil_k, best_k)
  }
  cat(sprintf("\n  %s\n\n", decision_msg))

  # --- Visualisations -------------------------------------------------------
  error_table <- data.frame(k = k_range, wcss = wcss, silhouette_score = sil_scores)

  # Elbow plot
  p_elbow <- ggplot(error_table, aes(x = k, y = wcss)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = elbow_k, linetype = "dashed", colour = "red") +
    annotate("text", x = elbow_k + 0.5, y = max(wcss),
             label = "Elbow", colour = "red") +
    labs(title = "Elbow Method – Optimal K Selection",
         y = "WCSS", x = "Number of Clusters (k)") +
    theme_minimal()
  ggsave(file.path(output_dir, "elbow_plot.png"), p_elbow)
  cat("  Saved       : elbow_plot.png\n")

  # Silhouette plot
  p_sil <- ggplot(error_table, aes(x = k, y = silhouette_score)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = sil_k, linetype = "dashed", colour = "blue") +
    annotate("text", x = sil_k + 0.5, y = max(sil_scores),
             label = "Best K", colour = "blue") +
    labs(title = "Silhouette Score – Optimal K Selection",
         y = "Avg Silhouette Width", x = "Number of Clusters (k)") +
    theme_minimal()
  ggsave(file.path(output_dir, "silhouette_plot.png"), p_sil)
  cat("  Saved       : silhouette_plot.png\n")

  # --- Final model & exports ------------------------------------------------
  final_km    <- kmeans(scaled_data, centers = best_k,
                         nstart = 100, iter.max = 300)
  career_data <- read_csv(preprocessed_path, show_col_types = FALSE)
  career_data$cluster <- final_km$cluster

  cluster_summary <- career_data %>%
    group_by(cluster) %>%
    summarise(across(where(is.numeric), mean), n = n())

  write_csv(error_table,    file.path(output_dir, "kmeans_error_table.csv"))
  write_csv(cluster_summary, file.path(output_dir, "cluster_summary.csv"))
  write_csv(career_data,    file.path(output_dir, "career_scm_clustered.csv"))

  cat(sprintf("  Saved       : kmeans_error_table.csv, cluster_summary.csv, career_scm_clustered.csv\n\n"))
  cat("  Clustering complete.\n\n")

  invisible(list(
    best_k          = best_k,
    error_table     = error_table,
    cluster_summary = cluster_summary,
    clustered_data  = career_data
  ))
}
