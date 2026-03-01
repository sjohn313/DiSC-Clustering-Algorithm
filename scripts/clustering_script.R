# Load libraries
install.packages(c("cluster", "ggplot2", "dplyr", "readr", "tidyr"))
library(cluster)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Set seed
set.seed(42)

# 1. Load Preprocessed Data
file_path <- "data/career_scm_preprocessed.csv"
data_prep <- read_csv(file_path)
cat("Preprocessed data loaded successfully:", nrow(data_prep), "rows,", ncol(data_prep), "columns.\n")

# Convert to matrix for clustering
scaled_data <- as.matrix(data_prep)

# 3. K-Means Evaluation (k=2 to 10)
k_range <- 2:10
wcss <- vector("numeric", length(k_range))
sil_scores <- vector("numeric", length(k_range))

for (i in seq_along(k_range)) {
  k <- k_range[i]
  km <- kmeans(scaled_data, centers = k, nstart = 100, iter.max = 300, algorithm = "Hartigan-Wong")
  wcss[i] <- km$tot.withinss
  
  ss <- silhouette(km$cluster, dist(scaled_data))
  sil_scores[i] <- mean(ss[, 3])
  cat(sprintf("k=%d | WCSS: %.2f | Sil Score: %.4f\n", k, wcss[i], sil_scores[i]))
}

# 4. Optimal K Selection
# Method 1: Elbow (Maximum second derivative of WCSS)
diff_wcss <- diff(wcss)
diff2_wcss <- diff(diff_wcss)
elbow_k <- k_range[which.max(diff2_wcss) + 1]

# Method 2: Silhouette (Highest score)
sil_k <- k_range[which.max(sil_scores)]

# Final Decision
best_k <- sil_k
decision_msg <- if(elbow_k == sil_k) {
  paste("Methods agree. Final k =", best_k)
} else {
  paste("Methods disagree (Elbow:", elbow_k, "vs Sil:", sil_k, "). Defaulting to Silhouette. Final k =", best_k)
}
print(decision_msg)

# 5. Visualizations
error_table <- data.frame(k = k_range, wcss = wcss, silhouette_score = sil_scores)

# Elbow Plot
ggplot(error_table, aes(x = k, y = wcss)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = elbow_k, linetype = "dashed", color = "red") +
  annotate("text", x = elbow_k + 0.5, y = max(wcss), label = "Elbow", color = "red") +
  labs(title = "Elbow Method – Optimal K Selection", y = "WCSS", x = "Number of Clusters (k)") +
  theme_minimal()
ggsave("output/elbow_plot.png")

# Silhouette Plot
ggplot(error_table, aes(x = k, y = silhouette_score)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = sil_k, linetype = "dashed", color = "blue") +
  annotate("text", x = sil_k + 0.5, y = max(sil_scores), label = "Best K", color = "blue") +
  labs(title = "Silhouette Score – Optimal K Selection", y = "Avg Silhouette Width", x = "Number of Clusters (k)") +
  theme_minimal()
ggsave("output/silhouette_plot.png")

# 6. Final Model & Exports
final_km <- kmeans(scaled_data, centers = best_k, nstart = 100, iter.max = 300)

# Load original data for cluster assignment and export
career_data <- read_csv("data/career_scm_preprocessed.csv")
career_data$cluster <- final_km$cluster

# Cluster Summary
cluster_summary <- career_data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean), n = n())

write_csv(error_table, "output/kmeans_error_table.csv")
write_csv(cluster_summary, "output/cluster_summary.csv")
write_csv(career_data, "output/career_scm_clustered.csv")

cat("Analysis complete. All files exported to output directory.\n")