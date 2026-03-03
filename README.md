# Career Archetype Clustering: An SCM-Driven Approach

This repository implements an end-to-end machine learning pipeline to simulate, preprocess, and cluster professional career data. By utilizing a **Structural Causal Model (SCM)**, we ensure the underlying data follows realistic business logic before applying unsupervised learning. This is a very important process that could become more common in HR/People Analytics as data accessibilty could present a serious issue. I hope to showcase in this .md file that I am able to fix this problem.

All csv data files are in the "data" folder.

## Project Structure

Each script must be run in sequence to maintain the data pipeline:

1.  **[`SCM_data_script.R`](./scripts/SCM_data_script.R)**: **Data Generation**
    - Generates 1,000 professional profiles using `rbeta` and `rnorm` distributions.
    - **Causal Logic:** Calculates career length as a function of age and determines hierarchy status based on tenure and DiSC style. Calculates current_salary as a function of age, hierarcy status, some influence from DISC etc. This process is called Domain Modeling.

2.  **[`data_prep_script.R`](./scripts/data_prep_script.R)**: **Feature Engineering**
    - **Normalization:** Performs Z-score standardization (scaling) on numeric variables like age, current and starting salary, and hierarchy status. All caterogircal variables are one-hot encoded.
    - **Encoding:** Transforms categorical data (DiSC and Industry) into numeric format via one-hot encoding.

3.  **[`clustering_script.R`](./scripts/clustering_script.R)**: **Unsupervised Learning**
    - **Evaluation:** Iterates through $k=2$ to $k=10$ using the **Hartigan-Wong** algorithm. This algorithm operates on the same basis as the famous Lloyd algorithm, means are considered before points are assigned to clusters, making it more robust and efficient.
    - **Metrics:** Calculates **Within-Cluster Sum of Squares (WCSS)** and **Silhouette Scores**.
    - **Clustering:** Executes K-Means with `nstart = 100` to ensure stability.

## The Clustering Logic

K-Means aims to minimize the **Within-Cluster Sum of Squares (WCSS)**. Mathematically, it seeks to minimize:
$$J = \sum_{i=1}^{k} \sum_{x \in C_i} ||x - \mu_i||^2$$

When this number is small it means clusters are grouped together in a way in which we can make inferences in the data.

## Usage

1. **Clone the Repository**
2. **Install R Dependencies:** `cluster`, `ggplot2`, `dplyr`, `readr`, `tidyr`.
3. **Run Pipeline:** Execute the scripts in the order listed above.
