# Clear workspace
rm(list = ls())

# Load libraries
library(data.table)
library(ClusterR)
library(ggplot2)

# Set seed for reproducibility
set.seed(10)

# Load data
data <- fread("./project/volume/data/raw/data.csv")

# Store and remove the id
id <- data$id
data$id <- NULL

# Perform PCA
pca <- prcomp(data, center = TRUE, scale. = TRUE)

# Extract PCA components
pca_dt <- data.table(unclass(pca)$x)

# Determine optimal number of clusters using AIC
max_clus <- 10
k_aic <- Optimal_Clusters_GMM(pca_dt[, 1:3], 
                              max_clusters = max_clus,
                              criterion = "AIC")

# Plot change in slope
delta_k <- c(NA, k_aic[-1] - k_aic[-length(k_aic)])
del_k_tab <- data.table(delta_k = delta_k, k = 1:length(delta_k))
ggplot(del_k_tab, aes(x = k, y = -delta_k)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

# Set optimal number of clusters
opt_num_clus <- 4

# Perform GMM
gmm_data <- GMM(pca_dt[, 1:3], opt_num_clus)

# Predict clusters
clusterInfo <- predict_GMM(pca_dt[, 1:3],
                           gmm_data$centroids,
                           gmm_data$covariance_matrices,
                           gmm_data$weights)

# Extract necessary information
log_likelihood <- clusterInfo$log_likelihood
cluster_proba <- clusterInfo$cluster_proba
cluster_labels <- clusterInfo$cluster_labels

# Read example submission file
example_sub <- fread("./project/volume/data/raw/example_sub.csv")

# Merge 'id' with predicted cluster probabilities
final <- cbind(id, cluster_proba)

# Rename columns
colnames(final)[-1] <- paste0("Class", 1:opt_num_clus)

# Convert to data.table
final <- as.data.table(final)

# Write the processed data to a CSV file
fwrite(final, './project/volume/data/processed/submit_PCA_2.csv')
