# Data Preparation
rm(list = ls())
library(data.table)
library(ggplot2)
library(caret)
library(ClusterR)
library(Metrics)

set.seed(3000)

# Load in data 
data <- fread("./project/volume/data/raw/data.csv")

# Store and remove the id
id <- data$id
data$id <- NULL

# Move the 'id' column to the beginning
data <- cbind(id = id, data)

# PCA
PCA <- prcomp(data[, -1], center = TRUE, scale. = TRUE)
summary(PCA)

# use the unclass() function to get the data in PCA space
PCA_dt <- data.table(unclass(PCA)$x)