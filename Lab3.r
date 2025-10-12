library(class)

# read dataset
abalone.data <- read.csv("abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"


set.seed(1)
##### EXCERCISE 1: #####

  # Split data into training and test sets (70% train, 30% test)
  train_index <- sample(1:nrow(abalone.data), 0.7 * nrow(abalone.data))
  
  # First Model - Physical dimensions
  
  first_feat <- abalone.data[, c("length", "diameter", "height")]
  train_first <- first_feat[train_index, ]
  test_first <- first_feat[-train_index, ]
  
  # Second Model - Weight Measurements
  second_feat <- abalone.data[, c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")]
  train_second <- second_feat[train_index, ]
  test_second <- second_feat[-train_index, ]
  
  train_labels <- abalone.data$age.group[train_index]
  test_labels <- abalone.data$age.group[test_index]
  
  knn_pred_1 <- knn(train = train_first, test = test_first, cl = train_labels, k = 5)
  knn_pred_2 <- knn(train = train_second, test = test_second, cl = train_labels, k = 5)
  
  # Create contingency tables
  print(table(Predicted = knn_pred_1, Actual = test_labels))
  print(table(Predicted = knn_pred_2, Actual = test_labels))
  
  # Find More accurate
  first_accuracy = sum(knn_pred_1 == test_labels) / length(test_labels)
  second_accuracy = sum(knn_pred_2 == test_labels) / length(test_labels)
  
  print(paste("Model 1 Accuracy:", round(first_accuracy, 4)))
  print(paste("Model 2 Accuracy:", round(second_accuracy, 4)))
  
  if (first_accuracy >= second_accuracy) {
    more_accurate <- list(train = train_first, test = test_first)
    better_features <- first_feat
  } else {
    more_accurate <- list(train = train_second, test = test_second)
    better_features <- second_feat
  }
  
  # Find optimal k
  k_values <- 1:30
  accuracies <- numeric(length(k_values))
  
  for(i in 1:length(k_values)) {
    k <- k_values[i]
    predictions <- knn(train = more_accurate$train, 
                       test = more_accurate$test, 
                       cl = train_labels, 
                       k = k)
    accuracies[i] <- sum(predictions == test_labels) / length(test_labels)
  }
  
  optimal_k <- k_values[which.max(accuracies)]
  max_accuracy <- max(accuracies)
  
  print(paste("Optimal k:", optimal_k, "with accuracy:", round(max_accuracy, 4)))

##### EXCERCISE 2 #####
  
  
library(cluster)

scaled_features <- scale(better_features)

kmeans_model <- kmeans(scaled_features, centers = 3, nstart = 25)

pam_model <- pam(scaled_features, k = 3)

k_range <- 2:10
kmeans_tot_withinss <- numeric(length(k_range))
pam_silhouette_avg <- numeric(length(k_range))

for(i in 1:length(k_range)) {
  k <- k_range[i]
  
  # K-Means - use total within sum of squares
  kmeans_temp <- kmeans(scaled_features, centers = k, nstart = 25)
  kmeans_tot_withinss[i] <- kmeans_temp$tot.withinss
  
  # PAM - use average silhouette width
  pam_temp <- pam(scaled_features, k = k)
  pam_silhouette_avg[i] <- pam_temp$silinfo$avg.width
}

optimal_k_pam <- k_range[which.max(pam_silhouette_avg)]

kmeans_diff <- diff(kmeans_tot_withinss)
kmeans_optimal_idx <- which.min(abs(kmeans_diff - mean(kmeans_diff)))
optimal_k_kmeans <- k_range[kmeans_optimal_idx]

print(paste("Optimal K for K-Means:", optimal_k_kmeans))

kmeans_optimal <- kmeans(scaled_features, centers = optimal_k_kmeans, nstart = 25)
pam_optimal <- pam(scaled_features, k = optimal_k_pam)

print(paste("Optimal K for PAM:", optimal_k_pam))

par(mfrow = c(1, 2))

# PAM Plot
plot(pam_optimal, which.plots = 2, main = paste("PAM Silhouette Plot (K =", optimal_k_pam, ")"))

# KMEANS Plot
kmeans_silhouette <- silhouette(kmeans_optimal$cluster, dist(scaled_features))
plot(kmeans_silhouette, main = paste("K-Means Silhouette Plot (K =", optimal_k_kmeans, ")"))
