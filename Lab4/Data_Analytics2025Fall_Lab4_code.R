##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###



 # ----- Abedalah Safi ----- #


## Compute PCA (excluding the Type column)
pca_result <- prcomp(wine[,-1], scale. = TRUE)

autoplot(pca_result, data = wine, colour = 'Type')

pca_rotation <- pca_result$rotation
pca_rotation[, 1:2]

# Train a classifier model (e.g. kNN) to predict the wine type using all the variables in the
# original dataset.

# Set data up and train it
set.seed(1)
train_idx <- sample(1:nrow(wine), 0.7 * nrow(wine))
train_data <- wine[train_idx, -1]
test_data <- wine[-train_idx, -1]
train_labels <- wine$Type[train_idx]
test_labels <- wine$Type[-train_idx]

knn_pred <- knn(train_data, test_data, train_labels, k = 3)

# Train a classifier model to predict the wine type using the data projected onto the first 2
# PCs (scores in the princomp function’s return object)

pca_scores <- pca_result$x[, 1:2]

train_pca <- pca_scores[train_idx, ]
test_pca <- pca_scores[-train_idx, ]

knn_pca_pred <- knn(train_pca, test_pca, train_labels, k = 3)

# Original variables model
table(knn_pred, test_labels)

# PCA model
table(knn_pca_pred, test_labels)

metrics <- function(pred, actual) {
  tbl <- table(pred, actual)
  precision <- diag(tbl) / rowSums(tbl)
  recall <- diag(tbl) / colSums(tbl)
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1)
}

# Original variables metrics
metrics(knn_pred, test_labels)

# PCA model metrics
metrics(knn_pca_pred, test_labels)


