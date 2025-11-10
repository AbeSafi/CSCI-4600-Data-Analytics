library(e1071)
library(class)
library(caret)

wine <- read.csv('wine.data')

features <- wine[, c(1, 2, 3, 8, 10)]

colnames(features) <- c("Alcohol", "Malic_acid", "Flavonoids", 
                        "Color_intensity", "Type")

features <- data.frame(
  Type = as.factor(wine[,1]),  # Class labels as factor
  Alcohol = wine[,2],
  Malic_acid = wine[,3], 
  Flavonoids = wine[,8],
  Color_intensity = wine[,10]
)
        
tune_linear <- tune.svm(Type ~ ., data=features, kernel="linear", cost=10^(-1:2))
svm_linear <- tune_linear$best.model

tune_rbf <- tune.svm(Type ~ ., data=features, kernel="radial", 
                     cost=10^(-1:2), gamma=10^(-2:1))
svm_rbf <- tune_rbf$best.model
svm_rbf_pred <- predict(svm_rbf, features)

set.seed(1)
train_control <- trainControl(method="cv", number=10)
knn_model <- train(Type ~ ., data=features, method="knn", 
                   trControl=train_control, tuneLength=10)


# -- ## -- ## -- ## -- ## - ## - #

svm_linear_pred <- predict(svm_linear, features)
svm_rbf_pred <- predict(svm_rbf, features)
knn_pred <- predict(knn_model, features)

calculate_metrics <- function(pred, actual) {
  cm <- confusionMatrix(pred, actual)
  metrics <- cm$byClass[, c("Precision", "Recall", "F1")]
  metrics[is.na(metrics)] <- 0  # Replace NA with 0
  return(metrics)
}

# Linear SVM Performance:
print(calculate_metrics(svm_linear_pred, features$Type))

# RBF SVM Performance:
print(calculate_metrics(svm_rbf_pred, features$Type))

# kNN Performance:
print(calculate_metrics(knn_pred, features$Type))
