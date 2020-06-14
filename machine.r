library(caret)
library(e1071)
library(progress)
#library(MLeval)

k_par = 10
times = 10

machine_lda_HOI <- function(dataset) {
  
  accuracy    <- rep(NA, k_par * times)
  kappa       <- rep(NA, k_par * times)
  specificity <- rep(NA, k_par * times)
  sensitivity <- rep(NA, k_par * times)
  
  for(t in 1:times){
    set.seed(t)
    folds <- createFolds(dataset$isHOI, k=k_par, list = TRUE)
    for(i in 1:k_par){
      kfolds.train <- dataset[-folds[[i]], ]
      kfolds.test  <- dataset[folds[[i]], ]
      
      model       <- train(isHOI~., data=kfolds.train, method="lda")
      predictions <- predict(model, newdata=kfolds.test)
      result      <- confusionMatrix(kfolds.test$isHOI, predictions)
      
      accuracy[i+k_par*(t-1)]    <- result$overall[1]
      kappa[i+k_par*(t-1)]       <- result$overall[2]
      sensitivity[i+k_par*(t-1)] <- result$byClass[[1]]
      specificity[i+k_par*(t-1)] <- result$byClass[[2]]
    }
  }
  
  return(list("result"  = result,
              "fit"     = model,
              "acc"     = accuracy,
              "kap"     = kappa,
              "spec"    = specificity,
              "sens"    = sensitivity))
}

#---------------------------------

machine_cart_HOI <- function(dataset) {
  
  accuracy    <- rep(NA, k_par * times)
  kappa       <- rep(NA, k_par * times)
  specificity <- rep(NA, k_par * times)
  sensitivity <- rep(NA, k_par * times)
  
  for(t in 1:times){
    set.seed(t)
    folds <- createFolds(dataset$isHOI, k=k_par, list = TRUE)
    for(i in 1:k_par){
      kfolds.train <- dataset[-folds[[i]], ]
      kfolds.test  <- dataset[folds[[i]], ]
      
      model       <- train(isHOI~., data=kfolds.train, method="rpart")
      predictions <- predict(model, newdata=kfolds.test)
      result      <- confusionMatrix(kfolds.test$isHOI, predictions)
      
      accuracy[i+k_par*(t-1)]    <- result$overall[1]
      kappa[i+k_par*(t-1)]       <- result$overall[2]
      sensitivity[i+k_par*(t-1)] <- result$byClass[[1]]
      specificity[i+k_par*(t-1)] <- result$byClass[[2]]
    }
  }
  
  return(list("result" = result,
              "fit"    = model,
              "acc"    = accuracy,
              "kap"    = kappa,
              "spec"   = specificity,
              "sens"   = sensitivity))
}

#---------------------------------

machine_knn_HOI <- function(dataset) {
 
  accuracy    <- rep(NA, k_par * times)
  kappa       <- rep(NA, k_par * times)
  specificity <- rep(NA, k_par * times)
  sensitivity <- rep(NA, k_par * times)
  
  for(t in 1:times){
    set.seed(t)
    folds <- createFolds(dataset$isHOI, k=k_par, list = TRUE)
    for(i in 1:k_par){
      kfolds.train <- dataset[-folds[[i]], ]
      kfolds.test  <- dataset[folds[[i]], ]
      
      model       <- train(isHOI~., data=kfolds.train, method="knn")
      predictions <- predict(model, newdata=kfolds.test)
      result      <- confusionMatrix(kfolds.test$isHOI, predictions)
      
      accuracy[i+k_par*(t-1)]    <- result$overall[1]
      kappa[i+k_par*(t-1)]       <- result$overall[2]
      sensitivity[i+k_par*(t-1)] <- result$byClass[[1]]
      specificity[i+k_par*(t-1)] <- result$byClass[[2]]
    }
  }
  
  return(list("result" = result,
              "fit"    = model,
              "acc"    = accuracy,
              "kap"    = kappa,
              "spec"   = specificity,
              "sens"   = sensitivity))
}

#---------------------------------

machine_svm_HOI <- function(dataset) {
  
  accuracy    <- rep(NA, k_par * times)
  kappa       <- rep(NA, k_par * times)
  specificity <- rep(NA, k_par * times)
  sensitivity <- rep(NA, k_par * times)
  
  for(t in 1:times){
    set.seed(t)
    folds <- createFolds(dataset$isHOI, k=k_par, list = TRUE)
    for(i in 1:k_par){
      kfolds.train <- dataset[-folds[[i]], ]
      kfolds.test  <- dataset[folds[[i]], ]
      
      model       <- train(isHOI~., data=kfolds.train, method="svmRadial")
      predictions <- predict(model, newdata=kfolds.test)
      result      <- confusionMatrix(kfolds.test$isHOI, predictions)
      
      accuracy[i+k_par*(t-1)]    <- result$overall[1]
      kappa[i+k_par*(t-1)]       <- result$overall[2]
      sensitivity[i+k_par*(t-1)] <- result$byClass[[1]]
      specificity[i+k_par*(t-1)] <- result$byClass[[2]]
    }
  }
  
  return(list("result" = result,
              "fit"    = model,
              "acc"    = accuracy,
              "kap"    = kappa,
              "spec"   = specificity,
              "sens"   = sensitivity))
}