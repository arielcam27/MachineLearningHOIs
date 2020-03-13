library(caret)
library(e1071)
library(progress)

machineHOI <- function(dataOut, dataOut2, sample1, sample2, sample3) {
  dataset <- rbind(dataOut, dataOut2)
  
  validation_index <- createDataPartition(dataset$isHOI, p=0.80, list=FALSE)
  validation <- dataset[-validation_index,]
  dataset <- dataset[validation_index,]
  
  dim(dataset)
  sapply(dataset, class)
  
  x <- dataset[, 1:21]
  y <- dataset[, 22]
  #featurePlot(x=x, y=y, plot="ellipse")
  
  scales <- list(x=list(relation="free"), y=list(relation="free"))
  #featurePlot(x=x, y=y, plot="density", scales=scales)
  
  # Run algorithms using 10-fold cross validation.
  # We will 10-fold crossvalidation to estimate accuracy.
  # This will split our dataset into 10 parts, train in 9 and test on 1 
  # and release for all combinations of train-test splits. We will also 
  # repeat the process 3 times for each algorithm with different splits 
  # of the data into 10 groups, in an effort to get a more accurate estimate.
  
  control <- trainControl(method="cv", number=100)
  metric <- "Accuracy"
  
  pb <- progress_bar$new(total = 5)
  
  set.seed(7)
  pb$tick()
  pb$tick()
  fit.lda <- train(isHOI~., 
                   data=dataset, 
                   method="lda", 
                   metric=metric, 
                   trControl=control)
  
  set.seed(7)
  pb$tick()
  fit.cart <- train(isHOI~., 
                   data=dataset, 
                   method="rpart", 
                   metric=metric, 
                   trControl=control)
  
  set.seed(7)
  pb$tick()
  fit.knn <- train(isHOI~., 
                    data=dataset, 
                    method="knn", 
                    metric=metric, 
                    trControl=control)
  
  set.seed(7)
  pb$tick()
  fit.svm <- train(isHOI~., 
                    data=dataset, 
                    method="svmRadial", 
                    metric=metric, 
                    trControl=control)
  
  # set.seed(7)
  # pb$tick()
  # fit.rf <- train(isHOI~., 
  #                   data=dataset, 
  #                   method="rf", 
  #                   metric=metric, 
  #                   trControl=control)
  #print(fit.lda)
  
  #predictions <- predict(fit.lda, dummie)
  #predictions <- predict(fit.lda, validation)
  #print(confusionMatrix(predictions, validation$isHOI))
  #return(predict(fit.lda, sample1))
  results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm))
  summary(results)
  predictions1 <- c(predict(fit.lda, sample1),
                    predict(fit.cart, sample1),
                    predict(fit.knn, sample1),
                    predict(fit.svm, sample1))
  predictions2 <- c(predict(fit.lda, sample2),
                    predict(fit.cart, sample2),
                    predict(fit.knn, sample2),
                    predict(fit.svm, sample2))
  predictions3 <- c(predict(fit.lda, sample3),
                    predict(fit.cart, sample3),
                    predict(fit.knn, sample3),
                    predict(fit.svm, sample3))
  return(list("results"=results, 
              "predictions1" = predictions1, 
              "predictions2" = predictions2, 
              "predictions3" = predictions3))
         
# predictions
# validation[1,]
# predictions[1]
}