library(caret)
library(e1071)

machineHOIlda <- function(dataOut, dataOut2, sample1) {
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

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Linear Discriminant Analysis (LDA)
set.seed(7)
fit.lda <- train(isHOI~., 
                 data=dataset, 
                 method="lda", 
                 metric=metric, 
                 trControl=control)

print(fit.lda)

#predictions <- predict(fit.lda, dummie)
predictions <- predict(fit.lda, validation)
print(confusionMatrix(predictions, validation$isHOI))
return(predict(fit.lda, sample1))
# predictions
# validation[1,]
# predictions[1]
}