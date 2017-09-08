# Author: Haifeng Liu
# This program is to build a prediction model of loss estimation for a given enterprise
# using the (sub) set of vcdb data
# Input: the csv file of VCDB data (selected columns) with amount of financial loss in USD
# Output: a regression model and its performance

library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

# Load the source data from a vcdb file
sourceDataFile <- "dataFiles/curatedData.csv"
trainingData <- fread(sourceDataFile)
#str(trainingData)


# Example code for using rpart
# m1 <- rpart(impact.overall_amount ~ ., data=sourceData, method="anova")
# rpart.plot(m1, type=3, digits=3, fallen.leaves=TRUE)
#
# # Make prediction
# p1 <- predict(m1, sourceData)
# # Evaluate the model
# mae1 <- MAE(sourceData$impact.overall_amount, p1)

# Prepare the feature sets
targetFeature <- 'impact.overall_amount'
featureSetLength <- 2
predictingFeatures1 <- c('victim.industry.name','victim.orgsize.Small', 'victim.orgsize.Large',
                         'victim.employee_count.Large','victim.employee_count.Small',
                         'victim.employee_count.Unknown', 'victim.country.US')
predictingFeatures2 <- c('victim.industry.name','victim.orgsize.Small', 'victim.orgsize.Large',
                         'victim.employee_count.1 to 10', 'victim.employee_count.10001 to 25000',
                        'victim.employee_count.1001 to 10000', 'victim.employee_count.101 to 1000',
                        'victim.employee_count.11 to 100', 'victim.employee_count.25001 to 50000',
                        'victim.employee_count.50001 to 100000', 'victim.employee_count.Over 100000',
                        'victim.employee_count.Unknown', 'victim.country.US')
# predictingFeatures2 <- c('victim.industry.name','victim.orgsize.Small', 'victim.orgsize.Large',
#                          'victim.employee_count.10001 to 25000',
#                          'victim.employee_count.1001 to 10000', 'victim.employee_count.101 to 1000',
#                          'victim.employee_count.11 to 100',
#                          'victim.employee_count.Over 100000',
#                          'victim.employee_count.Unknown', 'victim.country.US')
predictingFeatureSet <- vector("list", featureSetLength)
for (i in 1:featureSetLength) {
  x <- paste("predictingFeatures", i, sep = "")
  predictingFeatureSet[[i]] <- eval(as.name(x))
}

# prepare the learning algorithms that will be tried
learningAlgos <- c("lm", "glm")

# prepare resampling method
control <- trainControl(method="cv", number=5)

# Training models using caret
set.seed(7)
for (i in 1:featureSetLength) {
  predictFeatures <- predictingFeatureSet[[i]]
  trCols <- append(predictFeatures, targetFeature)
  trData <- trainingData[, trCols, with=FALSE]
  for (algo in learningAlgos) {
    #featurePara <- paste(targetFeature, "~ ")
    #featurePara <- paste(featurePara, paste(predictFeatures, collapse = "+"))
    cat("Training using feature set: ", i, " algo: ", algo, "\n")
    # fit <- train( impact.overall_amount ~., data = trData, method = algo,
    #             metric = "RMSE", trControl = control)
    fit <- train( impact.overall_amount ~., data = trData, method = algo,
                  metric = "RMSE")
    # display results
    print(fit)

  }
}


