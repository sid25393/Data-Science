library(readr)
car <- read_csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/car-data.csv")
View(car)
dt <- rpart(car$`Car Acceptability` ~ ., data = car)
rpart.plot(dt)
set.seed(1234)
split <- 0.8
trainingRowIndex <- sample(1:nrow(car),(split)*nrow(car))  # row indices for training data
trainingData <- car[trainingRowIndex, ]  # model training data
testData  <- car[-trainingRowIndex, ] # model test data 
cartree <- rpart(trainingData$`Car Acceptability` ~ ., data = trainingData, method = "class")
rpart.plot(cartree)
printcp(cartree)
prediction1 <- predict(cartree, testData, type = "class")
table(prediction1)
mean(testData$`Car Acceptability` != prediction1)
summary(cartree)
conf.table <- data.frame(actual= testData$`Car Acceptability`, Prediction = prediction1)
table(conf.table)
library(pROC)
prediction.prob <- predict(cartree, testData, type = "prob")
plot(roc(testData$`Car Acceptability`,prediction.prob[,2]))
auc(testData$`Car Acceptability`,prediction.prob[,2])
correct_predictions <- 114+218
total_predictions <- 114+218+13+1
ratio_predictions <- correct_predictions/total_predictions
ratio_predictions
