# rm(list=ls(all=TRUE))

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_bikedemand/resources/result.csv')
{
  trainData <- loadData()
  trainData <- addDateVariables(trainData)
  
  print(str(trainData))
  require(randomForest)
  
  trainedModel <- m5Tree(trainData[-1])
  print('trained model')
  
  testData <- loadData('/home/wijnand/R_workspace_bikedemand/resources/test.csv')
  testData <- addDateVariables(testData)
    
  result_model <- predict(trainedModel, testData)
  print('prediction done')
  
  result_model <- ifelse(result_model < 1, 1, result_model)
  
  result <- NULL
  result$datetime <- testData$datetime
  result$count <- result_model
  write.csv(result, targetLocation, quote=F, row.names=F)
}

main <- function(weekend=FALSE)
{
  trainData <- loadData()
  trainData <- addDateVariables(trainData)

  print(str(trainData))
  
  #m5treeModel <- m5Tree(trainData[-1])
  #regTree <- regTree(trainData[-1])
  #linearModel <- linearModel(trainData[-1])
  randomForestModel <- randomForestTree(trainData[-1])
  #caretModel <- caret(trainData[-1])
  
  #result_m5 <- predict(m5treeModel, trainData[-1])
  #result_regTree <- predict(regTree, trainData[-1])
  #result_linearModel <- predict(linearModel, trainData[-1])
  result_randomForest <- predict(randomForestModel, trainData[-1])
  #result_caret <- predict(caretModel, trainData[-1])
  
  # values < 0 are not allowed
  #result_m5 <- ifelse(result_m5 < 1, 1, result_m5)
  #result_regTree <- ifelse(result_regTree < 1, 1, result_regTree)
  #result_linearModel <- ifelse(result_linearModel < 1, 1, result_linearModel)
  result_randomForest <- ifelse(result_randomForest < 1, 1, result_randomForest)
  #result_caret <- ifelse(result_caret < 1, 1, result_caret)
  
  #print(paste0('m5tree correlation:              ', cor(result_m5, trainData$count)))
  #print(paste0('regression tree correlation:     ', cor(result_regTree, trainData$count)))
  #print(paste0('linearModel correlation:         ', cor(result_linearModel, trainData$count)))
  print(paste0('randomForstModel correlation:    ', cor(result_randomForest, trainData$count)))
  #print(paste0('caret correlation:               ', cor(result_caret, trainData$count)))
  
  #print(paste0('m5tree root mean squared error:           ', sqrt(mean((result_m5 - trainData$count)^2))))
  #print(paste0('regression root mean squared error:       ', sqrt(mean((result_regTree - trainData$count)^2))))
  #print(paste0('linearModel root mean squared error:      ', sqrt(mean((result_linearModel - trainData$count)^2))))
  print(paste0('randomForstModel root mean squared error: ', sqrt(mean((result_randomForest - trainData$count)^2))))
  #print(paste0('caret randomForstModel root mean squared error: ', sqrt(mean((result_caret - trainData$count)^2))))
  
  #print(summary(trainData$count))
  #print(summary(result_randomForest))
  #print(summary(result_m5))
  
  #print(summary(caretModel))
  print(summary(trainData$count))
  print(summary(result_randomForest))

}

addDateVariables <- function(data)
{
  date <- strptime(data$datetime, format="%Y-%m-%d %H:%M:%S")
  
  data$hour <- factor(format.POSIXlt(date, "%H"))
  data$month <- factor(format.POSIXlt(date, "%m"))
  data$weekday <- factor(format.POSIXlt(date, "%u"))
    
  # season will be derived from month
  data$season <- NULL
  #data$holiday <- NULL
  
  data$avondspitslekkerweertje <- ifelse(data$holiday=="0" & data$workingday==1 & (data$hour=="18" | data$hour=="17") & 
                                           data$atemp > 24, 1, 0)
  
  # workingday be derived from day
  data$workingday <- NULL

  data
}

randomForestTree <- function(trainData)
{
  require(randomForest)
  trainedModel <- randomForest(count ~ . , trainData, mtry=16, importance=T)
  print(trainedModel)
  trainedModel
}

caret <- function(trainData)
{
  require(caret)
  ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
  grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
  set.seed(300)
  m_rf <- train(count ~ ., data = trainData, method = "rf", metric = "RMSE", trControl = ctrl, tuneGrid = grid_rf)
  print(m_rf)
  m_rf
}

m5Tree <- function(trainData)
{
  require(RWeka)
  trainedModel <- M5P(count ~ . , trainData)
  print(trainedModel)
  trainedModel
}

regTree <- function(trainData)
{
  require(rpart)
  trainedModel <- rpart(count ~ . , trainData)
  print(trainedModel)
  #require(rpart.plot)
  #rpart.plot(trainedModel, digits = 5)
  
  trainedModel
}

linearModel <- function(trainData)
{
  trainedModel <- lm(count ~ . , trainData)
  print(trainedModel)
  trainedModel
}


writeResult <- function(resultData, testLocation, targetLocation='/home/wijnand/R_workspace_bikedemand/resources/result.csv')
{
  data <- read.csv(testLocation)
  datetime <- data$datetime
  
  result <- NULL
  result$datetime <- datetime
  result$count <- resultData
  
  write.csv(result, targetLocation, quote=F, row.names=F)
}


loadData <- function(location='/home/wijnand/R_workspace_bikedemand/resources/train.csv')
{
  data <- read.csv(location)
  
  data$season <- factor(data$season)
  data$holiday <- factor(data$holiday)
  data$weather <- factor(data$weather)
  data$datetime <- as.character(data$datetime)
  
  data$casual <- NULL
  data$registered <- NULL
  
  # temp & atemp have a .98 correlation so are basically the same
  data$temp <- NULL
  
  
  data
}