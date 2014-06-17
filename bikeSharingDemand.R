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
  
  m5treeModel <- m5Tree(trainData[-1])
  print(m5treeModel)
  regTree <- regTree(trainData[-1])
  linearModel <- linearModel(trainData[-1])
  #randomForestModel <- randomForestTree(trainData[1:4000, -1])
  #caretModel <- caret(trainData[-1])
  
  result_m5 <- predict(m5treeModel, trainData[-1])
  result_regTree <- predict(regTree, trainData[-1])
  result_linearModel <- predict(linearModel, trainData[-1])
  #result_randomForest <- predict(randomForestModel, trainData[-1])
  #result_caret <- predict(caretModel, trainData[-1])
  
  # values < 0 are not allowed
  result_m5 <- ifelse(result_m5 < 1, 1, result_m5)
  result_regTree <- ifelse(result_regTree < 1, 1, result_regTree)
  result_linearModel <- ifelse(result_linearModel < 1, 1, result_linearModel)
  #result_randomForest <- ifelse(result_randomForest < 1, 1, result_randomForest)
  #result_caret <- ifelse(result_caret < 1, 1, result_caret)
  
  print(paste0('m5tree correlation:              ', cor(result_m5, trainData$count)))
  print(paste0('regression tree correlation:     ', cor(result_regTree, trainData$count)))
  print(paste0('linearModel correlation:         ', cor(result_linearModel, trainData$count)))
  #print(paste0('randomForstModel correlation:    ', cor(result_randomForest, trainData$count)))
  #print(paste0('caret correlation:               ', cor(result_caret, trainData$count)))
  
  print(paste0('m5tree root mean squared error:           ', sqrt(mean((result_m5 - trainData$count)^2))))
  print(paste0('regression root mean squared error:       ', sqrt(mean((result_regTree - trainData$count)^2))))
  print(paste0('linearModel root mean squared error:      ', sqrt(mean((result_linearModel - trainData$count)^2))))
  #print(paste0('randomForstModel root mean squared error: ', sqrt(mean((result_randomForest - trainData$count)^2))))
  #print(paste0('caret randomForstModel root mean squared error: ', sqrt(mean((result_caret - trainData$count)^2))))
  
  require(Metrics)
  print(paste0('m5tree rmsle:                    ', rmsle(result_m5, trainData$count)))
  print(paste0('regression rmsle:                ', rmsle(result_regTree, trainData$count)))
  print(paste0('linearModel rmsle:               ', rmsle(result_linearModel, trainData$count)))
  #print(paste0('randomForstModel rmsle:          ', rmsle(result_randomForest, trainData$count)))
  #print(paste0('caret:            ', rmsle(result_m5, trainData$count)))
  
  trainData <- cbind(trainData, result_m5)
  trainData$diff <- trainData$result_m5 - trainData$count
  trainData
}

addDateVariables <- function(data)
{
  date <- strptime(data$datetime, format="%Y-%m-%d %H:%M:%S")
  
  data$hour <- as.numeric(format.POSIXlt(date, "%H"))
  data$month <- factor(format.POSIXlt(date, "%m"))
  data$weekday <- factor(format.POSIXlt(date, "%u"))
  
  data$hourtype <- ifelse(data$hour <= 5, "nacht", NA)
  data$hourtype <- ifelse(data$workingday==0 & data$hour > 5, "vrijedag_overdag", data$hourtype)
  
  data$hourtype <- ifelse(data$workingday==1 & data$hour==6, "werkdag_vroeg", data$hourtype)
  data$hourtype <- ifelse(data$workingday==1 & data$hour==8, "werkdag_ochtendspits", data$hourtype)
  data$hourtype <- ifelse(data$workingday==1 & (data$hour==17 | data$hour==18), "werkdag_middagspits", data$hourtype)
  data$hourtype <- ifelse(data$workingday==1 & (data$hour==7 | data$hour==9), "werkdag_rondochtendspits", data$hourtype)
  data$hourtype <- ifelse(data$workingday==1 & (data$hour==16 | data$hour==19), "werkdag_rondavondspits", data$hourtype)
  data$hourtype <- ifelse(data$workingday==1 & data$hour>=10 & data$hour<=16, "werkdag_middag", data$hourtype)
  data$hourtype <- ifelse(data$workingday==1 & (data$hour==20 | data$hour==21), "werkdag_vooravond", data$hourtype)
  data$hourtype <- ifelse(data$workingday==1 & data$hour>=22, "werkdag_avond", data$hourtype)

  data$hourtype <- factor(data$hourtype)
  data$hourtype <- NULL
  #data$hour <- NULL
    
  # season will be derived from month
  #data$season <- NULL
  #data$holiday <- NULL
  
  # workingday be derived from day
  #data$workingday <- NULL

  data
}

randomForestTree <- function(trainData)
{
  require(randomForest)
  trainedModel <- randomForest(count ~ . , trainData, mtry=8)
  #print(summary(trainedModel))
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
  
  #print(summary(trainedModel))
  trainedModel
}

regTree <- function(trainData)
{
  require(rpart)
  trainedModel <- rpart(count ~ . , trainData)
  #print(summary(trainedModel))
  #require(rpart.plot)
  #rpart.plot(trainedModel, digits = 5)
  
  trainedModel
}

linearModel <- function(trainData)
{
  trainedModel <- lm(count ~ . , trainData)
  #print(summary(trainedModel))
  trainedModel
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