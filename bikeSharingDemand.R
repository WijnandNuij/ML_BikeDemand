# rm(list=ls(all=TRUE))

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_bikedemand/resources/result.csv')
{
  trainData <- loadData()
  trainData <- addDateVariables(trainData)
  
  print(str(trainData))

  trainedModel <- randomForestTree(trainData[-1])
  print('trained model')
  
  testData <- loadData('/home/wijnand/R_workspace_bikedemand/resources/test.csv')
  testData <- addDateVariables(testData)
    
  result_model <- predict(trainedModel, testData)
  print('prediction done')
  
  result_model <- ifelse(result_model < 1, 1, result_model)
  result_model <- ifelse(is.na(result_model), mean(result_model, na.rm=T), result_model)
  
  result <- NULL
  result$datetime <- testData$datetime
  result$count <- result_model
  write.csv(result, targetLocation, quote=F, row.names=F)
}

main <- function(percentageTrain=0.5)
{
  completeSet <- loadData()
  completeSet <- addDateVariables(completeSet)
  
  # order randomly
  set.seed(159)
  completeSet <- completeSet[order(runif(nrow(completeSet))),]
  
  # row number of x% of the data for training set
  percentageTrain <- round(nrow(completeSet) * percentageTrain, digits=0) - 1
  trainingSet <- completeSet[1:percentageTrain,]
  testSet <- completeSet[(percentageTrain+1):nrow(completeSet),]
  
  print(str(trainingSet))
  
  result_m5 <- predictWithModel(m5Tree(trainingSet[-1]), testSet, "m5tree")
  result_lm <- predictWithModel(linearModel(trainingSet[-1]), testSet, "linearModel")
  result_reg <- predictWithModel(regTree(trainingSet[-1]), testSet, "regTree")
  result_rf <- predictWithModel(randomForestTree(trainingSet[-1]), testSet, "randomForestTree")
  result_cTree <- predictWithModel(cTree(trainingSet[-1]), testSet, "cTree")
  #result_gbm <- predictWithGbm(trainingSet[-1], testSet, "gbm")
  #result_caretM5 <- predictWithModel(caretM5(trainingSet[-1]), testSet, "caretm5")
  
  result_m5
}

predictWithGbm <- function(trainData, testData, name)
{
  require(gbm)
  trainedModel <- gbm(count ~ ., trainData, distribution = "poisson", n.trees = 150, shrinkage=0.01, interaction.depth = 3)
  result <- predict.gbm(trainedModel, testData)
  
  print(paste0(name, ' correlation: ', round(cor(result, testData$count), digits=3)))
  
  require(Metrics)
  print(paste0(name, ' rmsle: ', round(rmsle(result, testData$count), digits=3)))
  result
}

predictWithModel <- function(genericModel, testData, name)
{
  result <- predict(genericModel, testData)
  row.names(result) <- NULL
  result <- ifelse(result < 1, 1, result)
  result <- ifelse(is.na(result), mean(result, na.rm=T), result)
  #print(paste0(name, ' correlation: ', round(cor(result, testData$count), digits=3)))
  
  require(Metrics)
  print(paste0(name, ' rmsle: ', round(rmsle(result, testData$count), digits=3)))
  
  testData <- cbind(testData, result)
  testData$diff <- testData$result - testData$count
  testData
}

randomForestTree <- function(trainData)
{
  require(randomForest)
  trainedModel <- randomForest(count ~ . , trainData, na.action=na.roughfix, do.trace=T, ntree=250, mtry=12, importance=T, oob.prox=T)
  print(trainedModel)
  print(importance(trainedModel))
  trainedModel
}

caret <- function(trainData)
{
  require(caret)
  ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
  grid_rf <- expand.grid(.mtry = c(4, 8, 16))
  set.seed(301)
  m_rf <- train(count ~ ., data = trainData, method = "rf", metric = "RMSE", trControl = ctrl, tuneGrid = grid_rf)
  #print(m_rf)
  m_rf
}

caretM5 <- function(trainData)
{
  require(caret)
  fitControl <- trainControl(method = "cv", number = 2, repeats = 2)
  #model <- train(count ~ ., data = trainData, method = "M5", metric = "RMSE", trControl=fitControl)
  model <- train(count ~ ., data = trainData,method = "gbm",trControl = fitControl,verbose = F)
  print(model)
  model
}

m5Tree <- function(trainData)
{
  require(RWeka)
  trainedModel <- M5P(count ~ . , trainData)
  
  write_to_dot(trainedModel, con=file("/home/wijnand/test.dot", "w"))
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

cTree <- function(trainData)
{
        require(party)
        trainedModel <- ctree(count ~ . , trainData)
        #plot(trainedModel, main="CTree")
        trainedModel
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
        
        data$hourtype <- relevel(factor(data$hourtype), ref="nacht")
        #data$hourtype <- NULL
        data$hour <- relevel(factor(data$hour), ref="17")
        #data$hour <- NULL
        data
}

loadData <- function(location='/home/wijnand/R_workspace_bikedemand/resources/train.csv')
{
  data <- read.csv(location)
  
  data$season <- factor(data$season)
  data$holiday <- factor(data$holiday)
  data$weather <- factor(data$weather)
  
  data$workingday <- relevel(factor(data$workingday), ref="1")
  
  data$datetime <- as.character(data$datetime)
  
  # not present in TEST data
  data$casual <- NULL
  data$registered <- NULL
  
  # seems useless (no correlation at all)
  data$windspeed <- NULL
  
  # season will be derived from month
  data$season <- NULL
  # workingday
  data$holiday <- NULL
  
  data$humidityCategory <- relevel(factor(cut(data$humidity, breaks=c(0, 20, 40, 60, 80, 100))), ref="(40,60]")
  data$tempCategory <- relevel(factor(cut(data$temp, breaks=c(0, 5, 10, 15, 20, 25, 30, 35))), ref="(20,25]")
  
  # temp & atemp have a .98 correlation so are basically the same
  data$temp <- NULL
  
  data
}