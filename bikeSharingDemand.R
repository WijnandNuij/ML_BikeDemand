# rm(list=ls(all=TRUE))

runTraining <- function(percentageTrain=0.7)
{
        completeSet <- loadData()
        completeSet <- addDateVariables(completeSet)
        completeSet <- makeNumeric(completeSet)
                
        # order randomly
        set.seed(159)
        completeSet <- completeSet[order(runif(nrow(completeSet))),]
        
        # row number of x% of the data for training set
        percentageTrain <- round(nrow(completeSet) * percentageTrain, digits=0) - 1
        trainingSet <- completeSet[1:percentageTrain,]
        testSet <- completeSet[(percentageTrain+1):nrow(completeSet),]
        
        result_extraTrees <- predictWithExtraTrees(trainingSet[-1], testSet[-1], "extraTrees")
        #result_m5 <- predictWithModel(m5Tree(trainingSet[-1]), testSet, "m5tree")
        #result_lm <- predictWithModel(linearModel(trainingSet[-1]), testSet, "linearModel")
        #result_reg <- predictWithModel(regTree(trainingSet[-1]), testSet, "regTree")
        #result_rfCaret <- predictWithModel(randomForestTreeCaret(trainingSet[-1]), testSet, "randomForestTreeCaret")
        
        #result_rf <- predictWithModel(randomForestTree(trainingSet[-1]), testSet, "randomForestTree")
        #result_gbm <- predictWithGbm(trainingSet[-1], testSet, "gbm")
        #result_caretM5 <- predictWithModel(caretM5(trainingSet[-1]), testSet, "caretm5")
        
        
        #result_m5
}


predictWithExtraTrees <- function(trainingSet, testData, name)
{
        library(extraTrees)
        options(java.parameters = "-Xmx8g,-XX:-UseGCOverheadLimit,-XX:+UseConcMarkSweepGC")
        
        resultTrain <- trainingSet$count
        trainingSet$count <- NULL
        
        resultTest <- testData$count
        testData$count <- NULL
        
        print(str(trainingSet))
        
        trainedModel <- extraTrees(trainingSet, 
                                   resultTrain, 
                                   mtry = 10,
                                   ntree = 600, 
                                   numRandomCuts = 5, 
                                   numThreads = 4)
        result <- predict(trainedModel, testData)
        require(Metrics)
        print(paste0(name, ' rmsle: ', round(rmsle(result, resultTest), digits=3)))
        result
}



predictWithGbm <- function(trainData, testData, name)
{
  require(gbm)
  trainedModel <- gbm(count ~ ., trainData, distribution = "poisson", n.trees = 150, shrinkage=0.01, interaction.depth = 3)
  result <- predict.gbm(trainedModel, testData, trainedModel$n.trees)
  
  require(Metrics)
  print(paste0(name, ' rmsle: ', round(rmsle(result, testData$count), digits=3)))
  result
}

predictWithModel <- function(genericModel, testData, name)
{
  result <- predict(genericModel, testData)
  result <- ifelse(result < 1, 1, result)
  
  require(Metrics)
  print(paste0(name, ' rmsle: ', round(rmsle(result, testData$count), digits=3)))
  
  testData <- cbind(testData, result)
  testData$diff <- testData$result - testData$count
  testData
}

makeNumeric <- function(completeSet)
{
        library(caret)
        dummyVariables <- dummyVars(~season + holiday + workingday + weather + hour + month + weekday + year, completeSet)
        wcmatrix <- predict(dummyVariables, newdata=completeSet)
        
        nums <- sapply(completeSet, is.factor)
        noFactor <- completeSet[!nums]
        
        completeSet <- cbind(noFactor, wcmatrix)
}

addDateVariables <- function(data)
{
  date <- strptime(data$datetime, format="%Y-%m-%d %H:%M:%S")
  
  data$hour <- factor(as.numeric(format.POSIXlt(date, "%H")))
  #data$hour <- factor(data$hour)
  data$month <- factor(format.POSIXlt(date, "%m"))
  data$weekday <- factor(format.POSIXlt(date, "%u"))
  data$year <- factor(format.POSIXlt(date, "%Y"))
  
  #data$datetime <- NULL
  
  #data$hourtype <- ifelse(data$hour <= 5, "nacht", NA)
  #data$hourtype <- ifelse(data$workingday==0 & data$hour > 5, "vrijedag_overdag", data$hourtype)
  
  #data$hourtype <- ifelse(data$workingday==1 & data$hour==6, "werkdag_vroeg", data$hourtype)
  #data$hourtype <- ifelse(data$workingday==1 & data$hour==8, "werkdag_ochtendspits", data$hourtype)
  #data$hourtype <- ifelse(data$workingday==1 & (data$hour==17 | data$hour==18), "werkdag_middagspits", data$hourtype)
  #data$hourtype <- ifelse(data$workingday==1 & (data$hour==7 | data$hour==9), "werkdag_rondochtendspits", data$hourtype)
  #data$hourtype <- ifelse(data$workingday==1 & (data$hour==16 | data$hour==19), "werkdag_rondavondspits", data$hourtype)
  #data$hourtype <- ifelse(data$workingday==1 & data$hour>=10 & data$hour<=16, "werkdag_middag", data$hourtype)
  #data$hourtype <- ifelse(data$workingday==1 & (data$hour==20 | data$hour==21), "werkdag_vooravond", data$hourtype)
  #data$hourtype <- ifelse(data$workingday==1 & data$hour>=22, "werkdag_avond", data$hourtype)

  #data$hourtype <- factor(data$hourtype)
  #data$hourtype <- NULL
  
    
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
  trainedModel <- randomForest(count ~ . , trainData, mtry=8, do.trace = T, ntree = 250, importance = T, 
                               oob.prox = T, nPerm = 1)
  #print(summary(trainedModel))
  trainedModel
}

randomForestTreeCaret <- function(trainData)
{
        library(caret)
        #trainedModel <- randomForest(count ~ . , trainData, mtry=8, do.trace = T, ntree = 250, importance = T, 
        #                             oob.prox = T, nPerm = 1)
        trainedModel <- train(count ~ . , trainData, 
                              method="rf", 
                              metric = "RMSE",
                              trControl = trainControl(method = "repeatedcv", number = 3, repeats = 3),
                              do.trace=T,
                              ntree=50)
        #print(summary(trainedModel))
        trainedModel
}

caretM5 <- function(trainData)
{
  require(caret)
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  model <- train(count ~ ., data = trainData,method = "gbm",trControl = fitControl,verbose = F)
  model
}

m5Tree <- function(trainData)
{
  require(RWeka)
  trainedModel <- M5P(count ~ . , trainData, control = Weka_control(M=1))
}

regTree <- function(trainData)
{
  require(rpart)
  trainedModel <- rpart(count ~ . , trainData)
}

linearModel <- function(trainData)
{
  trainedModel <- lm(count ~ . , trainData)
}

loadData <- function(location='/home/wijnand/R_workspace_bikedemand/resources/train.csv')
{
  data <- read.csv(location)
  
  data$season <- factor(data$season)
  data$holiday <- factor(data$holiday)
  data$weather <- factor(data$weather)
  data$workingday <- factor(data$workingday)
  data$datetime <- as.character(data$datetime)
  
  # not present in testset
  data$casual <- NULL
  data$registered <- NULL

  data
}

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_bikedemand/resources/result.csv')
{
        trainData <- loadData()
        trainData <- addDateVariables(trainData)
        trainData <- makeNumeric(trainData)
        
        library(extraTrees)
        options(java.parameters = "-Xmx8g,-XX:-UseGCOverheadLimit,-XX:+UseConcMarkSweepGC")
        
        print(str(trainData[-1]))
        
        resultTrain <- trainData$count
        trainData$count <- NULL
        trainData$datetime <- NULL
        
        trainedModel <- extraTrees(trainData, 
                                   resultTrain, 
                                   mtry = 10,
                                   ntree = 600, 
                                   numRandomCuts = 5, 
                                   numThreads = 4)
        
        print('trained model')
        
        testData <- loadData('/home/wijnand/R_workspace_bikedemand/resources/test.csv')
        testData <- addDateVariables(testData)
        testData <- makeNumeric(testData)
        
        print(str(testData))
        
        result_model <- predict(trainedModel, testData[-1])
        print('prediction done')
        
        result_model <- ifelse(result_model < 1, 1, result_model)
        
        result <- NULL
        result$datetime <- testData$datetime
        result$count <- result_model
        write.csv(result, targetLocation, quote=F, row.names=F)
}