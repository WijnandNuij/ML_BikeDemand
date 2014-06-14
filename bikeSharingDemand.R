# rm(list=ls(all=TRUE))

runTestSet <- function(targetLocation='/home/wijnand/R_workspace_bikedemand/resources/result.csv')
{
        trainData <- loadData()
        trainData <- addDateVariables(trainData)
        
        trainDataWeek <- trainData[trainData$weekday=="1" | trainData$weekday=="2"| trainData$weekday=="3" | trainData$weekday=="4" 
                               | trainData$weekday=="5",]
        
        trainDataWeekend <- trainData[trainData$weekday=="6" | trainData$weekday=="7",]
        trainDataWeekend$holiday <- NULL
        
        print(str(trainDataWeek))
        m5treeModelWeek <- m5Tree(trainDataWeek[-1])
        m5treeModelWeekend <- m5Tree(trainDataWeekend[-1])
        
        testData <- loadData('/home/wijnand/R_workspace_bikedemand/resources/test.csv')
        testData <- addDateVariables(testData)
        testDataWeek <- testData[testData$weekday=="1" | testData$weekday=="2"| testData$weekday=="3" | testData$weekday=="4" 
                                                   | testData$weekday=="5",]
        testDataWeekend <- testData[testData$weekday=="6" | testData$weekday=="7",]
        
        result_m5week <- predict(m5treeModelWeek, testDataWeek)
        result_m5weekend <- predict(m5treeModelWeekend, testDataWeekend)
        
        result_m5week <- ifelse(result_m5week < 1, 1, result_m5week)
        result_m5weekend <- ifelse(result_m5weekend < 1, 1, result_m5weekend)
        
        result <- NULL
        result$datetime <- append(testDataWeek$datetime, testDataWeekend$datetime)
        result$count <- c(result_m5week, result_m5weekend)
        write.csv(result, targetLocation, quote=F, row.names=F)
}



main <- function(weekend=FALSE)
{
        trainData <- loadData()
        trainData <- addDateVariables(trainData)
        
        if(weekend)
        {
                trainData <- trainData[trainData$weekday=="1" | trainData$weekday=="2"| trainData$weekday=="3" | trainData$weekday=="4" 
                                        | trainData$weekday=="5",]
        }
        else
        {
                trainData <- trainData[trainData$weekday=="6" | trainData$weekday=="7",]
                trainData$holiday <- NULL
        }
        print(str(trainData))
        
        m5treeModel <- m5Tree(trainData[-1])
        regTree <- regTree(trainData[-1])
        linearModel <- linearModel(trainData[-1])
        
        result_m5 <- predict(m5treeModel, trainData[-1])
        result_regTree <- predict(regTree, trainData[-1])
        result_linearModel <- predict(linearModel, trainData[-1])
        
        # values < 0 are not allowed
        result_m5 <- ifelse(result_m5 < 1, 1, result_m5)
        result_regTree <- ifelse(result_regTree < 1, 1, result_regTree)
        result_linearModel <- ifelse(result_linearModel < 1, 1, result_linearModel)
        
        print(paste0('m5tree correlation:              ', cor(result_m5, trainData$count)))
        print(paste0('regression tree correlation:     ', cor(result_regTree, trainData$count)))
        print(paste0('linearModel correlation:         ', cor(result_linearModel, trainData$count)))
}

addDateVariables <- function(data)
{
        date <- strptime(data$datetime, format="%Y-%m-%d %H:%M:%S")
        
        data$hour <- factor(format.POSIXlt(date, "%H"))
        data$month <- factor(format.POSIXlt(date, "%m"))
        data$weekday <- factor(format.POSIXlt(date, "%u"))
        
        # datetime is unique, so remove
        #data$datetime <- NULL
        # workingday be derived from day
        data$workingday <- NULL
        # season will be derived from month
        data$season <- NULL
        #data$holiday <- NULL
        
        data
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
        
        data
}