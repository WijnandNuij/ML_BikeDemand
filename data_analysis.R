# rm(list=ls(all=TRUE))

measure <- function(data)
{
        #xtabs(count ~ hour + workingday , data= data)
        xtabs(count ~ season, data= data)
        
}

totalDiff <- function(data)
{
  print(sum(abs(data$diff)))
}

tempHumitidytable <- function(data)
{
  print(mean(data$count))
  tabelletje <- NULL
  tabelletje$humCat <- cut(data$humidity, breaks=c(0, 20, 40, 60, 80, 100))
  tabelletje$tempCat <- cut(data$temp, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
  tabelletje$count <- data$count
  
  xtabs(count ~ humCat + tempCat, aggregate(count ~ humCat + tempCat,tabelletje,mean))
}

tempWindtable <- function(data)
{
        print(mean(data$count))
        tabelletje <- NULL
        tabelletje$windCat <- cut(data$windspeed, breaks=c(0, 10, 20, 30, 40, 50, 60))
        tabelletje$tempCat <- cut(data$temp, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
        tabelletje$count <- data$count
        
        xtabs(count ~ windCat + tempCat, aggregate(count ~ windCat + tempCat,tabelletje,mean))
}

tempWeathertable <- function(data)
{
        print(mean(data$count))
        tabelletje <- NULL
        tabelletje$tempCat <- cut(data$temp, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
        tabelletje$weather <- data$weather
        tabelletje$count <- data$count
        
        xtabs(count ~ weather + tempCat, aggregate(count ~ weather + tempCat,tabelletje,mean))
}

windspeedTable <- function(data)
{
        print(mean(data$count))
        aggregate(data$count, list(cut(data$windspeed, breaks=c(0, 10, 20, 30, 40, 50, 60))), mean)
}

temperatureTable <- function(data)
{
  print(mean(data$count))
  aggregate(data$count, list(cut(data$temp, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))), mean)
}