
measure <- function(data)
{
        #xtabs(count ~ hour + workingday , data= data)
        xtabs(diff ~ hour + workingday , data= data)
}

totalDiff <- function(data)
{
  print(sum(abs(data$diff)))
}

humidityTable <- function(data)
{
  print(mean(data$count))
  aggregate(data$count, list(cut(data$humidity, breaks=c(0, 20, 40, 60, 80, 100, 120))), mean)
}

temperatureTable <- function(data)
{
  print(mean(data$temp))
  aggregate(data$count, list(cut(data$temp, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))), mean)
}