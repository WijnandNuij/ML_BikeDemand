
measure <- function(data)
{
        #xtabs(count ~ hour + workingday , data= data)
        xtabs(count ~ hourtype , data= data)
}