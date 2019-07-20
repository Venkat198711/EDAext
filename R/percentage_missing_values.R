
#' @title Missing values summary
#'
#' @description Printing dataframe of total missing values and it's percentage
#'
#' @param data
#'
#' @return NULL
#'
#' @examples  percentage_missing_values("Boston")
#'
#' @export percentage_missing_values


percentage_missing_values <- function(data)
{
  var_name <- c()
  nos_miss_values <- c()
  miss_values_percent <- c()

  for(i in 1:ncol(data))
  {

    nos_miss_values <- c(nos_miss_values, sum(is.na(data[,i]))) #passing missing values counts
    miss_values_percent <- c(miss_values_percent, sum(is.na(data[,i]))/length(data[,i])*100) #converting the counts into percentage missing values
    miss_values_percent <- round(miss_values_percent, digits = 3)
    var_name <- c(var_name, names(data[i]))
  }

  df5 <- data.frame(var_name, nos_miss_values, miss_values_percent)

  df5

}
