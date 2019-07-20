
#' @title Outliers summary after log transformation – an extension of Outlier_wo_data_transformation function
#'
#' @description Summary of dataframe of Oultiers nos in Lower and upper bound before and after log transformation
#'
#' @param data1  columnn1, first_function
#'
#' @return NULL
#'
#' @examples  Outlier_after_log_transformation(Boston, columnn1 ((User can use this parameter as it is)) , first_function (call the function “Outlier_wo_data_transformation))
#'
#' @export Outlier_after_log_transformation



Outlier_after_log_transformation <- function(data1, columnn1, first_function)

{

  variable_name_1 <- c()
  Log_Lower_count <- c()
  Log_Upper_count <- c()
  Log_Outlier_detect <- c()

  for(a in 1:ncol(data1))
  {
    columnn1 <- c(a)

    for(var1 in columnn1)
    {
      if(is.factor(data1[, var1]) | (length(unique(data1[,var1]))/nrow(data1)*100 < 5))
      {
        print(paste(names(data1)[var1],'is a categorical variable'))

      }
      else if(is.numeric(data1[, var1])) #
      {
        {
          for(x in 1:nrow(data1))
          {
            if(data1[x,var1] == 0)

            {
              data1[x,var1] <- log10(data1[x,var1]+1)
            }

            else

              data1[x,var1] <- log10(data1[x,var1])
          }

          Log_Upperbound = min(max(data1[,var1]), (quantile(data1[,var1],0.75) + 1.5*IQR(data1[,var1])))
          Log_Lowerbound = max(min(data1[,var1]), (quantile(data1[,var1],0.25) - 1.5*IQR(data1[,var1])))


          loglowerout <- c()
          logupperout <- c()

          for(b in data1[,var1])
          {
            if(b < Log_Lowerbound)
            {
              loglowerout <- c(loglowerout,b)
            }
          }

          for(c in data1[,var1])
          {
            if(c > Log_Upperbound)
            {
              logupperout <- c(logupperout,c)
            }
          }

          variable_name_1 <- variable_name_1[!is.na(variable_name_1)]
          loglowerout <- loglowerout[!is.na(loglowerout)]
          logupperout <- logupperout[!is.na(logupperout)]

          Log_Lower_count <- Log_Lower_count[!is.na(Log_Lower_count)]
          Log_Upper_count <- Log_Upper_count[!is.na(Log_Upper_count)]

          variable_name_1[a] <- names(data1[var1])
          Log_Lower_count[a] <- length(loglowerout)
          Log_Upper_count[a] <- length(logupperout)

        }

      }

    }

  }


  df1 <- data.frame(variable_name_1, Log_Lower_count, Log_Upper_count)

  df1 <- df1[-which(is.na(df1$variable_name_1)),]

  rownames(df1) <- 1:nrow(df1)

  for(d in 1:nrow(df1))
  {
    if((df1[d,2] == 0) & (df1[d,3]== 0))
    {
      Log_Outlier_detect <- c(Log_Outlier_detect, "No outlier detected")
    }

    else

      Log_Outlier_detect <- c(Log_Outlier_detect, "Outlier detected")
  }

  df1$Log_Outlier_detection <- Log_Outlier_detect

  df4<-cbind(first_function, df1[, 2:ncol(df1)])

  return(df4)

}
