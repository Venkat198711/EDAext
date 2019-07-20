
#' @title Outliers without any data transformation
#'
#' @description Summary of dataframe of Oultiers nos in Lower and upper bound without any data transformation
#'
#' @param data columnn
#'
#' @return NULL
#'
#' @examples  Outlier_wo_data_transformation(Boston, columnn (user can pass this parameter as it is))
#'
#' @export Outlier_wo_data_transformation


Outlier_wo_data_transformation <- function(data, columnn)

{

  variable_name <- c()
  Lower_count <- c()
  Upper_count <- c()
  Outlier_detect <- c()

  for(i in 1:ncol(data))
  {

    # Passing all the columns in a dataset to the vector columnn

    columnn <- c(i)

    for(var in columnn)
    {
      # # Check whether the variable is categorical or not

      if(is.factor(data[, var]) | (length(unique(data[,var]))/nrow(data)*100 < 5))
      {
        print(paste(names(data)[var],'is a categorical variable'))

      }
      else if(is.numeric(data[, var]))
      {
        # Check whether the variable is numeric or not

        {

          # Defining Outliers upperbound and lowerbound

          Upperbound = min(max(data[,var]), (quantile(data[,var],0.75) + 1.5*IQR(data[,var])))
          Lowerbound = max(min(data[,var]), (quantile(data[,var],0.25) - 1.5*IQR(data[,var])))

          lowerout <- c()
          upperout <- c()

          # Storing upperbound and lowerbound for all variables in two different vectors respectively

          for(j in data[,var])
          {
            if(j < Lowerbound)
            {
              lowerout <- c(lowerout,j)
            }
          }

          for(k in data[,var])
          {
            if(k > Upperbound)
            {
              upperout <- c(upperout,k)
            }
          }


          variable_name <- variable_name[!is.na(variable_name)]
          lowerout <- lowerout[!is.na(lowerout)]
          upperout <- upperout[!is.na(upperout)]

          Lower_count <- Lower_count[!is.na(Lower_count)]
          Upper_count <- Upper_count[!is.na(Upper_count)]

          # Storing count of outliers in both bounds of corresponding variables

          variable_name[i] <- names(data[var])
          Lower_count[i] <- length(lowerout)
          Upper_count[i] <- length(upperout)

        }

      }

    }
  }

  # Creating a dataframe for the Outliers counts in both bounds

  df <- data.frame(variable_name, Lower_count, Upper_count)

  df <- df[-which(is.na(df$variable_name)),]

  rownames(df) <- 1:nrow(df)

  # Check whether Outlier is detected for each variable or not?

  for(x in 1:nrow(df))
  {
    if((df[x,2] == 0) & (df[x,3])== 0)
    {
      Outlier_detect <- c(Outlier_detect, "No outlier detected")
    }

    else

      Outlier_detect <- c(Outlier_detect, "Outlier detected")
  }

  # Storing Outlier detected or not for each variable

  df$Outlier_detection <- Outlier_detect

  df=data.frame(df)

  return(df)

}
