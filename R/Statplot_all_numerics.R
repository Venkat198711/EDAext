
#' @title Statistical plots for all numeric variables
#'
#' @description Plot box plot and histogram for all numeric variables
#'
#' @param data columnn (user can give this parameter as it is) exportfile
#'
#' @return NULL
#'
#' @examples  Statplot_all_numerics ("Boston", columnn, exportfileto = "new directory")
#'
#' @export Statplot_all_numerics


Statplot_all_numerics <- function(data, columnn, exportfileto = " ")

{

  # Create a new folder, if not exists, else hit error as folder already exists

  ifelse(!dir.exists(exportfileto), dir.create(exportfileto), FALSE)

  setwd(exportfileto)

  {

    for(i in 1:ncol(data))
    {

      # Passing all the columns in a dataset to the vector columnn

      columnn <- c(i)

      for(var in columnn)
      {

        # Check whether the variable is categorical or not

        if(is.factor(data[, var]) | (length(unique(data[,var]))/nrow(data)*100 < 5))
        {
          print(paste(names(data)[var],'is categorical'))
        }

        # Check whether the variable is numeric or not

        else if(is.numeric(data[, var])  )
        {
          png(filename = paste(paste(names(data)[var], ".png", sep="")))
          par(mfrow=c(1,2))

          # Plot the boxplots

          boxplot(data[,var], main = paste("Boxplot of", names(data)[var]),
                  ylab = names(data)[var], col = "maroon", border = "grey5",
                  horizontal = T)

          # Plot the histogram

          hist(data[,var], main = paste("Histogram of", names(data)[var]),
               xlab = names(data)[var], ylab = paste("No. of", names(data)[var]), col = "lightgreen", border=F)

          # Export the png files to the folder which user passes into the function

          dev.off()  #NOTE this step

        }

      }
    }
  }
}
