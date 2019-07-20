
#' @title Numeric and Categoric statistic plots
#'
#' @description Plot box plot and histogram for numeric variables & Pie chart and Barplot for categorical variables
#'
#' @param data columnn (user can give this parameter as it is) exportfileto
#'
#' @return NULL
#'
#' @examples  Statplot_all_numerics_cum_categorical("Boston", columnn, exportfileto = "New directory)
#'
#' @export Statplot_all_numerics_cum_categorical


Statplot_all_numerics_cum_categorical <- function(data, columnn, exportfileto = " ")

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
          counts <- table(data[var])

          slices <- c(counts)
          lbls <- c(counts)

          print(paste(names(data)[var],'is a categorical variable'))
          png(filename = paste(paste(names(data)[var], ".png", sep="")))
          par(mfrow = c(1,2))

          # Plot barplot

          barplot(counts, main = paste("Barplot of", names(data[var])), col = rainbow(length(slices)))

          # Plot pie chart

          pie(slices, labels = lbls, main = paste("Pie chart of", names(data[var])), col = rainbow(length(slices)))

          dev.off()
        }
        else if(is.numeric(data[, var]))
        {

          # Check whether the variable is categorical or not

          png(filename = paste(paste(names(data)[var], ".png", sep="")))
          par(mfrow=c(1,2))

          #Plot boxplot

          boxplot(data[,var], main = paste("Boxplot of", names(data)[var]),
                  ylab = names(data)[var], col = "maroon", border = "grey5",
                  horizontal = T)

          # Plot histogram

          hist(data[,var], main = paste("Histogram of", names(data)[var]),
               xlab = names(data)[var], ylab = paste("No. of", names(data)[var]), col = "lightgreen", border=F)
          dev.off()  #NOTE this step

        }

      }
    }
  }
}
