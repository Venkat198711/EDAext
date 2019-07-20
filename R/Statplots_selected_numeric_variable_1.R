
#' @title Statistical plots for user interest variables
#'
#' @description Plot box plot and histogram for user input numeric variables
#'
#' @param data variable (vector of user interest variable index) exportfileto (current directory)
#'
#' @return NULL
#'
#' @examples  Statplots_selected_numeric_variable_1 ("Boston", variable = c(1,2,3), exporfileto = "Directory name")
#'
#' @export Statplots_selected_numeric_variable_1

Statplots_selected_numeric_variable_1 <- function(data, variable = c(), exportfileto = " ")

{
  # Create a new folder, if not exists, else hit error as folder already exists

  ifelse(!dir.exists(exportfileto), dir.create(exportfileto), FALSE)

  setwd(exportfileto)

  for(var in variable)
  {

    # Check whether the variable is categorical or not

    if(is.factor(data[,var]) | (length(unique(data[,var]))/nrow(data)*100<5))
    {
      print(paste(names(data)[var],' is categorical variable'))
    }

    # Check whether the variable is numeric or not

    else if(is.numeric(data[,var]))
    {
      png(filename = paste(paste(names(data)[var], ".png", sep="")))
      par(mfrow=c(1,2))

      # Plot the boxplot for numeric variables.

      boxplot(data[,var], main = paste("Boxplot of", names(data)[var]),
              ylab = names(data)[var], col = "maroon", border = "grey5",
              horizontal = T)

      # Plot the histogram for numeric variables

      hist(data[,var], main = paste("Histogram of", names(data)[var]),
           xlab = names(data)[var], ylab = paste("No. of", names(data)[var]), col = "lightgreen", border=F)

      # Export the png file to the folder which user passed in the function

      dev.off()
    }
  }

}
