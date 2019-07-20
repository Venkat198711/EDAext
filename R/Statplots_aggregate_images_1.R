
#' @title png files aggregation
#'
#' @description Moving all the png files from one folder to another folder
#'
#' @param data variable (user interest variable index) exportfile newfilepath
#'
#' @return NULL
#'
#' @examples  Statplots_aggregate_images_1("Boston", variable = c(1,2,3), pathfrom, pathto)
#'
#' @export Statplots_aggregate_images_1


Statplots_aggregate_images_1 <- function(data, variable = c(), exportfileto = " ", newfilepath = '')

{

  # Create a new folder, if not exists, else hit error as folder already exists

  ifelse(!dir.exists(exportfileto), dir.create(exportfileto), FALSE)

  setwd(exportfileto)

  # create a new directory to move png from existing folder to new folder to aggregate all png files

  dir.create(newfilepath)

  {
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

      # Move all the png files from existing folder to newfilepath which user passed in the function

      a = list.files(pattern = ".png")

      file.copy(a, newfilepath)

    }

  }

}
