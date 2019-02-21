#' Creating a Boxplot by Group
#'
#' This allows you to create a plot where there is a boxplot for each category.
#' @name scaff_boxplot
#' @param data dataset to use for plot.
#' @param x Categorical Predictor.
#' @param y Outcome of Interest.
#' @param xname (Optional) Name of Categorical Predictor you would like displayed in the plot. If not specified, the name of data$x is used instead.
#' @param xyname (Optional) Name of Outcome of Interest you would like displayed in the plot. If not specified, the name of data$y is used instead.
#' @examples
#' scaff_boxplot(data = iris,x = Species ,y = Sepal.Length, xname = "Iris Species", yname = "Sepal Length")
#' @import dplyr ggplot2 rlang
#' @export
scaff_boxplot <- function(data, x, y, xname = NULL, yname = NULL) {
#Returning x and y as quosures to be passed into linear model formula object
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)

#Checking if user provided xname and yname, if unspecified the function returns the names of data$x and data$y

  if (is.null(xname))
    {
    xname <- rlang::quo_text(x)
  }
  if (is.null(yname)){
    yname <- rlang::quo_text(y)
  }


  #Use ggplot function to create and customize plot
  summary_plot <- ggplot2::ggplot(data, aes(
    x = !!x,
    y = !!y,
    fill = !!x
  )) +
    geom_boxplot() +
    labs(title = paste("Boxplot of",
                       xname,
                       "by", yname), x = xname, y = yname) +
    theme(plot.title = element_text(hjust = 0.5)) +
    #Remove legend from graph to reduce redundancy
    guides(fill = FALSE)
  return(summary_plot)
}

