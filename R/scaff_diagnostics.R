#' Creating a Table of Summary Statistics
#'
#' This function takes an input of a linear object model and uses ggplot to output typical model diagnostic plots.
#'
#' @name scaff_diagnostics
#' @param LMinput Object created by linear model fitting function to be passed into diagnostic plots
#' @param globaloutput An optional logical indicating whether the ggplot objects should be output to global environment as a list
#' @examples
#' lmOut <- lm(Sepal.Length ~ Species, data = iris)
#' scaff_diagnostics(lmOut)
#' @import dplyr ggplot2 gridExtra broom
#' @export
scaff_diagnostics <- function(LMinput, globaloutput = FALSE) {
  .fitted <- .resid <- .stdresid <- .std.resid <- NULL
  df <- broom::augment(LMinput)


  #plot of fitted values vs residuals, with labels updating based on variable names in LM passed to function
  p1 <- ggplot2::ggplot(data = LMinput, aes(x = .fitted, y = .resid))+
    geom_point(na.rm = T) +
    stat_smooth(method = "auto", se = FALSE, color = "red", alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(x = paste0("Fitted values of ", LMinput$terms[[2]]), y = "Residuals", title = paste0("Residuals vs. Fitted Values for ", LMinput$terms[[2]], " ~ ", LMinput$terms[[3]])) +
    theme_classic()


  #Q-Q plot
  p2 <- ggplot2::ggplot(data = LMinput, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
    geom_point(na.rm = T) +
    geom_qq_line(aes(sample = .stdresid), linetype = 2) +
    labs(x = "Theoretical Quantiles", y = "Standardized Residuals", title = paste0("Normal Q-Q Plot of ", LMinput$terms[[2]], " ~ ", LMinput$terms[[3]])) +
    theme_classic()

  #scale-location (fitted on x, standardized resids on y) - using broom::augment to pull data for

  p3 <- ggplot2::ggplot(data = df, aes(x = .fitted, y = sqrt(abs(.std.resid))))+
    geom_point()+
    stat_smooth(method = "loess", se = FALSE) +
    labs(x = paste0("Fitted values of", LMinput$terms[[2]]), y = "Sqrt(Standardized Residuals)",
         title = "Scale-Location Plot") +
    theme_classic()

  #constant leverage - stdresid on y, factor level on x
  p4 <- ggplot2::ggplot(data = df, aes(x = df %>% pull(2), y = .std.resid)) +
    geom_point()+
    stat_smooth(method = "loess", se = FALSE) +
    labs(x = "Factor-Level Combinations", y = "Standardized residuals", title = "Residuals vs. Leverage") +
    #geom_point(aes(size = .cooksd)) +
    theme_classic()

  pltGrid <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  if (globaloutput == TRUE)
    {
    scaffDiagnosticPlots <- list(p1, p2, p3, p4)
    return(pltGrid)
  }
  else
    {
  return(pltGrid)
    }
}


