#' Creating Table of Multiple Comparisons for General Linear Regression
#'
#' This function allows you to create a dataframe with computation of multiple comparisons for general linear regression, which
#' can then be output to a table using different styles for publication or scientific reports. The function
#' conducts multiple comparison of a factor variable to a continious variable using general linear regression.
#' It will generate a table with the estimates for the multiple comparison between factor groups, the confidence intervals,
#' and the p-value of that parameter, with the possibility of using adjusting p-value methods.
#' @name scaff_comparisons
#' @param x Categorical Predictor
#' @param y Outcome of Interest
#' @param weights an optional vector of weights to be used in the fitting process.
#' Should be NULL or a numeric vector. If non-NULL, weighted least squares is used
#' with weights weights; otherwise ordinary least squares is used.
#' Default = NULL.
#' @param matrix the matrix required for the multiple comparison
#' @param padj type of p-value adjustment method; example: "holm", "bonferroni", "Westfall", "Shaffer", etc.
#' Default = "holm"
#' @param conf.level level of confidence requiered to calculate; default = 0.95.
#' @examples
#' Creating two different matrices for the function
#' tukey <- contrMat(table(iris$Species),"Tukey")
#' i <- rbind("Setosa-Versicolor" = c(1,-1,0),"Setosa-Virginica" = c(1,0,-1),"Versicolor-Virginica" = c(0,1,-1))
#' scaff_comparison(iris$Species, iris$Sepal.Width, matrix = tukey, padj = "holm", conf.level = 0.95)
#' scaff_comparison(iris$Species, iris$Sepal.Width, matrix = i, padj = "bonferroni", conf.level = 0.90)
#' @export
scaff_comparison <-
  function(x, y, weight = NULL, matrix, padj = "holm", conf.level = 0.95) {
    # Converts x (categorical variable) to a factor so that it can be used in the following functions inside the function
    x <- factor(x)
    # Create the linear regression model, can be with weighted values. Saved as the variable out
    out <- lm(y ~ x, weights = weight)
    # Running the multiple comparison model with the matrix and no p-value adjusted saved as variable p
    p <- summary(multcomp::glht(out, linfct = multcomp::mcp(x = matrix)), test = multcomp::adjusted(type = "none"))
    # Running the multiple comparison model with the matrix and p-value adjusted using holm as default saved as variable padj
    pad <- summary(multcomp::glht(out, linfct = multcomp::mcp(x = matrix)), test = multcomp::adjusted(type = padj))
    p <- summary(multcomp::glht(out, linfct = multcomp::mcp(x = matrix)), test = adjusted(type = "none"))
    # Running the multiple comparison model with the matrix and p-value adjusted using holm as default saved as variable padj
    pad <- summary(multcomp::glht(out, linfct = multcomp::mcp(x = matrix)), test = adjusted(type = padj))
    # Getting the confidence interval for the estimate saved as ci
    ci <- confint(p, level = conf.level)
    # Creating data.frame summarizing important information for the final table
    out1 <- data.frame(
      "es" = round(c(p$test$coefficient[1:length(p$test$coefficient)]), 4),
      "ci" = paste0(round(c(ci$confint[, 2]), 3), ",",
                    round(c(ci$confint[, 3]), 3)),
      "p" = sapply(c(p$test$pvalues[1:length(p$test$pvalues)]),
                   scaff_pvnot),
      "padj" = sapply(c(pad$test$pvalues[1:length(pad$test$pvalues)]),
                      scaff_pvnot))
    # Naming columns
    names(out1) <- c("Estimate",
                     paste0(conf.level * 100, "% Conf.Int"),
                     "p-value",
                     padj)
    return(out1)
  }


