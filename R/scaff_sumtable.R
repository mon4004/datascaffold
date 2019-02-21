#' Creating a Table of Summary Statistics
#'
#' This function takes three inputs: a dataframe, a grouping variable x, and an outcome y, and returns a table of summary statistics grouped by the x variable. The table contains commonly-requested parametric and nonparametric summary statistics, and the results can be passed to a formatting command (such as kable or flextable).
#'
#' @name scaff_sumtable
#' @param data dataframe passed into function
#' @param x Categorical grouping variable
#' @param y Outcome of Interest
#' @examples
#' scaff_sumtable(data = iris, x = Sepal.Length, y = Species)
#' @import dplyr tibble
#' @export
scaff_sumtable <- function(data, x, y) {
  #Returning x and y as quosureS to be passed into linear model formula object
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)

  #Checking whether x variable is a factor - function not intended for analysis of non-categorical predictors
  if(!is.factor(data %>%
                pull(!!x)))
  {
    stop("Predictor variable must be categorical / ordinal!")
  }

  #Creating summary statistics table
  summaryTbl <-
    data %>%
    dplyr::group_by(!!x) %>%
    dplyr::summarize(n = length(!!x), "N Complete" =(length(!!y) - sum(is.na(!!y))),
              "Mean (SD)" = paste0(mean(!!y, na.rm = T) %>%
                                     round(2), " (", sd(!!y, na.rm = T) %>%
                                     round(2), ")"),
              "Median (IQR)" = paste0(median(!!y, na.rm = T),
                                      " (", quantile(!!y, 0.25, na.rm = T) %>% round(2), " - ",
                                      quantile(!!y, 0.75, na.rm = T) %>% round(2), ")")
    )

  #Counting levels of factor variable for determine whether to use t-test or ANOVA / F-test
  levelsCount <- data %>%
    dplyr::summarize(n_distinct(!!x)) %>%
    as.numeric()

  #creating LM with quosure created at beginning of function
  lmFormula <- paste(y, x, sep = " ~ ")
  lmFormula <- lmFormula[2]
  outLM <- lm(lmFormula, data = data)

  #Conducting parametric and nonparametric ANOVA to output p-values to summary table
  outAOV <- anova(outLM)
  outKruskal <- kruskal.test(lmFormula %>%
                               as.formula(), data = data)

  #Extracting p-values from ANOVA and KW test to add to table
  anovaP <- c(outAOV$`Pr(>F)`[1] %>% scaff_pvnot,
              rep(" ", times = levelsCount - 1))
  names(anovaP) <- "ANOVA.P"
  kwP <- c(outKruskal$p.value %>% scaff_pvnot,
           rep (" ", times = levelsCount - 1))
  names(kwP) <- "KruskalWallis.P"

  summaryTbl <- summaryTbl %>%
    tibble::add_column(., anovaP, .after = "Mean (SD)") %>%
    tibble::add_column(., kwP, .after = "Median (IQR)") %>%
    dplyr::rename("ANOVA.P" = anovaP, "KruskalWallis.P" = kwP)

  return(summaryTbl)
}
