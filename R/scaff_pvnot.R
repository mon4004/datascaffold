#' Rounding P-Values
#'
#' This function allows you to print any value that is less than 0.001 as '<0.001' instead of using long scientific notation.
#' @name scaff_pvnot
#' @param pvalue Value that need to be changed to appropriate notation.
#' @examples
#' pvnot(9.8E-9)
#' @export
scaff_pvnot <- function(pvalue) {
  if(pvalue < 0.001) p.txt <- "<0.001"
  else if(pvalue < 0.01) p.txt <- format(pvalue, digits=1)
  else p.txt <- format(pvalue, digits=2)
  return(p.txt)
}


