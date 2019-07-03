#' myformat_pvalue
#'
#' This function ensures that all the p-values are formatted with the right number of
#' characters. Below the `digits` threshold, they are presented in scientific notation,
#' otherwise they are rounded to `digits`
#'
#' @param x A vector of p-values
#' @param digits Number of digits to present in the output
#'
#' @return A character vector of formatted p-values
#' @export
#'
#' @examples
myformat_pvalue <- function(x, digits = 3){
  require(scales)
  return(
    ifelse(x < 10^(-digits), scientific(x, digits = digits),
           round(x, digits = digits))
  )
}
