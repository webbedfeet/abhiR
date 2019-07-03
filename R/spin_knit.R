#' spin_knit
#'
#' Start with a R file with "spin" annotation, and derive a rendered document using
#' rmarkdown::render rather than knit::knit2html
#'
#' @param f A R script file, ending in ".R"
#'
#' @return (Invisibly) a HTML document
#' @export
#'
#' @examples
spin_knit <- function(f){
  require(knitr)
  require(rmarkdown)
  require(stringr)
  spin(f, knit = F, format = 'Rmd')
  render(str_replace(f,'.R','.Rmd'))
}
