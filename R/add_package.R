#' Add a package to the packages.R file
#'
#' Packages can be in Github format ('user/packagename') as well
#' as pure 'packagename'. If bioC=TRUE, all the packages must be from Bioconductor
#'
#' @param pkgs A character vector of packages
#' @param homedir Where are these being included
#' @param bioC Whether packages are from Bioconductor
#'
#' @return
#' @export
#'
#' @examples
#' add_package(c('tidyverse','broom','stringr'))
add_package <- function(pkgs, homedir = '.', bioC = FALSE){
  if (bioC){
    pkgs <- paste(pkgs,'bioC',sep='-')
  }
  if(file.exists(file.path(homedir,'lib','pkgs.yml'))){
    existing_pkgs <- yaml::yaml.load_file(file.path(homedir,'lib','pkgs.yml'))
    pkgs <- unique(c(pkgs, existing_pkgs))
    write_packages(pkgs, homedir)
  } else {
    write_packages(pkgs, homedir)
  }
  load_packages()
}
