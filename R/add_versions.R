#' Add version information to project packages
#'
#' @return
add_versions <- function(pkgs) {
  out <- dplyr::select(
    dplyr::filter(as.data.frame(installed.packages()), Package %in% pkgs),
    Package, Version)
  out <- paste(out$Package, out$Version, collapse='=>')
  return(out)
}
