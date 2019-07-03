#' iitle
#'
#' @param pkgs A character array of packages to initialize the project with
#' @param homedir The top-level directory of the project, defaults to the current directory
#'
#' @return
#'
#' @examples
write_packages <- function(pkgs = NULL, homedir = '.'){
  if(!is.null(pkgs)){
    pkg_yml <- yaml::as.yaml(pkgs)
    if (file.exists(file.path(homedir, 'lib','pkgs.yml'))){
    write(pkg_yml, file = file.path(homedir, 'lib','pkgs.yml'))
    } else {
      if(!dir.exists(file.path(homedir,'lib'))){
        dir.create(file.path(homedir,'lib'))
      }
      file.create(file.path(homedir,'lib','pkgs.yml'), recursive=T)
      write(pkg_yml, file = file.path(homedir, 'lib','pkgs.yml'))
    }
  }
}
