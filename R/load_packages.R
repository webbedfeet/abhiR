#' Extracting package names from strings
#'
#' Options exist for Bioconductor and GitHub
#'
#' @param pkg Input string containing package name
#'
#' @return A string representing the package name
extract_names <- function(pkg){
  if(stringr::str_detect(pkg, 'BioC')){
    pkg <- stringr::str_extract(pkg, '^\\w+')
  }
  if(stringr::str_detect(pkg, '/')){ # Github
    pkg <- stringr::str_extract(pkg, '\\w+$')
  }
  return(pkg)
}




#' Loading project-required packages in R
#'
#' This function reads a file "pkgs.yml" in the `lib` directory of the project
#' and loads them in R, installing them if necessary
#'
#' Non-CRAN packages can be accessed via GitHub by specifying `user/package` format
#' Particular versions of packages can be ensured by specifying `package => version` format
#'
#' @return
#' @export
#'
#' @examples
load_packages <- function(homedir = here::here()) {
  require(pacman)
  `%notin%` <- Negate('%in%')
  rversion <- paste(version$major, version$minor, sep='.')
  if(!file.exists(here::here('lib','pkgs.yml'))){
    add_package('abhiR')
  }
  pkgs <- yaml::yaml.load_file(here::here('lib','pkgs.yml'))

  pkg_names <- purrr::map_chr(pkgs, extract_names)
  pkgs1 <- pkg_names[!purrr::map_lgl(pkg_names, pacman::p_isinstalled)] # What's not installed
  if(all(stringr::str_detect(pkgs, '=>'))){# Version information included
    out = do.call(rbind,stringr::str_split(pkgs,'=>'))
    pkgs <- out[,1]
    versions <- out[,2]
  } else {versions <- NULL}
  # Installing missing packages
  for(p in pkgs1){
    if (stringr::str_detect(p, 'bioC')){ # Bioconductor packages
      pkgname <- extract_name(p)
      p_install(pkgname, try.bioconductor = TRUE)
      # if(rversion < '3.5'){
      #   source('http://bioconductor.org/biocLite.R')
      #   biocLite(pkgname)
      # } else {
      #   BiocManager::install(pkgname)
      # }
    } else if (stringr::str_detect(p, '/')){ # GitHub packages
      pkgname <- extract_names(p)
      p_install_gh(p)
      # devtools::install_github(p)
    } else { # CRAN packages
      if(!is.null(versions)){
        devtools::install_version(p, version = out[out[,1]==p,2], repos = 'http://cran.rstudio.com')
      } else{
        p_install(p)
        # utils::install.packages(p, repos='http://cran.rstudio.com')
      }
    }
  }
  p_load(char = pkgs)
}
