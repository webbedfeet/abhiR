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
load_packages <- function(homedir = '.') {
  `%notin%` <- Negate('%in%')
  if(!file.exists(file.path('lib','pkgs.yml'))){
    add_package('ProjTemplate')
  }
  pkgs <- yaml::yaml.load_file(file.path('lib','pkgs.yml'))
  if(all(stringr::str_detect(pkgs, '=>'))){# Version information included
    out = do.call(rbind,stringr::str_split(pkgs,'=>'))
    pkgs <- out[,1]
    versions <- out[,2]
  } else {versions <- NULL}
  for(p in pkgs){
    if (stringr::str_detect(p, 'bioC')){ # Bioconductor packages
      pkgname <- stringr::str_extract(p, '^[a-zA-Z0-9]+')
      if (pkgname %notin% installed.packages()[,1]){
        source('http://bioconductor.org/biocLite.R')
        biocLite(pkgname)
        suppressPackageStartupMessages(library(pkgname, character.only = TRUE))
      }
    } else if (stringr::str_detect(p, '/')){ # GitHub packages
      pkgname = stringr::str_split(p, '/')[[1]][[2]]
      if (pkgname %notin% installed.packages()[,1]){
        devtools::install_github(p)
      }
      suppressPackageStartupMessages(library(pkgname, character.only = TRUE))
    } else { # CRAN packages
      if(!(p %in% installed.packages()[,1])){
        if(!is.null(versions)){
          devtools::install_version(p, version = out[out[,1]==p,2], repos = 'http://cran.rstudio.com')
        } else{
          utils::install.packages(p, repos='http://cran.rstudio.com')
        }
      }
      suppressPackageStartupMessages(library(p, character.only = TRUE))
    }
  }
}
