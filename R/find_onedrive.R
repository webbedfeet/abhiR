#' Finding the OneDrive directory on your system
#'
#' If there are more than one folder with "OneDrive" in it, the
#' user is prompted for a choice
#'
#' @param home (default=NULL) The home directory under which the OneDrive folder exists. If it is NULL, the system HOME folder is used (`Sys.getenv("HOME")`)
#' @return The normalized path name of the OneDrive folder
#' @export
find_onedrive <- function(home=NULL){
  require(fs)
  if(is.null(home)){
    home <- normalizePath(Sys.getenv('HOME'), winslash = '/')
  }
  OD_dir <- dir_ls(home, glob = '*OneDrive*')
  if(length(OD_dir) == 0){
    return("No OneDrive folder found")
  }
  if(length(OD_dir) > 1){
    ind <- menu(OD_dir, title='OneDrive folder choices')
    OD_dir <- OD_dir[ind]
  }
  return(OD_dir)
}
