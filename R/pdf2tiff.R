#' pdf2tiff
#'
#' This function converts all PDF files in a directory into TIFF files, optionally with
#' a specified DPI (default is 300) and LZW compression. This function is meant to be used
#' in directories with graphs
#'
#' @param dirs The directory within which to convert PDF files
#' @param compress TRUE/FALSE. Do we use LZW compression (default=TRUE)
#' @param dpi The DPI for the TIFF files (default=300)
#'
#' @return
#' @export
#'
#' @examples
#' pdf2tiff('graphs', compress=T, dpi=600)
#'
pdf2tiff <- function(dirs, compress = TRUE, dpi = 300){
  require(stringr)
  require(glue)
  gsfn <- ifelse(Sys.info()['sysname']=='Windows','gswin32c','gs')
  cmd <- paste(c(gsfn, '-q','-dNOPAUSE','-dBATCH','-sDEVICE=tiff24nc'), collapse = ' ')
  pdffiles <- dir(dirs, pattern = 'pdf$')
  curdir = getwd()
  setwd(dirs)
  for(f in pdffiles){
    cmd2 <- as.character(glue(cmd,
                              ' -r{dpi}x{dpi}',
                              ifelse(compress, ' -sCompression=lzw',''),
                              ' -sOutputFile={str_replace(f,"pdf","tif")} {f}'))
    print(cmd2)
    system(cmd2, wait = FALSE)
  }
  setwd(curdir)
}
