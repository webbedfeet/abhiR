% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf2tiff.R
\name{pdf2tiff}
\alias{pdf2tiff}
\title{pdf2tiff}
\usage{
pdf2tiff(dirs, compress = TRUE, dpi = 300)
}
\arguments{
\item{dirs}{The directory within which to convert PDF files}

\item{compress}{TRUE/FALSE. Do we use LZW compression (default=TRUE)}

\item{dpi}{The DPI for the TIFF files (default=300)}
}
\value{

}
\description{
This function converts all PDF files in a directory into TIFF files, optionally with
a specified DPI (default is 300) and LZW compression. This function is meant to be used
in directories with graphs
}
\examples{
pdf2tiff('graphs', compress=T, dpi=600)

}
