% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f.read.image.R
\name{f.read.image}
\alias{f.read.image}
\title{Function to read image files}
\usage{
f.read.image(source, height = NULL, Normalize = F, ls.format = F)
}
\arguments{
\item{source}{the path of the file}

\item{height}{redimension the file}

\item{Normalize}{should the data be normalized, i.e. force the values on each channel between 0 and 1}

\item{ls.format}{boolean to keep the pictures in list format}
}
\description{
Function to read image files
}
\examples{
data <- f.read.image('www/rTLC_demopicture.JPG',height=256)
data \%>\% raster()
}
\author{
Dimitri Fichou
}
