% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deconstruct.R
\name{deconstruct}
\alias{deconstruct}
\title{function to deconstruct an array,}
\usage{
deconstruct(data, margin, transform, conv_width = 1)
}
\arguments{
\item{data}{a data frame or a matrix}

\item{margin}{the margin used for deconstruction}

\item{transform}{was transform used for deconstruction}
}
\description{
function to deconstruct an array,
}
\examples{
data <- f.read.image(source='www/rTLC_demopicture.JPG',height=128)
decon <- data \%>\% deconstruct(margin =3 ,transform = F)
}
\author{
Dimitri Fichou
}
