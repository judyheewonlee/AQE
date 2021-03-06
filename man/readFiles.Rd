% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFiles.R
\name{readFiles}
\alias{readFiles}
\title{readFiles}
\usage{
readFiles(database)
}
\arguments{
\item{database}{A database that will have the alignment ID and the corresponding
category appended to it.}
}
\value{
The modified database containing alignments, alignment categories
and sequence categories.
}
\description{
Find the .msf files contained in the BaliBASE suite and
read each alignment. Call helper functions which modify the database
provided. Return the modified database.
}
\keyword{internal}
