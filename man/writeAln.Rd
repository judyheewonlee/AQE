% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeAln.R
\name{writeAln}
\alias{writeAln}
\title{writeAln}
\usage{
writeAln(alnID, fileName = NULL, directory = "inst/extdata/Output",
  collapse = TRUE)
}
\arguments{
\item{alnID}{The Balibase alignment ID in the form of a character vector.
Note: Be sure to provide the correct cases as \code{writeAln} is
case sensitive.}

\item{fileName}{The name of the alignment file as a character vector.
Automatically set to NULL, where the file will be named as the
\code{alnID}.}

\item{directory}{The directory the user would like the file to be placed in.
Must be a character vector. Note: "/" should not be added to the end of the
directory.}

\item{collapse}{TRUE if the user would like to collapse the gaps in the
alignment. FALSE to maintain gaps. \code{collapse} is automatically set to
TRUE.}
}
\description{
The writeAln function returns an alignment from the
database generated from the Balibase suite.
}
\examples{
\dontrun{
alnID <- "BB11001"
writeAln(alnID)

writeAln(alnID, fileName = "aln", collapse = FALSE)
}

}
