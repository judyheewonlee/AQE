% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchClustalW.R
\name{fetchClustalW}
\alias{fetchClustalW}
\title{fetchClustalW}
\usage{
fetchClustalW(inputSeqs, readType = c("AA", "DNA", "RNA"),
  cluster = "default", gapOpening = "default", gapExtension = "default",
  maxiters = "default", substitutionMatrix = "default", type = "default",
  order = c("aligned", "input"), verbose = FALSE, help = FALSE, ...)
}
\arguments{
\item{inputSeqs}{The sequences the user would like to align. Can either
be an alignment ID of a Balibase alignment or a file path to a .mfa file
containing sequences for an alignment.}

\item{readType}{The type of characters found in the provided input
sequences. Can either be "AA", "DNA" or "RNA" depending on the characters
in the file.}

\item{cluster}{The clustering method which should be used. Possible values
are \code{"nj"} (default) and \code{"upgma"}. In the original ClustalW
implementation, this parameter is called clustering.}

\item{gapOpening}{gap opening penalty; the default value for nucleotide
sequences is 15.0, the default value for amino acid sequences is 10.0.}

\item{gapExtension}{gap extension penalty; the default value for
nucleotide sequences is 6.66, the default value for amino acid
sequences is 0.2.}

\item{maxiters}{maximum number of iterations; the default value
is 16. In the original ClustalW implementation, this parameter
is called \code{numiters}.}

\item{substitutionMatrix}{substitution matrix for scoring matches and
mismatches; can be a real matrix, a file name, or the name of a built-in
substitution matrix. In the latter case, the choices \code{"blosum"},
\code{"pam"}, \code{"gonnet"}, and \code{"id"} are supported
for amino acid sequences. For aligning nucleotide sequences,
the choices \code{"iub"} and \code{"clustalw"} are possible.
The parameter dnamatrix can also be used instead for the sake of backwards
compatibility. The valid choices for this parameter are \code{"iub"} and
\code{"clustalw"}. In the original ClustalW implementation, this parameter
is called \code{matrix}.}

\item{type}{type of the input sequences \code{inputSeqs}; see \code{msa()}
from \code{msa} package.}

\item{order}{how the sequences should be ordered in the output object
(see msa); in the original ClustalW implementation, this parameter is
called \code{outorder}.}

\item{verbose}{if TRUE, the algorithm displays detailed information
and progress messages.}

\item{help}{if TRUE, information about algorithm-specific parameters is
displayed. In this case, no multiple sequence alignment is performed and
the function quits after displaying the additional help information.}

\item{...}{further parameters specific to ClustalW; An overview of parameters
that are available in this interface is shown when calling \code{msaClustalW}
with \code{help=TRUE}. For more details, see also the documentation of ClustalW.

Please view details on \code{\link[msa]{msa}} and
\code{\link[msa]{msaClustalW}} function which provides details
about the parameters that adjust the alignment.}
}
\value{
The alignment generated by ClustalW in the form of a seqinr
alignment.
}
\description{
Return a seqinr alignment containing sequences that are
aligned by the ClustalW alignment algorithm. The user may search the
referenceDB for a set of BaliBASE sequences by entering the alignment ID
as input, or they may input a .mfa file containing the sequences in
fasta format.
}
\examples{
\dontrun{
clustalw <- fetchClustalW("BB11001", readType "AA")

writeAln("BB11002")
aln <- fetchClustalW("inst/extdata/Output/BB11002.mfa",
readType = "AA", gapExtension = 2)
}

}
