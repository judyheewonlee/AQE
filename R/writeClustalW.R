#' writeClustalW
#'
#' @description
#' Write a multifasta file containing sequences that are
#' aligned by the ClustalW alignment algorithm. The user may search the
#' referenceDB for a set of BaliBASE sequences by entering the alignment ID
#' as input, or they may input a .mfa file containing the sequences in
#' fasta format.
#'
#' @param inputSeqs The sequences the user would like to align. Can either
#' be an alignment ID of a BaliBASE alignment or a file path to a .mfa file
#' containing sequences for an alignment.
#'
#' @param readType The type of characters found in the provided input
#' sequences. Can either be "AA", "DNA" or "RNA" depending on the characters
#' in the file.
#'
#' @param fileName The name of the ClustalW alignment file as a character
#' vector. Automatically set to NULL, where the file will be named as the
#' concatenated string of the \code{alnID} and "ClustalW".
#'
#' @param directory The directory the user would like the file to be placed in.
#' Must be a string. Note: "/" should not be added to the end of the directory.
#'
#' @param cluster The clustering method which should be used. Possible values
#' are \code{"nj"} (default) and \code{"upgma"}. In the original ClustalW
#' implementation, this parameter is called clustering.
#'
#' @param gapOpening gap opening penalty; the default value for nucleotide
#' sequences is 15.0, the default value for amino acid sequences is 10.0.
#'
#' @param gapExtension gap extension penalty; the default value for
#' nucleotide sequences is 6.66, the default value for amino acid
#' sequences is 0.2.
#'
#' @param maxiters maximum number of iterations; the default value
#' is 16. In the original ClustalW implementation, this parameter
#' is called \code{numiters}.
#'
#' @param substitutionMatrix substitution matrix for scoring matches and
#' mismatches; can be a real matrix, a file name, or the name of a built-in
#' substitution matrix. In the latter case, the choices \code{"blosum"},
#' \code{"pam"}, \code{"gonnet"}, and \code{"id"} are supported
#' for amino acid sequences. For aligning nucleotide sequences,
#' the choices \code{"iub"} and \code{"clustalw"} are possible.
#' The parameter dnamatrix can also be used instead for the sake of backwards
#' compatibility. The valid choices for this parameter are \code{"iub"} and
#' \code{"clustalw"}. In the original ClustalW implementation, this parameter
#' is called \code{matrix}.
#'
#' @param type type of the input sequences \code{inputSeqs}; see \code{msa()}
#' from \code{msa} package.
#'
#' @param order how the sequences should be ordered in the output object
#' (see msa); in the original ClustalW implementation, this parameter is
#' called \code{outorder}.
#'
#' @param verbose if TRUE, the algorithm displays detailed information
#' and progress messages.
#'
#' @param help if TRUE, information about algorithm-specific parameters is
#' displayed. In this case, no multiple sequence alignment is performed and
#' the function quits after displaying the additional help information.
#'
#' @param ... further parameters specific to ClustalW; An overview of parameters
#' that are available in this interface is shown when calling \code{msaClustalW}
#' with \code{help=TRUE}. For more details, see also the documentation of
#' ClustalW.
#'
#' @seealso
#' Please view details on \code{\link[msa]{msa}} and
#' \code{\link[msa]{msaClustalW}} function which provides details about
#' the parameters that adjust the alignment.
#'
#' @examples
#' \dontrun{
#' writeClustalW("BB11001", readType = "AA")
#'
#' writeAln("BB11002")
#' writeClustalW("data/Output/BB11002.mfa",
#'                         readType = "AA", gapExtension = 2)
#' }
#'
#' @export
#' @import seqinr
#' @importFrom utils tail

writeClustalW <- function(inputSeqs, readType = c("AA", "DNA", "RNA"),
                        fileName = NULL, directory = "inst/extdata/Output",
                        cluster = "default", gapOpening = "default",
                        gapExtension = "default", maxiters = "default",
                        substitutionMatrix = "default", type = "default",
                        order = c("aligned", "input"), verbose = FALSE,
                        help = FALSE, ...) {
  referenceDB <- get("referenceDB", envir  = environment())
  aln <- referenceDB$alignments[[inputSeqs]]

  if (file.exists(inputSeqs)) {
    # Create file name for later use
    splitDirectory <- strsplit(inputSeqs, "/")[[1]]
    alnID <- gsub("\\..*", "", tail(splitDirectory, n = 1))
  }

  else if (!is.null(aln)) {
    alnID <- inputSeqs
  }

  # If the inputSeqs the user provided is invalid
  else {
    stop("The InputSeqs you provided is invalid.\n
         If you are providing an alignment ID, make sure it
         exists in BaliBASE. If you are providing a file,
         make sure the filepath is correct. \n")
  }

  if (is.null(fileName)) {
    extension <- "ClustalW.mfa"
  }

  else {
    extension <- ".mfa"
  }

  alnFilePath <- nameFile(alnID, fileName, extension, directory)
  checkFileExist(alnFilePath)

  # Convert the alignment into seqinr alignment and write is as a msf file
  seqinrAln <- fetchClustalW(inputSeqs, readType, cluster, gapOpening,
                           gapExtension, maxiters, substitutionMatrix,
                           type, order, verbose, help, ...)
  seqinr::write.fasta(as.list(seqinrAln$seq), seqinrAln$nam,
                      alnFilePath)
}











