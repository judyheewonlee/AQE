#' writeClustalOmega
#'
#' \code{<function>} Write a multifasta file containing sequences that are
#' aligned by the ClustalOmega alignment algorithm. The user may search the
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
#' @param fileName The name of the ClustalOmega alignment file. Automatically
#' set to NULL, where the file will be named as the concatenated string of
#' the \code{alnID} and "ClustalOmega".
#'
#' @param directory The directory the user would like the file to be placed in.
#' Must be a string. Note: "/" should not be added to the end of the directory.
#'
#' @param cluster The cluster size which should be used. The default is 100.
#' In the original ClustalOmega implementation, this parameter is called
#' cluster-size.
#'
#' @param gapOpening,gapExtension ClustalOmega currently does not allow
#' to adjust gap penalties; these arguments are only for future extensions
#' and consistency with the other algorithms and msa. However, setting these
#' parameters to values other than "default" will result in a warning.
#'
#' @param maxiters maximum number of iterations; the default value
#' is 0 (no limitation). In the original ClustalOmega implementation,
#' this parameter is called \code{iterations}.
#'
#' @param substitutionMatrix substitution matrix for scoring matches and
#' mismatches; can be a real matrix or a file name. If the file interface
#' is used, matrices have to be in NCBI-format. The original MUSCLE
#' implementation also accepts matrices in WU_BLAST (AB_BLAST) format, but,
#' due to copyright restrictions, this format is not supported by
#' \code{msaMuscle}.
#'
#' @param type type of the input sequences \code{inputSeqs}; see \code{msa()}
#' from \code{msa} package.
#'
#' @param order how the sequences should be ordered in the output object (see msa);
#' in the original ClustalW implementation, this parameter is called
#' \code{output-order}.
#'
#' @param verbose if TRUE, the algorithm displays detailed information
#' and progress messages.
#'
#' @param help if TRUE, information about algorithm-specific parameters is
#' displayed. In this case, no multiple sequence alignment is performed and
#' the function quits after displaying the additional help information.
#'
#' @param ... further parameters specific to ClustalOmega; An overview of parameters
#' that are available in this interface is shown when calling \code{msaClustalOmega}
#' with \code{help=TRUE}. For more details, see also the documentation of ClustalOmega.
#'
#' Please view details on \code{\link[msa]{msa}}
#' \code{\link[msa]{msaClustalOmega}} function which provides details
#' about the parameters that adjust the alignment.
#'
#' @examples
#' \dontrun{
#' writeClustalOmega("BB11001", readType = "AA")
#'
#' writeAln("BB11002")
#' writeClustalOmega("inst/extdata/Output/BB11002.mfa",
#' readType = "AA", gapExtension = 2)
#' }
#'
#' @export
#' @import seqinr
#' @importFrom utils tail

writeClustalOmega <- function(inputSeqs, readType = c("AA", "DNA", "RNA"),
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
    extension <- "ClustalOmega.mfa"
  }

  else {
    extension <- ".mfa"
  }

  alnFilePath <- nameFile(alnID, fileName, extension, directory)
  checkFileExist(alnFilePath)

  # Convert the alignment into seqinr alignment and write is as a msf file
  seqinrAln <- fetchClustalOmega(inputSeqs, readType, cluster, gapOpening,
                           gapExtension, maxiters, substitutionMatrix,
                           type, order, verbose, help, ...)
  seqinr::write.fasta(as.list(seqinrAln$seq), seqinrAln$nam,
                      alnFilePath)
}









