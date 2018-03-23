#' writeClustalW
#'
#' \code{<function>} Write a multifasta file containing sequences that are
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
#' For the remaining parameters:
#' Please view details on \code{msa::msa()} function which provides details
#' about the remaining parameters for writeClustalW.
#'
#' @export
#' @import seqinr

writeClustalW <- function(inputSeqs, readType = c("AA", "DNA", "RNA"),
                        fileName = NULL, directory = "data/Output",
                        cluster = "default", gapOpening = "default",
                        gapExtension = "default", maxiters = "default",
                        substitutionMatrix = "default", type = "default",
                        order = c("aligned", "input"), verbose = FALSE,
                        help = FALSE, ...) {
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











