#' writeMuscle
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
#' @param fileName The name of the Muscle alignment file as a character vector.
#' Automatically set to NULL, where the file will be named as the concatenated
#' string of the \code{alnID} and "Muscle".
#'
#' @param directory The directory the user would like the file to be placed in.
#' Must be a string. Note: "/" should not be added to the end of the directory.
#'
#' @param cluster The clustering method which should be used. Possible values
#' are \code{"upgma"}, \code{"upgmamax"}, \code{"upgmamin"}, \code{"upgmb"},
#' and \code{"neighborjoining"}.
#'
#' @param gapOpening gap opening penalty; the default is 400 for DNA
#' sequences and 420 for RNA sequences. The default for amino acid sequences
#' depends on the profile score settings: for the setting le=TRUE, the default
#' is 2.9, for sp=TRUE, the default is 1,439, and for sv=TRUE, the default is
#' 300. Note that these defaults may not be suitable if custom substitution
#' matrices are being used. In such a case, a sensible choice of gap
#' penalties that fits well to the substitution matrix must be made.
#'
#' @param gapExtension gap extension penalty; the default is 0.
#'
#' @param maxiters maximum number of iterations; the default is 16.
#' In the original MUSCLE implementation, it is also possible to set
#' \code{maxiters} to 0 which leads to an (out of memory) error. Therefore,
#' \code{maxiters=0} is not allowed in \code{msaMuscle}.
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
#' @param order how the sequences should be ordered in the output object
#' (see msa for more details); the original MUSCLE implementation does
#' not allow for preserving the order of input sequences. The \code{msaMuscle}
#' function realizes this functionality by reverse matching of sequence names.
#' Therefore, the sequences need to have unique names. If the sequences do
#' not have names or if the names are not unique, the msaMuscle function
#' assignes generic unique names \code{"Seq1"-Seqn} to the sequences and
#' issues a warning. The choice \code{"input"} is not available at all for
#' sequence data that is read directly from a FASTA file.
#'
#' @param verbose if TRUE, the algorithm displays detailed information
#' and progress messages.
#'
#' @param help if TRUE, information about algorithm-specific parameters is
#' displayed. In this case, no multiple sequence alignment is performed and
#' the function quits after displaying the additional help information.
#'
#' @param ... further parameters specific to MUSCLE; An overview of parameters
#' that are available in this interface is shown when calling \code{msaMuscle}
#' with \code{help=TRUE}. For more details, see also the documentation of MUSCLE.
#'
#' Please view details on \code{msa::msa()} function which provides details
#' about the parameters that adjust the alignment.
#'
#' @export
#' @import seqinr
#' @importFrom utils tail

writeMuscle <- function(inputSeqs, readType = c("AA", "DNA", "RNA"),
                              fileName = NULL, directory = "data/Output",
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
    extension <- "Muscle.mfa"
  }

  else {
    extension <- ".mfa"
  }

  alnFilePath <- nameFile(alnID, fileName, extension, directory)
  checkFileExist(alnFilePath)

  # Convert the alignment into seqinr alignment and write is as a msf file
  seqinrAln <- fetchMuscle(inputSeqs, readType, cluster, gapOpening,
                           gapExtension, maxiters, substitutionMatrix,
                           type, order, verbose, help, ...)
  seqinr::write.fasta(as.list(seqinrAln$seq), seqinrAln$nam,
                      alnFilePath)
}











