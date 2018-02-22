
#' fetchSeq.R
#'
#' \code{<function>} Print or save a sequence of interest from the BaliBASE suite
#'
#' Details.
#' @section Input: The name of the sequence of interest with TRUE or FALSE if the user
#' would like to save the sequence in a .fasta format file.
#'
#' @param seq A string of the sequence name.
#' @param write A boolean value of TRUE or FALSE if the user would like to have the
#' sequence in a .fasta format file. This parameter is automatically set to FALSE.
#'
#' @return The sequence of interest or also a .fasta file of the sequence if desired.

fetchSeq <- function(seq, write = FALSE) {
  seqName <- tolower(seq)
  if (is.null(referenceDB$sequences[[seqName]])) {
    cat("No such sequence is available on baliBASE.\n")
  }
  else {
    cat(referenceDB$sequences[[seqName]])
    if (write == TRUE) {
      fileName = paste(seqName, ".fasta", sep = "")
      write.fasta(referenceDB$sequences[[seqName]], seqName, fileName)
    }
  }
}
