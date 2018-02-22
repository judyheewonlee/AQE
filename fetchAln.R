#' fetchAln.R
#'
#' \code{<function>} Print or save an alignment of interest from the BaliBASE suite
#'
#' Details.
#' @section Input: The alignment ID of an alignment of interest with TRUE or FALSE if the user
#' would like to save the alignment in a .fasta format file.
#'
#' @param seq A string of the alignment ID.
#' @param write A boolean value of TRUE or FALSE if the user would like to have the
#' alignment in a .fasta format file. This parameter is automatically set to FALSE.
#'
#' @return The sequence of interest or also a .fasta file of the alignment if desired.

fetchAln <- function(alnID, write = FALSE) {
  ID <- toupper(alnID)
  if (is.null(referenceDB$alignments[[ID]])) {
    cat("No such alignment is available on baliBASE.\n")
  }
  else {
    print(referenceDB$alignments[[ID]])
    if (write == TRUE) {
      fileName = paste(ID, ".fasta", sep = "")
      write.fasta(referenceDB$alignments[[ID]]$seq,
                  referenceDB$alignments[[ID]]$name,
                  fileName)
    }
  }
}
