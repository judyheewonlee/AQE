#' fetchSeq.R
#'
#' \code{<function>} Return a sequence of interest from BaliBASE collpased
#' or with gaps and/or in matrix format.
#'
#' Details.
#' @section Input: The name of the sequence of interest.
#'
#' @param seq A character vector of the sequence name in quotations.
#'
#' @param collapse TRUE if the user would like to remove gaps in the sequence.
#' FALSE if they would like to keep the gaps. @collapse is automatically
#' set to TRUE.
#'
#' @param asMatrix TRUE if the user would like the sequence in matrix form.
#' FALSE will return a character vector of the sequence. @asMatrix is
#' automatically set to FALSE.
#'
#' @return The sequence of interest from the BaliBASE database.
#'
#' @export

fetchSeq <- function(seqName, collapse = TRUE, asMatrix = FALSE) {
  # Grab all positions in the seqCategory dataframe where
  # the sequence ID is positioned
  seqNum <- grep(seqName, referenceDB$seqCategory$seqID)

  # Check if the user provided a valid seqID
  if (is.null(seqNum)) {
    cat("No such sequence is available on baliBASE. Make sure that the cases are
        correct and quotations are used. \n")
  }

  else {
    alnID <- referenceDB$seqCategory[seqNum[1],]$alnID
    seq <- paste(referenceDB$alignments[[alnID]][seqName,], collapse = "")

    if (isTRUE(collapse)) {
      seq <- gsub("-", "", seq)
    }

    if(isTRUE(asMatrix)) {
      seq <- strsplit(seq, "")
    }

    return (seq)
  }
}
