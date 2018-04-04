#' fetchSeq
#'
#' @description
#' Return a sequence of interest from BaliBASE collpased
#' or with gaps and/or in matrix format.
#'
#' @section Input: The name of the sequence of interest as a character vector.
#'
#' @param seqName A character vector of the sequence name in quotations.
#'
#' @param collapse TRUE if the user would like to remove gaps in the sequence.
#' FALSE if they would like to keep the gaps. \code{collapse} is automatically
#' set to TRUE.
#'
#' @param asMatrix TRUE if the user would like the sequence in matrix form.
#' FALSE will return a character vector of the sequence. \code{asMatrix} is
#' automatically set to FALSE.
#'
#' @examples
#' \dontrun{
#' seqID <- "1aab_"
#'
#' seq <- fetchSeq(seqID, collapse = FALSE)
#' seq2 <- fetchSeq(seqID, asMatrix = FALSE)
#' }
#'
#' @return The sequence of interest from the BaliBASE database.
#' @export

fetchSeq <- function(seqName, collapse = TRUE, asMatrix = FALSE) {
  referenceDB <- get("referenceDB", envir  = environment())
  # Grab all positions in the seqCategory dataframe where
  # the sequence ID is positioned
  seqNum <- grep(seqName, referenceDB$seqCategory$seqID)

  # Check if the user provided a valid seqID
  if (is.null(seqNum)) {
    stop("No such sequence is available on baliBASE. Make sure that the cases are
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
