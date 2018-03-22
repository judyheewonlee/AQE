#' writeSeq.R
#'
#' \code{<function>} The writeSeq function returns a sequence from the
#' database generated from the Balibase suite.
#'
#' Details.
#'
#' @param seqID The Balibase seq ID in the form of a character vector.
#' Note: Be sure to provide the correct cases as @writeSeq is case sensitive.
#'
#' @param fileName The name of the alignment file as a character vector.
#' Automatically set to NULL, where the file will be named as the @seqID.
#'
#' @param directory The directory the user would like the file to be placed in.
#' Must be a character vector. Note: "/" should not be added to the end of the
#' directory.
#'
#' @param collapse TRUE if the user would like to collapse the gaps in the
#' alignment. FALSE to maintain gaps. @collapse is automatically set to
#' TRUE.
#'
#' @export
#'
#' @import seqinr

writeSeq <- function(seqID, fileName = NULL, directory = "data/Output", collapse = TRUE) {
  refNum <- grep(seqID, referenceDB$seqCategory$seqID)

  if (is.null(refNum)) {
    cat("No such sequence is available on baliBASE. Make sure that the cases are
        correct and quotations are used. \n")
  }

  else {
    #Create a name for the file, use fileName if provided
    if (is.null(fileName)) {
      seqFile <- paste(seqID, ".fasta", sep = "")
    }

    else {
      seqFile <- paste(fileName, ".fasta", sep = "")
    }

    directory <- paste(directory, "/", seqFile, sep = "")

    # Call checkFileExist to see if the file already exists
    checkFileExist(directory)
    seqinr::write.fasta(fetchSeq(seqID, collapse, asMatrix = TRUE), seqID,
                directory)
  }
}

