#' writeAln
#'
#' \code{<function>} The writeAln function returns an alignment from the
#' database generated from the Balibase suite.
#'
#' @param alnID The Balibase alignment ID in the form of a character vector.
#' Note: Be sure to provide the correct cases as \code{writeAln} is
#' case sensitive.
#'
#' @param fileName The name of the alignment file as a character vector.
#' Automatically set to NULL, where the file will be named as the
#' \code{alnID}.
#'
#' @param directory The directory the user would like the file to be placed in.
#' Must be a character vector. Note: "/" should not be added to the end of the
#' directory.
#'
#' @param collapse TRUE if the user would like to collapse the gaps in the
#' alignment. FALSE to maintain gaps. \code{collapse} is automatically set to
#' TRUE.
#'
#' @export
#' @import seqinr

writeAln <- function(alnID, fileName = NULL, directory = "data/Output",
                     collapse = TRUE) {
  referenceDB <- get("referenceDB", envir  = environment())
  if (is.null(referenceDB$alignments[[alnID]])) {
    cat("No such sequence is available on baliBASE. Make sure that the cases are
        correct and quotations are used. \n")
  }

  else {
    #Create a name for the file, use fileName if provided
    if (is.null(fileName)) {
      alnFile <- paste(alnID, ".mfa", sep = "")
    }

    else {
      alnFile <- paste(fileName, ".mfa", sep = "")
    }

    # Place output files into data/Output directory in project folder
    directory <- paste(directory, "/", alnFile, sep = "")

    # Call checkFileExist to see if the file already exists
    checkFileExist(directory)

    # Fetch the alignment and write it into a mfa file
    alignment <- fetchAln(alnID, collapse, asMatrix = FALSE)
    seqinr::write.fasta(as.list(alignment), rownames(alignment),
                directory)
  }
}
