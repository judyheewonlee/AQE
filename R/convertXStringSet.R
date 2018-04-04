#' convertXStringSet
#'
#' @description
#' The convertXStringSet function converts a provided
#' .mfa file into a XStringSet corresponding to the \code{readType} provided.
#' If the provided readType is invalid, \code{convertXStringSet} will stop
#' all running functions and prompt the user to input a valid readType.
#'
#' Details.
#'
#' @param filePath The path to the file of sequences stored in a .mfa
#' file in the form of a character vector.
#'
#' @param readType The type of characters found in the provided input
#' sequences. Can either be "AA", "DNA" or "RNA" depending on the characters
#' in the file.
#'
#' @return A \code{XStringSet} object corresponding to the \code{readType}
#' input.
#'
#' @export
#' @keywords internal
#' @importFrom Biostrings readAAStringSet readDNAStringSet readRNAStringSet

convertXStringSet <- function(filePath,
                              readType = c("AA", "DNA", "RNA")) {
  # Check the read type that the user provided and
  # convert the provided file into a XStringSet accordingly
  if(readType == "AA") {
    seq <- Biostrings::readAAStringSet(filePath)
  }

  else if (readType == "DNA") {
    seq <- Biostrings::readDNAStringSet(filePath)
  }

  else if (readType == "RNA") {
    seq <- Biostrings::readRNAStringSet(filePath)
  }

  else {
    stop("The readType you provided must either be AA, DNA, or RNA. \n")
  }

  return (seq)
}
