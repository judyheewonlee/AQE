#' nameFile.R
#'
#' \code{<function>} The @nameFile function generates a name for a file. If the
#' passed fileName is NULL, then concatenate the @inputID and @extension.
#' If not, then concatenate the passed @fileName and @extension and return
#' the final name of the file.
#'
#' Details.
#'
#' @param inputID The name of the file as a string if fileName is NULL.
#' Automatically set to NULL.
#'
#' @param fileName The name of the file if the user provides a desired
#' fileName. Automatically set to NULL.
#'
#' @param extension The extension of the file provided by the user.
#'
#' @return The generated file name.

nameFile <- function(inputID = NULL, fileName, extension, directory) {
  #Create a name for the file, use fileName if provided
  if (is.null(fileName)) {
    filePath <- paste(directory, "/", inputID, extension, sep = "")
  }

  else {
    filePath <- paste(directory, "/", fileName, extension, sep = "")
  }

  return (filePath)
}


