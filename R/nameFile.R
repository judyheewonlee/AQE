#' nameFile
#'
#' \code{<function>} The \code{nameFile} function generates a name for a
#' file. If the passed fileName is NULL, then concatenate the
#' \code{inputID} and \code{extension.} If not, then concatenate the
#' passed \code{fileName} and \code{extension} and return
#' the final name of the file.
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
#'
#' @export
#' @keywords internal

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


