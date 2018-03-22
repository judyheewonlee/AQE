#' checkFileExist.R
#'
#' \code{<function>} The @checkFileExist function checks to see if
#' the filename requested is already existing in the directory. If
#' it is, prompt the user if they would like to overwrite. If no,
#' then stop the running functions.
#'
#' Details.
#'
#' @param file The filepath of the file of interest in the form
#' of a character vector.
#'
#' @export
#'
#' @keywords internal

checkFileExist <- function(file) {
  # If the file name exists already, prompt the user if they would like to replace the file
  if (file.exists(file)) {
    answer <- readline(prompt =
                         "There exists a file with the same name.
                          Would you like to replace it? (Y/N)\n")

    if (identical(answer, "N") || identical(answer, "n")) {
      stop("Please resolve the file naming.\n")
    }

    else {
      cat("Overwriting existing file... \n\n")
    }
  }
}
