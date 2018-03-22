#' scoreAlignment.R
#'
#' \code{<function>} The @scoreAlignment function takes a reference alignment
#' and a test alignment and returns an object of class "pairwise alignment
#' comparison" (PAC) containing the sum of pairs and/or total column scores
#' using the AlignStat package.
#'
#' Details.
#'
#' @param reference The filepath as a character vector to the reference
#' alignment.
#'
#' @param test The pilepath as a character vector to the alignment that will
#' be compared with the reference alignment.
#'
#' @param SP A boolean value, TRUE if the user would like the sum of pairs
#' score included into the returned PAC object. Automatically set to
#' FALSE.
#'
#' @param CS A boolean value, TRUE if the user would like the total column
#' score included into the returned PAC object. Automatically set to
#' FALSE.
#'
#' @return A PAC object containing providing the optimal pairwise column
#' alignment of two alternative MSAs of the same sequences, and summary
#' statistics of the differences between them. Refer to the documentation
#' for @compare_alignments function for details on the output components
#' in the AlignStat package.

scoreAlignment <- function(reference, test, SP = FALSE, CS = FALSE) {
  if (!file.exists(reference) || !file.exists(test)) {
    stop("Please make sure to provide valid filepaths.")
  }

  score <- AlignStat::compare_alignments(reference, test, SP, CS)

  return(score)
}
