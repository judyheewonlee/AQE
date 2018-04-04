#' scoreAlignment
#'
#' @description
#' The \code{scoreAlignment} function takes a reference
#' alignment and a test alignment and returns an object of class "pairwise
#' alignment comparison" (PAC) containing the sum of pairs and/or total
#' column scores using the AlignStat package if \code{details} is set to TRUE.
#' If the user would simply like the scores, \code{scoreAlignment} returns a
#' data frame containing the total column score and sum of pairs score.
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
#' @param details A boolean value, FALSE if the user would like
#' only the score values rather than a PAC object containing details about
#' the alignments. If TRUE, a PAC is returned.
#'
#' @return A PAC object containing providing the optimal pairwise column
#' alignment of two alternative MSAs of the same sequences, and summary
#' statistics of the differences between them. (Refer to the documentation
#' for \code{\link[AlignStat]{compare_alignments}} function for details on
#' the output components in the AlignStat package.) Or a data frame
#' containing only the total column score and sum of pairs score,
#' depending on the value of \code{details}.
#'
#' @examples
#' \dontrun{
#' writeClustalW("BB11001", readType = "AA", fileName = "testAln")
#' writeAln("BB11001", fileName = "refAln")
#'
#' scoreAlignment("inst/extdata/Output/refAln.mfa",
#' "inst/extdata/Output/testAln.mfa",
#' SP = TRUE, CS = TRUE, details = FALSE)
#'
#' scoreAlignment("inst/extdata/Output/refAln.mfa",
#' "inst/extdata/Output/testAln.mfa",
#' SP = FALSE, CS = TRUE, details = TRUE)
#' }
#'
#' @export
#' @importFrom AlignStat compare_alignments

scoreAlignment <- function(reference, test, SP = FALSE, CS = FALSE,
                           details = FALSE) {
  if (!file.exists(reference) || !file.exists(test)) {
    stop("Please make sure to provide valid filepaths.")
  }

  score <- AlignStat::compare_alignments(reference, test, SP, CS)

  if (!isTRUE(details)) {
    if (isTRUE(SP) && isTRUE(CS)) {
      score <- data.frame(Type = c("Total Column Score",
                                   "Sum of Pairs Score"),
                          Score = c(score$column_score$column.score,
                                    score$sum_of_pairs$sum.of.pairs.score))
    }

    else if (isTRUE(SP)) {
      score <- data.frame(Type = c("Sum of Pairs Score"),
                          Score = c(score$sum_of_pairs$sum.of.pairs.score))
    }

    else {
      score <- data.frame(Type = c("Total Column Score"),
                          Score = c(score$column_score$column.score))
    }

  }

  return(score)
}
