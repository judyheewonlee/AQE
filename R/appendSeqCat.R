#' appendSeqCat.R
#'
#' \code{<function>} Append the sequence ID and it's corresponding alignment
#' ID to the provided database.
#'
#' Details.
#'
#' @param database A database that will have the alignment in matrix form
#' appended to it.
#'
#' @param readaln An object of class alignment produced by seqinr. It contains
#' the number of aligned sequences, a vector of strings containing the names
#' of the aligned sequences, a vector of strings containing the aligned
#' sequence, and a vector of strings containing commentaries for each sequence
#' or NA is there is no comments.
#'
#' @param alnID The alignment ID as a string.
#'
#' @return The database with the sequence ID and it's corresponding alignment
#' ID appended to the data frame seqCategory contained in the the database.

appendSeqCat <- function(database, readaln, alnID) {
  # Add the alignment ID for each given sequence
  for (seqID in readaln$nam) {
    database$seqCategory <- rbind(database$seqCategory,
                                  data.frame(seqID = seqID,
                                             alnID = alnID,
                                             stringsAsFactors = FALSE))
  }

  return (database)
}
