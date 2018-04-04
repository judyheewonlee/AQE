#' appendAlnCat
#'
#' @description
#' Append the alignment ID with the corresponding reference
#' ID it belongs to into the provided database.
#'
#' @param database A database that will have the alignment ID and the
#' corresponding category appended to it.
#'
#' @param alnID The alignment ID as a string.
#'
#' @param directory The directory which alnID is contained in as a string.
#'
#' @return The database with the alignment category appended to the
#' alnCategory data frame contained in the database.
#'
#' @export
#' @keywords internal

appendAlnCat <- function(database, alnID, directory) {
  # Add the alignment ID and it's reference category to the database
  database$alnCategory <- rbind(database$alnCategory,
                                data.frame(alnID = alnID,
                                           refID = directory,
                                           stringsAsFactors = FALSE))

  return(database)
}
