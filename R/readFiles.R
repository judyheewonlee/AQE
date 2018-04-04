#' readFiles
#'
#' @description
#' Find the .msf files contained in the BaliBASE suite and
#' read each alignment. Call helper functions which modify the database
#' provided. Return the modified database.
#'
#' @param database A database that will have the alignment ID and the corresponding
#' category appended to it.
#'
#' @return The modified database containing alignments, alignment categories
#' and sequence categories.
#'
#' @export
#'
#' @keywords internal
#'
#' @importFrom jsonlite fromJSON
#' @import seqinr

readFiles <- function(database) {
  directories <- jsonlite::fromJSON("inst/extdata/refCategory.json")[,1]

  # Iterate through each directory and each file in the directories
  for (dir in directories) {
    directory <- paste("inst/extdata/bb3_release/", paste(dir, "/", sep = ""), sep = "")
    fileList <- list.files(directory, pattern = ".msf")

    for (ref in fileList) {
      filePath <- paste(directory, ref, sep = "")
      readaln <- seqinr::read.alignment(filePath, "msf")
      alnID <- gsub(".msf", "", ref)

      # Modify database by calling helper functions
      database <- appendMatrix(database, readaln, alnID)
      database <- appendSeqCat(database, readaln, alnID)
      database <- appendAlnCat(database, alnID, dir)
    }
  }

  return (database)
}
