#' createDB.R
#'
#' \code{<function>} Produce a datamodel containing the contents from the
#' BaliBASE database. Refer to the data model to understand the structure
#' of the reference data base.
#'
#' Details.
#'
#' @return A data model containing the alignments and sequences of
#' the BaliBASE database.
#'
#' @export
#'
#' @importFrom jsonlite fromJSON

createDB <- function() {
  # Define variables for the data model structure
  referenceDB <- list()
  referenceDB$alignments <- list()
  referenceDB$seqCategory <- data.frame(seqID = c(), alnID = c(), stringsAsFactors = FALSE)
  referenceDB$alnCategory <- data.frame(alnID = c(), refID = c(), stringsAsFactors = FALSE)

  # Access refCategory.json to create referenceDB$category
  refCat <- jsonlite::fromJSON("data/refCategory.json")
  referenceDB$category <- as.data.frame(refCat)

  # Call readFiles to fill the reference DB
  referenceDB <- readFiles(referenceDB)



  return (referenceDB)
}
