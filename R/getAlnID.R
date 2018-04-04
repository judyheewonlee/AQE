#' getAlnID
#'
#' @description
#' Retrieve alignment ID's in a corresponding reference
#' category of BaliBASE or retrieve the alignment ID's corresponding
#' to a specific sequence ID.
#'
#' @param referenceCat A reference category in the form of a character
#' vector. \code{getAlnID} will return all alignment ID's that are
#' contained in that reference category. Note: "RV10", "RV12",
#' "RV20", "RV30", "RV40", and "RV50" are the valid reference
#' categories.
#'
#' @param seqID A sequence ID in the form of a character vector.
#' \code{getAlnID} will return the alignment ID's containing the
#' specified sequence.
#'
#' @return Either a list of alnID's that are contained in the
#' specified reference category or a list of alnID's that contain
#' the specified sequence ID's.
#'
#' @examples
#' \dontrun{
#' alnIds <- getAlnID(seqID = "1aab_")
#' (alnIds)
#'
#' alnIds <- getAlnID(referenceCat = "RV50")
#' (alnIds)
#' }
#'
#' @export

getAlnID <- function(referenceCat = NULL, seqID = NULL) {
  referenceDB <- get("referenceDB", envir  = environment())

  # Return the alignment ID's from the given reference Category
  if (!is.null(referenceCat)) {
    numList <- grep(referenceCat, referenceDB$alnCategory$refID)
    alnList <- lapply(numList,
                      function(x) referenceDB$alnCategory[x,]$alnID)
  }

  # Return the alignment ID's that contain te provided seqID
  else if (!is.null(seqID)) {
    numList <- grep(seqID, referenceDB$seqCategory$seqID)
    alnList <- lapply(numList, function(x) referenceDB$seqCategory[x,]$alnID)
  }

  return (alnList)
}
