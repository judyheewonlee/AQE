#' getSeqID
#'
#' @description
#' Retrieve sequence ID's that are contained in the
#' alignment ID provided
#'
#' @param alnID An alignment ID in the form of a character vector.
#'
#' @return A list of sequence ID's are containedin the specified
#' alignment ID.
#'
#' @examples
#' \dontrun{
#' seqIds <- getSeqID("BB11001")
#' (seqIds)
#' }
#'
#' @export

getSeqID <- function(alnID) {
  referenceDB <- get("referenceDB", envir  = environment())
  numList <- grep(alnID, referenceDB$seqCategory$alnID)
  seqList <- lapply(numList, function(x) referenceDB$seqCategory[x,]$seqID)

  return (seqList)
}
