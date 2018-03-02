#' createDB.R
#'
#' \code{<function>} Produce a datamodel containing the contents from the BaliBASE database.
#' Refer to the data model to understand the structure of the reference data base.
#'
#' Details.

#' @return A data model containing the alignments and sequences of the BaliBASE database.

createDB <- function() {

  # Define variables for the data model structure
  referenceDB <- list()
  referenceDB$alnCategory <- data.frame(alnID = c(), refID = c(), stringsAsFactors = FALSE)

  # Define category descriptions and add them into referenceDB$category
  # will convert this list of strings into a json to make it less redundant
  r11 <- "equi-distant sequences with <20% identity"
  r12 <- "equi-distant sequences with 20-40% identity"
  r2 <- "families aligned with a highly divergent orphan sequence"
  r3 <- "subgroups with <25% residue identity between groups"
  r4 <- "sequences with N/C-terminal extensions"
  r5 <- "internal insertions"

  referenceDB$category <- data.frame(refID = c("RV11", "RV12", "RV20", "RV30", "RV40", "RV50"),
                                     description = c(r11, r12, r2, r3, r4, r5),
                                     stringsAsFactors = FALSE)


  # Call readMsf() to return a vector of alignments in the form of matrices and as well the
  # sequence ID's and their corresponding alignment ID's.
  alnSeq <- readMsf(referenceDB)
  alignments <- alnSeq[[1]]
  sequenceCat <- alnSeq[[2]]

  referenceDB$alignments <- alignments
  referenceDB$sequenceCategory <- sequenceCat

  return (referenceDB)

}
