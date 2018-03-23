#' fetchAln
#'
#' \code{<function>} Return an alignment of interest from BaliBASE.
#'
#' @section Input: The alignment ID of the alignment of interest from BaliBASE.
#' Additonal inputs are collapse and asMatrix which are automatically
#' set to FALSE if not provided.
#'
#' @param alnID A character vector of the alignment name.
#'
#' @param collapse TRUE if the user would like to remove gaps in the sequence.
#' FALSE if they would like to keep the gaps. \code{collapse} is automatically
#' set to TRUE.
#'
#' @param asMatrix TRUE if the user would like the sequence in matrix form.
#' FALSE will return a character vector of the sequence. \code{asMatrix} is
#' automatically set to FALSE.
#'
#' @return The alignment of interest from the BaliBASE database.
#' @export


fetchAln <- function(alnID, collapse = TRUE, asMatrix = TRUE) {
  referenceDB <- get("referenceDB", envir  = environment())
  # Check if the provided alnID is valid
  if (is.null(referenceDB$alignments[[alnID]])) {
    stop("No such alignment is available on baliBASE. Make sure that the cases are
        correct and quotations are used.\n")
  }

  else {
    aln <- referenceDB$alignments[[alnID]]
    seqList <- c()

    # Collapse the sequences and add them to seqList
    for (i in 1:nrow(aln)) {
      sequence <- paste(aln[i,], collapse = "")

      # Collapse the sequence if collapse is TRUE
      if (isTRUE(collapse)) {
        sequence <- gsub("-", "", sequence)
      }

      seqList <- append(seqList, sequence)
    }

    # Convert the sequences into a matrix format if asMatrix is
    # TRUE
    if (isTRUE(asMatrix)) {
      maxSeqLength <- max(nchar(seqList))
      modAln <- matrix(nrow = nrow(aln), ncol = maxSeqLength)

      for (j in 1:length(seqList)) {
        splitSeq <- strsplit(seqList[j], "")[[1]]
        length(splitSeq) <- maxSeqLength

        modAln[j,] <- splitSeq
      }
    }

    # Use seqList and create a matrix of the sequences without them separated if
    # asMatrix is FALSE
    else {
      modAln <- c()
      for (sequence in seqList) {
        modAln <- rbind(modAln, sequence)
      }
    }

    # Change the rownames to the corresponding sequence ID's
    rownames(modAln) <- rownames(aln)

  }

  return(modAln)
}


