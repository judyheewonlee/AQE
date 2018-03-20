#' fetchAln.R
#'
#' \code{<function>} Return an alignment of interest from BaliBASE in matrix form.
#'
#' Details.
#' @section Input: The alignment ID of the alignment of interest.
#'
#' @param seq A string of the sequence name in quotations.
#'
#' @return The alignment of interest from the BaliBASE database.
#'

fetchAln <- function(alnID, collapse = FALSE, asMatrix = TRUE) {

  if (is.null(referenceDB$alignments[[alnID]])) {
    cat("No such alignment is available on baliBASE. Make sure that the cases are
        correct and quotations are used.\n")
  }

  else{
    aln <- referenceDB$alignments[[alnID]]
    modAln <- referenceDB$alignments[[alnID]]

    if (isTRUE(collapse)) {
      modAln <- matrix(nrow = nrow(aln))

      for (i in nrow(aln)) {

        seq <- paste(aln[i,], collapse = "")
        seq <- gsub("-", "", seq)
        modAln <- rbind(modAln, seq)

        # if (isTRUE(asMatrix)) {
        #   modAln <- rbind(modAln, strsplit(seq, "")[[1]])
        #
        # }
        #
        # else {
        #   modAln <- rbind(modAln, seq)
        #
        # }
      }

      #rownames(modAln) <- rownames(aln)

    }

    return(modAln)

  }

}

