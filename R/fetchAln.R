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
#' @param collapse Let the user choose a file name, if none is provided make the name
#' of the file
#'

#think about how to collpase the matrix alignment and what data structure you should
# use to store it

fetchAln <- function(alnID, collapse = FALSE, asMatrix = TRUE) {

  if (is.null(referenceDB$alignments[[alnID]])) {
    cat("No such alignment is available on baliBASE. Make sure that the cases are
        correct and quotations are used.\n")
  }

  else{

    return(referenceDB$alignments[[alnID]])

  }
    # if (write == TRUE) {
    #   fileName = paste(ID, ".fasta", sep = "")
    #   write.fasta(referenceDB$alignments[[ID]]$seq,
    #               referenceDB$alignments[[ID]]$name,
    #               fileName)
    # }



}

#need to mkae sure that we're not overwriting files without the user knowing
#make sure to check if the file exists already (file.exists() function)
