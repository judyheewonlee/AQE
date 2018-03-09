#' appendMatrix.R
#'
#' \code{<function>} Append the alignment to the database provided in the form of a matrix.
#' Rownames are set as the sequence names in the alignment. Each row contains the corresponding sequence
#' and each column is the individual characters of the sequences.
#'
#' Details.
#'
#' @param database A database that will have the alignment in matrix form appended to it.
#'
#' @param readaln An object of class alignment produced by seqinr. It contains the number
#' of aligned sequences, a vector of strings contianing the names of the aligned sequences,
#' a vector of strings containing the aligned sequence, and a vector of strings containing
#' commentaries for each sequence or NA is there is no comments.
#'
#' @param alnID The alignment ID as a string.
#'
#' @return The database with the alignment matrix appended to the alignments list contained in
#' the database.

appendMatrix <- function(database, readaln, alnID) {

  alnMatrix <- c()

  # Iterate through all the sequences in readaln sequences and generate a matrix
  # of that sequence
  for(seq in readaln$seq) {
    splitSeq <- strsplit(seq, "")[[1]]
    alnMatrix <- rbind(alnMatrix, splitSeq)
  }

  # Set rownames and add the matrix with it's corresponding alignment ID
  # to the database
  rownames(alnMatrix) <- readaln$nam
  database$alignments[[alnID]] <- alnMatrix

  return(database)

}
