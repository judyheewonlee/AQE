#writeSeq.R still in progress

writeSeq <- function(seqName, fileName = NULL, collapse = TRUE) {

  refNum <- grep(seqName, referenceDB$seqCategory$seqID)

  if (is.null(refNum)) {
    cat("No such sequence is available on baliBASE. Make sure that the cases are
        correct and quotations are used. \n")
  }

  else {

    #Create a name for the file, use fileName if provided
    if (is.null(fileName)) {
      seqFile <- paste(seqName, ".fasta", sep = "")
    }
    else {
      seqFile <- paste(fileName, ".fasta", sep = "")
    }

    # If the file name exists already, prompt the user if they would like to replace the file
    if (!file.exists(seqFile)) {
      write.fasta(fetchSeq(seqName, collapse, TRUE), seqName,
                  seqFile)
    }
    else {
      answer <- readline(prompt =
                           "There exists a file with the same name. Would you like to replace it? (Y/N)\n")

      if (identical(answer, "Y")) {
        write.fasta(fetchSeq(seqName, collapse, TRUE), seqName,
                      seqFile)
      }
    }
  }
}

