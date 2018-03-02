#writeSeq.R still in progress

writeSeq <- function(seqName, fileName, collapse = TRUE) {

  if (is.null(referenceDB$sequenceCategory[[seqName]])) {
    cat("No such sequence is available on baliBASE. Make sure that the cases are
        correct and quotations are used. \n")
  }

  else {
    if (is.null(fileName)) {
      seqFile <- paste(seqName, ".fasta", sep = "")
    }
    else {
      seqFile <- paste(fileName, ".fasta", sep = "")
    }

    if (!file.exists(seqFile)) {
      write.fasta(fetchSeq(seqName, collapse, TRUE), seqName,
                  seqFile)
    }
    else {
      answer <- readline(prompt = "There exists a file with the same name. Would you like to replace it? (Y/N)\n")

      if (identical(answer, "Y")) {
        write.fasta(fetchSeq(seqName, collapse, TRUE), seqName,
                      seqFile)
      }
    }
  }
}

