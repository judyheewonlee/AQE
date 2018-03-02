#' readMsf.R
#'
#' \code{<function>} Return a vector of alignments read from the BaliBASE data base in the form
#' in matrix form. Each row represents a sequence from the alignment and each column
#' contains a single character from the sequence. The row names are set as the sequence ID's.
#' Additionally, return a list of sequence ID's and the alignment ID's they fall under.
#' readMsf will also populate the alnCategory data frame.
#'
#' Details.
#' @section Input: The reference database from createDB.R
#'
#' @param database The reference database from createDB.R
#'
#' @return A list containing a vector of alignments and a list of sequence ID's with their
#' corresponding alignment ID's.

readMsf <- function(database) {

  # Define data storing variables
  alignment <- list()
  sequenceCat <- list()

  # Iterate through the directories in the database
  directories <- c("RV11", "RV12", "RV20", "RV30", "RV40", "RV50")

  for (dir in directories) {
    directory <- paste("data/bb3_release/", paste(dir, "/", sep = ""), sep = "")
    fileList <- list.files(directory, pattern = ".msf")

    for (ref in fileList) {
      filePath <- paste(directory, ref, sep = "")
      readaln <- read.alignment(filePath, "msf")

      alnID <- gsub(".msf", "", ref)

      # Produce a matrix that contains the sequences in it's rows
      # and individual characters in its column for the sequence.
      # Row names are given as the sequence names.
      alnMatrix <- c()

      for(seq in readaln$seq) {
        splitSeq <- strsplit(seq, "")[[1]]
        alnMatrix <- rbind(alnMatrix, splitSeq)
      }

      rownames(alnMatrix) <- readaln$nam
      alignment[[alnID]] <- alnMatrix

      # Add the alignment ID for each given sequence
      for (seqID in readaln$nam) {
        sequenceCat[[seqID]] <- c(sequenceCat[[seqID]], alnID)

      }

      # Add the balibase category for the alignment
      database$alnCategory <- rbind(database$alnCategory,
                                       data.frame(alnID = alnID, refID = dir,
                                                  stringsAsFactors = FALSE))

    }
  }

  return (list(alignment, sequenceCat))
}


