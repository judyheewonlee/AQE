# makefile path


readFiles <- function(database) {

  directories <- c("RV11", "RV12", "RV20", "RV30", "RV40", "RV50")

  for (dir in directories) {
    directory <- paste("data/bb3_release/", paste(dir, "/", sep = ""), sep = "")
    fileList <- list.files(directory, pattern = ".msf")

    for (ref in fileList) {
      filePath <- paste(directory, ref, sep = "")
      readaln <- read.alignment(filePath, "msf")

      #add if condition here
      alnID <- gsub(".msf", "", ref)

      database <- appendMatrix(database, readaln, alnID)
      database <- appendSeqCat(database, readaln, alnID)
      database <- appendAlnCat(database, alnID, dir)

    }
  }

  return (database)
}
