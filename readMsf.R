# ReadMsf.R

# import the seqinr package to read msf files
library(seqinr)

# Initialize the reference database. For details on how it is structured, refer to the data model

referenceDB <- list()
alignment <- list()
sequence <- list()
index <- 0

# Define category descriptions and add them into referenceDB$category
r11 <- "equi-distant sequences with <20% identity"
r12 <- "equi-distant sequences with 20-40% identity"
r2 <- "families aligned with a highly divergent orphan sequence"
r3 <- "subgroups with <25% residue identity between groups"
r4 <- "sequences with N/C-terminal extensions"
r5 <- "internal insertions"

referenceDB$category <- data.frame(refID = c("RV11", "RV12", "RV20", "RV30", "RV40", "RV50"),
                                   description = c(r11, r12, r2, r3, r4, r5),
                                   stringsAsFactors = FALSE)

referenceDB$alnCategory <- data.frame(ID = c(), refCategory = c(), stringsAsFactors = FALSE)

directories <- c("RV11", "RV12", "RV20", "RV30", "RV40", "RV50")

for (dir in directories) {
    directory <- paste("data/bb3_release/", paste(dir, "/", sep = ""), sep = "")
    fileList <- list.files(directory, pattern = ".msf")

    # Add an alignment to the list alignment and add the alignment and it's category to
    # referenceSB$alnCategory data frame Add the sequences in each alignment msf file
    # separatly to sequences
    for (ref in fileList) {
        filePath <- paste(directory, ref, sep = "")
        readaln <- read.alignment(filePath, "msf")

        alnID <- gsub(".msf", "", ref)

        # Add the alignment by adding the sequences and their correspoding names
        # to the alignment list
        alignment[[alnID]] <- list(name = readaln$nam, seq = readaln$seq)

        # Add the sequences to the corresponding sequence name to the sequence list
        for (seqName in alignment[[alnID]]$name) {
            index <- index + 1
            sequence[[tolower(seqName)]] <- alignment[[alnID]]$seq[[1]]
        }
        index <- 0

        # Add the category information of the alignment
        referenceDB$alnCategory <- rbind(referenceDB$alnCategory,
                                         data.frame(ID = alnID, refCategory = dir,
                                                    stringsAsFactors = FALSE))

    }
}

referenceDB$alignments <- alignment
referenceDB$sequences <- sequence

# Save referenceDB into a .rda file for less computational costs
save(referenceDB, file = "balibaseReferenceDB.rda")


##--------------------------- Sample Searches -------------------------------##

# ##Sample category search ##Print every alignment in a given category
# targetCat <- 'RV11'
# list <- referenceDB$alnCategory[referenceDB$alnCategory$refCategory == targetCat , 'ID']
#
# for (i in list) {
#   print(referenceDB$alignments[[i]])
# }

# ##Sample sequence search by sequence name
# targetSeq <-'FER_HAEIN'
# referenceDB$sequences[[targetSeq]]

# ##Sample alignment search by alignment ID
# targetAln <- 'BB30014'
# print(referenceDB$alignments[[targetAln]])
