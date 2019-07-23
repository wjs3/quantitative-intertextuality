# longworkStemBatchEn.R
# Translate a batch of documents into a stem list.
# Just a wrapper around

longworkStemBatchEn <- function(docs, outDir) {
   library("SnowballC")
   source("longworkStemEn.R")

   for (i in 1:length(docs)) {
      j <- length(unlist(strsplit(docs[i], "/")))
      outFile <- paste(outDir, unlist(strsplit(docs[i], "/"))[j], sep="")
      outFile <- paste(unlist(strsplit(outFile, ".txt"))[1], ".rds", sep="")
      longworkStemEn(docs[i], outFile)   
   }
}
