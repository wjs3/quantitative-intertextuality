# genFeaturesBatch.R
# Generate a batch of feature vectors from a formatted input file:
# "sample 1","sample 2",document1,document2
# This function is a wrapper around genFeatures.R

genFeaturesBatch <- function(inFile, outFile, corpus, tag, language="en") {
   library("gtools")
   source("genFeatures.R")

   samples <- readLines(inFile)
   vectors <- c()

   for (i in 1:length(samples)) {
      temp1 <- unlist(strsplit(samples[i], "\""))
      temp2 <- unlist(strsplit(temp1[5], ","))
      print(temp1)
      print(temp2)
      sample1 <- temp1[2]
      sample2 <- temp1[4]
      document1 <- readRDS(temp2[2])
      document2 <- readRDS(temp2[3])

      vectors[i] <- genFeatures(sample1, sample2, document1, document2, corpus, tag, lang=language)
   }

   write(vectors, outFile)
}
