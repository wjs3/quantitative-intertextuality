# genSamples.R
# Generate text samples for LSI processing that have been stripped of punctuation
# and stop words.
#
# Note: the last sample will be padded with a sufficient amount of preceding text
# to reach the target number of words. Thus, there will be some overlap when matching
# at the end of the text.
#
# Usage genSamples(<raw text file>, <stop list>, <output directory>, <number of words>)
# 
# <raw text file> is the full path of the raw text file to be processed
# <stop list> is the path to the stop list text
# <output diretory> is the name of a directory where the output texts should be written to
# <number of words> defines how many words should be contained in each sample
#
# Example: Produce 100 line samples from the Book of Genesis
# > source("genSamples.R")
# > dir.create("../data/bible_samples_100_words")
# > genSamples("../data/bible/book_001.txt", "../data/stoplist.csv", "../data/bible_samples_100_words", 100)
# > length(list.files("../data/bible_samples_100_words"))
# [1] 141

genSamples <- function(rawTextFilePath, stopListFilePath, outputDir, numWords) {

   rawTextFile <- readLines(rawTextFilePath)
   stopListFile <- readLines(stopListFilePath) 

   # strip punctuation
   strippedLinesSmartApost <- gsub('[\u2019]', "\'", rawTextFile)
   strippedLinesDash <- gsub('[\u2014]', " ", strippedLinesSmartApost) 
   strippedLines <- gsub("[^[:alnum:][:space:]']", "", strippedLinesDash)

   # convert text to lowercase
   textLower <- tolower(strippedLines)

   sampleCount <- 1
   wordCount <- 1
   wordList <- vector()
   wordBuffer <- vector()

   for (i in 1:length(textLower)) {

      currStr <- strsplit(textLower[i], " ")

      if (length(currStr[[1]]) != 0) {
         for (j in 1:length(currStr[[1]])) {

            # check the stoplist
            if ((currStr[[1]][j] != "") && !(currStr[[1]][j] %in% stopListFile)) {
               wordList[wordCount] <- currStr[[1]][j]
               wordCount <- wordCount + 1 

               if (wordCount == numWords + 1) {

                  if (sampleCount < 10) {
                     sampleID <- paste('00', sampleCount, sep='')
                  }
                  else if (sampleCount < 100) {
                     sampleID <- paste('0', sampleCount, sep='') 
                  }
                  else {
                     sampleID <- sampleCount
                  }

                  outFile <- paste(outputDir, '/', strsplit(strsplit(rawTextFilePath, "/")[[1]][4],
                                   ".txt")[[1]], '_', sampleID, '.txt', sep='')
                  outStr <- paste(wordList, collapse=' ')
                  fileConn<-file(outFile)
                  writeLines(outStr, fileConn, sep='')
                  close(fileConn)
  
                  sampleCount <- sampleCount + 1
                  wordCount <- 1
                  wordBuffer <- wordList
                  wordList <- vector()
               }
            }
         }
      }
   }

   if (wordCount != 1) {
      missing <- numWords - wordCount + 1

      for (i in 1:missing) {
         wordList <- c(wordBuffer[numWords + 1 - i], wordList)    
      }

      if (sampleCount < 10) {
         sampleID <- paste('00', sampleCount, sep='')
      }
         else if (sampleCount < 100) {
         sampleID <- paste('0', sampleCount, sep='')
      }
      else {
         sampleID <- sampleCount
      }

      outFile <- paste(outputDir, '/', strsplit(strsplit(rawTextFilePath, "/")[[1]][4],
                       ".txt")[[1]], '_', sampleID, '.txt', sep='')
      outStr <- paste(wordList, collapse=' ')
      fileConn<-file(outFile)
      writeLines(outStr, fileConn, sep='')
      close(fileConn)
   }
}
