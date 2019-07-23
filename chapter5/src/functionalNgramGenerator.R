# functionalNgramGenerator.R
# Generate a list of the top character-level n-grams in a text. This
# information is roughly analogous to primitive sound.
#
# Assumes all markup/metadata has been removed.
#
# Usage: functionalNgramGenerator(<input text>, <size>, <number>)
#
# <input text> is the input text (pre-loaded into memory)
# <size> is the length of the n-gram
# <number> is the number of desired character-level n-grams
#
# Example: top ten bi-grams in the elegiacs of Catullus
# > source("functionalNgramGenerator.R")
# > rawLines <- readLines("../testing/catullus_elegiac")
# > functionalNgramGenerator(rawLines, 2, 10)
#
# Expected output:
# [1] "423 er 0.241026" "384 qu 1.000000" "328 is 0.168378" "318 re 0.285971"
# [5] "304 um 0.177778" "274 es 0.156125" "261 it 0.133984" "257 am 0.178596"
# [9] "257 tu 0.216330" "238 in 0.122177"
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

functionalNgramGenerator <- function(rawLines, ngramLength, numNgrams) {
   # remove the punctuation
   linesFiltered <- gsub("[[:punct:]]", "", rawLines)

   # produce a sorted list of all words
   wordsSorted <- tolower(sort(unlist(strsplit(linesFiltered, " "))))

   # collect n-grams in a master list
   ngrams <- vector()
   ngramCount <- 1

   for (i in 1:length(wordsSorted)) {
      wordChars <- unlist(strsplit(wordsSorted[i], ""))
   
      if (length(wordChars) > ngramLength - 1) {
         numIterations <- length(wordChars) - (ngramLength - 1)
         for (j in 1:numIterations) {
            str <- wordChars[j]
            for (k in 1:(ngramLength - 1)) {
               str <- paste(str, wordChars[j+k], sep="")
            }

            ngrams[ngramCount] <- str 
            ngramCount <- ngramCount + 1
         }
      }
   }

   ngramsSorted <- sort(ngrams)

   # generate a frequency count for each unique n-gram in the content array
   ngramCounts <- data.frame(table(ngramsSorted))
   ngramCountsSorted <- ngramCounts[with(ngramCounts, order(-Freq)),]

   # generate a relative probability for the top k n-grams
   results <- vector()
   for (i in 1:numNgrams) {
      ngram <- lapply(ngramCountsSorted[i,1], as.character)
      ngramChars <- unlist(strsplit(ngram[[1]], ""))
      searchExpr <- paste("^", ngramChars[1], sep="")
      relatedNgrams <- ngramCountsSorted[grep(searchExpr, ngramCountsSorted$ngramsSorted),]

      firstLetterCount <- 0
      for (j in 1:nrow(relatedNgrams)) {
         firstLetterCount <- firstLetterCount + relatedNgrams[j,2]
      }

      prob <- ngramCountsSorted[i,2] / firstLetterCount
      results[i] <- sprintf("%d %s %f", ngramCountsSorted[i,2], ngram[[1]], prob)
   }

   return(results)
}
