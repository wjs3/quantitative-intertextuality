# sampleText.R
# Split a text into m number of n line samples.
#
# Assumes all markup/metadata has been removed.
#
# Usage: sampleText(<input text>, <number>)
#
# <input text> is the input text (pre-loaded into memory)
# <number> desired number of lines per sample
#
# Example: produce a set of 20 line samples from the elegies of Catullus
# > source("sampleText.R")
# > X <- sampleText("../testing/catullus_elegiac", 20)
# > length(X)
# [1] 29
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

sampleText <- function(inputText, numLines) {
   # read the raw source file
   rawLines <- readLines(inputText)

   # split the text (ignores the end of a file if the final number of lines
   # doesn't meet the target)
   textSet <- list()
   currEntry <- 1
   currLine <- 1
   for (i in 1:length(rawLines)) {

      if (currLine <= numLines ) {
         if (currLine == 1) {
            textSet[currEntry][currLine] <- rawLines[i]
         }
         else {
            textSet[[currEntry]][currLine] <- rawLines[i]
         }
         currLine <- currLine + 1
      }
      else { 
         currLine <- 1
         currEntry <- currEntry + 1
      }
   }

   if (length(textSet[[length(textSet)]]) < numLines) {
      textSet[[length(textSet)]] <- NULL
   }

   return(textSet)
}
