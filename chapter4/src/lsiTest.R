# lsiTest.R
# Return a list of cosine distances for a query text matched
# against a target text, using an LSI model as a projection
# basis.
#
# Assumes all markup/metadata has been removed.
#
# Usage: lsiTest(<query text>, <LSI model>, <score threshold>)
#
# <query text> is the path to the input text
# <LSI model> is the pre-trained LSI model, generated via lsiGenBoWTrain.R
# <score threshold> is a lower-bound on the cosine distances to be returned
#                   as valid matches
#
# Example: Find sections of text from the novel "A Silent Prayer" that may be a semantic
#          match to the beginning of Chapter 72 of the Qur'an.
# > source("lsiTest.R")
# > lsiTest("../data/quran_BoW/chapter_072_a_silent_prayer/chapter_072_1.bin", M, 0.9)
# [1] "chapter_16_04.txt 0.905549024673489"
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

lsiTest <- function(queryBoWFile, lsiModel, thresh) {
   library("lsa")

   load(queryBoWFile)
   docs <- as.textmatrix(lsiModel)
   query <- fold_in(matrix2, lsiModel)
   numTargets <- dim(docs)
  
   for (i in 1:numTargets[2]) {
      score <- cosine(query[,1], docs[,i])
      if (score[1,1] > thresh) {
         docName <- names(lsiModel$dk[i,1])
         scoreStr <- toString(score[1,1])
         print(paste(docName, scoreStr))
      }
   }
}
