# lsiTrain.R
# Produce an LSI model from a training corpus.
#
# Assumes all markup/metadata has been removed.
#
# Usage: lsiTrain(<bag-of-words binary file>, <number of dimensions>)
#
# <bag-of-words binary file> bag-of-words binary representation of the training texts,
#                            generated via lsiGenBoWTrain.R
# <number of dimensions> desired number of dimensions for the model
#
# Example: Train an LSI model from the bag-of-words representation of the novel
#          "A Silent Prayer"
# > source("lsiTrain.R")
# > M <- lsiTrain("../data/a_silent_prayer_BoW/a_silent_prayer_BoW.bin", 20)
# > length(M)
# [1] 3
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

lsiTrain <- function(BoWFile, numDims) {
   library("lsa")

   # will have the TF-IDF table pre-computed for release
   load(BoWFile)
   space1 = lsa(matrix1, dims=numDims)

   return(space1)
}
