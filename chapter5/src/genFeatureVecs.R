# genFeatureVecs.R
# Given output from functionalNgramGenerator.R, generate a formatted
# feature vector for the functional n-grams of interest.
#
# Assumes all markup/metadata has been removed.
#
# Usage: genFeatureVecs(<input features>, <n-gram list>)
#
# <input features> is the output of functionalNgramGenerator.R, collected into
#    a list
# <n-gram list> is a set (vector of strings) of functional n-grams to build the 
#    feature vector from. The code assumes that these features exist in the 
#    output of functionalNgramGenerator.R
#
# Example:
# > source("genFeatureVecs.R")
# > source("sampleText.R")
# > source("functionalNgramGenerator.R")
# > X <- sampleText("../testing/catullus_elegiac", 20)
# > X_f <- list()
# > X_f[[1]] <- functionalNgramGenerator(X[[1]], 2, 20)
# > genFeatureVecs(X_f, c("er", "re"))
#
# Expected Output:
#         [,1]     [,2]
# [1,] 0.222222 0.195122
# 
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

genFeatureVecs <- function(inputFeatures, ngramList) {

   featureVecs <- array(,dim=c(length(inputFeatures), length(ngramList)))

   # generate a vector from each text
   for (i in 1:length(inputFeatures)) {
      for (j in 1:length(ngramList)) {
         str <- inputFeatures[[i]][grep(ngramList[j], inputFeatures[[i]])]
         vals <- unlist(strsplit(str, " "))
         featureVecs[i, j] <- as.numeric(vals[3])
      }
   }

   return(featureVecs)
}
