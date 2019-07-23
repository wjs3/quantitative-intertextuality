# lsiGenBoWTrain.R
# Produce Bag-of-Words representations of input texts to be stored
# as binary files
#
# Assumes all markup/metadata has been removed.
#
# Usage: lsiGenBoWTrain(<text directory>, <output file>)
#
# <text directory> is the directory of the texts to be processed
# <output file> is the name of the a file where the output should be written to 
#
# Example: produce a BoW representation of all 250 word samples from the Qu'ran
# > source("lsiGenBoWTrain.R")
# > lsiGenBoWTrain("../data/quran_samples_250_words", "../data/quran_BoW/Quran_BoW.bin")
# > list.files("../data/quran_BoW")
# [1] "Quran_BoW.bin"
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

lsiGenBoWTrain <- function(inputTextDir, outputFile) {
   library("lsa")

   # Use the LSA package to generate the BoW representation needed for LSI
   matrix1 = textmatrix(inputTextDir, minWordLength=1)
   
   # save the output to the target file
   save(matrix1, file=outputFile)
}
