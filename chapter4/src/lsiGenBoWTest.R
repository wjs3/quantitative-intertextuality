# lsiGenBoWTest.R
# Produce Bag-of-Words representations of query texts to be stored
# as binary files
#
# Assumes all markup/metadata has been removed.
#
# Usage: lsiGenBoWTest(<query text directory>, <output directory>, <training bag-of-words binary>)
#
# <query text directory> contains the individual query texts
# <output diretory> is the name of a directory where the output binaries should be written to
# <training bag-of-words binary> is the bag-of-words matrix of the training texts that will be used to prepare
#                                each query in the proper term context.
#
# Example: produce BoW representations for all 100 word samples from Ibn Kathir's Tafsir, using
#          the Qur'an for context. 
# > source("lsiGenBoWTest.R")
# > dir.create("../data/tafsir_ibn_kathir_BoW/tafsir_ibn_kathir_quran/")
# > lsiGenBoWTest("../data/tafsir_ibn_kathir_samples_100_words/", 
#                 "../data/tafsir_ibn_kathir_BoW/tafsir_ibn_kathir_quran/",
#                 "../data/quran_BoW/Quran_BoW.bin")
# > list.files("../data/tafsir_ibn_kathir_BoW/tafsir_ibn_kathir_quran/")
# [1] "tafsir_93_1.bin" "tafsir_93_2.bin" "tafsir_93_3.bin" "tafsir_93_4.bin"
# [5] "tafsir_93_5.bin" "tafsir_93_6.bin" "tafsir_93_7.bin"
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

lsiGenBoWTest <- function(inputTextDir, outputTextDir, trainingBoWFile) {
   library("lsa")

   fileList <- list.files(inputTextDir)
   load(trainingBoWFile)

   for (i in 1:length(fileList)) {
      # Use the LSA package to generate the BoW representations needed for LSI
      queryText <- paste(inputTextDir, fileList[i], sep="")
      tmpText <- scan(queryText, what="", sep="\n")
      td <- tempfile()
      dir.create(td)
      write( tmpText, file=paste(td, "A1", sep="/") )
      matrix2 <- textmatrix(td, vocabulary=rownames(matrix1), minWordLength=1)
      outputFile <- paste(outputTextDir, strsplit(fileList[i], ".txt"), ".bin", sep="")
      save(matrix2, file=outputFile)
      unlink(td, recursive=TRUE)
   }
}
