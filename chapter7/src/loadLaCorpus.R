# loadLaCorpus.R
#
# Load a corpus into memory before processing samples for features

loadLaCorpus <- function(corpus) {
   
   # Prepare corpus
   corpusList <- dir(corpus, full.names=TRUE)
   
   # for F_* features
   corpusStemmed <- c()

   # for TF-IDF features
   corpusIndFiles <- new.env()

   for (i in 1:length(corpusList)) {
      docTmp <- readRDS(corpusList[i])
      corpusStemmed <- c(corpusStemmed, docTmp)

      assign(paste("doc", i, sep=""), new.env(), corpusIndFiles)
      for (j in 1:length(docTmp)) {
         assign(docTmp[j], 1, get(paste("doc", i, sep=""), corpusIndFiles))
      }
   }

   return(list("corpusStemmed" = corpusStemmed, "corpusIndFiles" = corpusIndFiles))
}
