# longworkStemEn.R
# Translate a document or a corpus to stem list

longworkStemEn <- function(workFile, outFile) {

   work <- readLines(workFile)  
   words <- tolower(unlist(strsplit(gsub("[[:punct:]]", "", work), " ")))

   workStemmed <- c()
   idx <- 1
   for (i in 1:length(words)) {
      if (words[i] != "") {
         stemVal <- wordStem(words[i])

         if (stemVal != "") {
            workStemmed[idx] <- stemVal
            idx <- idx + 1
         }
      }
   }

   saveRDS(workStemmed, outFile)
}
