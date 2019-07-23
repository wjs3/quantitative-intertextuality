# longworkStemLa.R
# Translate a document or a corpus to stem list

longworkStemLa <- function(workFile, stemDict, outFile) {

   work <- readLines(workFile)  
   words <- tolower(unlist(strsplit(gsub("[[:punct:]]", "", work), " ")))

   latinTable <- new.env()
   rawLines <- readLines(stemDict)
   for (i in 1:length(rawLines)) {
      entry <- strsplit(rawLines[i], ",")
      latinTable[[entry[[1]][1]]] = entry[[1]][2]
   }

   workStemmed <- c()
   for (i in 1:length(words)) {
      if (words[i] != "") {
         if (!is.null(latinTable[[words[i]]])) {
            workStemmed[i] <- latinTable[[words[i]]]
         }
         else {
            workStemmed[i] <- words[i]
         }
      }
   }

   saveRDS(workStemmed, outFile)
}
