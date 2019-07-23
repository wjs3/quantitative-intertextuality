# prepLaLexicon.R
#
# Convert a Latin lexicon conforming to Helma Dik's syntax into a 
# hash table

prepLaLexicon <- function(stemDict, outFile) {
   
   rawLines <- readLines(stemDict)
   latinTable <- new.env()
   for (i in 1:length(rawLines)) {
      entry <- strsplit(rawLines[i], ",")
      latinTable[[entry[[1]][1]]] = entry[[1]][2]
   }

   saveRDS(latinTable, outFile)
}
