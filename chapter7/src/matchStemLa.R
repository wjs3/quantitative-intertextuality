# matchStemLa.R
# Calculate the number of stem-based matches between one sample and another.
#
# Assumes all markup/metadata has been removed and that all words have
# been stemmed. This function is not symmetric (parameter order matters).
#
# Usage: matchStemLa(<sample 1>, <sample 2>)
#
# <sample x> sample pre-loaded into memory
#
# Example: Calculate stem-based matches between a sample from Lucan's Civil War and 
#          Vergil's Aeneid.
# > source("matchStemLa.R")
# > sample1Stemmed <- "bellum-bellus per emathios multus qui-quis2-quam-quis civilis campus ius2 do-data-datus scelerus-scelus cano populus-populus2 potens-possum in suus victrix converto viscera dextera-dexter cognatus-cognata acies et rumpo foedus2-foedo regnum certo2-certatus totus concutio-concussus vis orbus-orbis-orba in communis-commune nefas infestus obvius-obvio signum signum-signo par-pareo-paro aquilus-aquila et pila3-pila-pilum-pilo minor pilus-pilum-pila"
# > sample2Stemmed <- "sed pater omnipotens spelunca abdo ater huc-hic metuo moles et mons insuper altus-alo impono rex do-dedo qui-quis2-qui2-quis foedus2-foedo certus-certo2 et premo et laxo-laxus scio do iussus-iubeo habena"
# > matchStemLa(sample1Stemmed, sample2Stemmed)
# > [1] "et"                  "do-dedo"             "qui-quis2-qui2-quis"
# > [4] "foedus2-foedo"       "certus-certo2"       "et"
# > [7] "et"                  "do"
# > matchStemLa(sample2Stemmed, sample1Stemmed)
# > [1] "qui-quis2-quam-quis" "do-data-datus"       "et"
# > [4] "foedus2-foedo"       "certo2-certatus"     "et"
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

matchStemLa <- function(sample1Stemmed, sample2Stemmed) {
   numMatches <- 1
   sample1Tmp <- strsplit(sample1Stemmed, " ")
   sample2Tmp <- strsplit(sample2Stemmed, " ")

   matchList <- c()

   # make this its own function
   for (i in 1:length(sample2Tmp[[1]])) {

      probesTmp <- strsplit(sample2Tmp[[1]][i], "-")

      matchSentinal <- 0
      for (l in 1:length(probesTmp[[1]])) {

         if (matchSentinal != 0) {
            break;
         }

         for (j in 1:length(sample1Tmp[[1]])) {
            if (grepl("-", sample1Tmp[[1]][j])) {
               wordsTmp <- strsplit(sample1Tmp[[1]][j], "-")
               for (k in 1:length(wordsTmp[[1]])) {
                  if (probesTmp[[1]][l] == wordsTmp[[1]][k]) {
                     matchList[numMatches] <- sample2Tmp[[1]][i]
                     numMatches <- numMatches + 1
                     matchSentinal <- 1
                     break
                  }
               }

               if (matchSentinal != 0) {
                  break
               }
            }
            else {
               if (probesTmp[[1]][l] == sample1Tmp[[1]][j]) {
                  matchList[numMatches] <- sample2Tmp[[1]][i]
                  numMatches <- numMatches + 1
                  matchSentinal <- 1
                  break
               }
            }
         }
      }
   }
   
   return(matchList)
}
