# matchStemUniqLa.R
# Calculate the number of unique stem-based matches between two samples.
#
# Assumes all markup/metadata has been removed and that all words have
# been stemmed. This function is symmetric (parameter order does not matter).
#
# Usage: matchStemUniqLa(<sample 1>, <sample 2>)
#
# <sample x> sample pre-loaded into memory
#
# Example: Calculate stem-based matches between a sample from Lucan's Civil War and
#          Vergil's Aeneid.
# > source("matchStemUniqLa.R")
# > sample1Stemmed <- "bellum-bellus per emathios multus qui-quis2-quam-quis civilis campus ius2 do-data-datus scelerus-scelus cano populus-populus2 potens-possum in suus victrix converto viscera dextera-dexter cognatus-cognata acies et rumpo foedus2-foedo regnum certo2-certatus totus concutio-concussus vis orbus-orbis-orba in communis-commune nefas infestus obvius-obvio signum signum-signo par-pareo-paro aquilus-aquila et pila3-pila-pilum-pilo minor pilus-pilum-pila"
# > sample2Stemmed <- "sed pater omnipotens spelunca abdo ater huc-hic metuo moles et mons insuper altus-alo impono rex do-dedo qui-quis2-qui2-quis foedus2-foedo certus-certo2 et premo et laxo-laxus scio do iussus-iubeo habena"
# > matchStemUniqLa(sample1Stemmed, sample2Stemmed)
# > [1] 7
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

matchStemUniqLa <- function(sample1Stemmed, sample2Stemmed) {
   sample1Tmp <- strsplit(sample1Stemmed, " ")
   sample2Tmp <- strsplit(sample2Stemmed, " ")

   matchList <- c()

   # make this its own function
   for (i in 1:length(sample2Tmp[[1]])) {

      probesTmp <- strsplit(sample2Tmp[[1]][i], "-")
      probesTmp2 <- unique(gsub("[0-9]", "", probesTmp[[1]]))

      for (l in 1:length(probesTmp2)) {

         for (j in 1:length(sample1Tmp[[1]])) {
            if (grepl("-", sample1Tmp[[1]][j])) {
               wordsTmp <- strsplit(sample1Tmp[[1]][j], "-")
               wordsTmp2 <- unique(gsub("[0-9]", "", wordsTmp[[1]]))
               for (k in 1:length(wordsTmp2)) {
                  if (probesTmp2[l] == wordsTmp2[k]) {
#                     print(probesTmp2[l])
                     matchList <- c(matchList, probesTmp2[l])
                  }
               }
            }
            else {
               if (probesTmp2[l] == sample1Tmp[[1]][j]) {
#                  print(probesTmp2[l])
                  matchList <- c(matchList, probesTmp2[l])
               }
            }
         }
      }
   }
 
    
   numMatches <- length(unique(unlist(matchList))) 
   return(numMatches)
}
