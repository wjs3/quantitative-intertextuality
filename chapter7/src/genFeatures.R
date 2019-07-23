# genFeatures.R
# Generate a feature vector reflecting the output of a diverse feature set
# computed over pairs of text samples.
#
# Prerequisites:
# install.packages("RecordLinkage")
# install.packages("vecsets")
# install.packages("stylo")
#
# Assumes all markup/metadata has been removed.
#
# Usage: genFeatures(<sample 1>, <sample 2>, <document 1>, <document 2>, <corpus>, 
#                    <language>, <stem dictionary>, <label>)
# <sample 1> is a string for the first sample
# <sample 2> is a string for the second sample
# <document 1> is the source document in memory for the first sample that has
#              already been pre-processed via a stemmer (longworkStemLa.R or XXX).
# <document 2> is the path to the source document for the second sample that has
#              already been pre-processed via a setmmer (longworkStemLa.R or XXX)
# <corpus> is a directory of documents that have already been pre-processed via a stemmer
#          (longworkStemLa.R or XXX)
# <language> English ("en") or Latin ("la"). Default is English.
# <stem dictionary> Path to a stem dictionary; required if language is Latin
# <label> Integer label to tag each feature vector with
#
# Example: Generate a feature vector for two samples from the Lucan / Vergil Tesserae Benchmark
# > library("gtools")
# > source("genFeatures.R")
# > source("loadCorpus.R")
# > sample1 <- "bella per emathios plus quam ciuilia campos iusque datum sceleri canimus populumque
#               potentem in sua uictrici conuersum uiscera dextra cognatasque acies et rupto foedere
#               regni certatum totis concussi uiribus orbis in commune nefas infestisque obuia signis
#               signa pares aquilas et pila minantia pilis"
# > sample2 <- "sed pater omnipotens speluncis abdidit atris hoc metuens molemque et montis insuper
#               altos imposuit regemque dedit qui foedere certo et premere et laxas sciret dare iussus
#               habenas"
# > lexicon <- readRDS("../data/lexicon/la.lexicon3.rds")
# > document1 <- readRDS("../testing/texts/lucan.bellum_civile_stemmed.rds")
# > document2 <- readRDS("../testing/texts/vergil.aeneid_stemmed.rds")
# > corpus <- loadCorpus("../testing/texts/corpus")
# > vec <- genFeatures(sample1, sample2, document1, document2, corpus, 1, lang="la", lexicon)
# > length(unlist(strsplit(vec, " ")))
# [1] 81
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

genFeatures <- function(sample1, sample2, document1, document2, corpus, label, lang="en", stemDict) {
   library("RecordLinkage")
   library("vecsets")
   library("stylo")
   library("lsa")
   source("matchStemLa.R")
   source("matchStemUniqLa.R")

   vecStr <- label
   index = 1

   # prepare matrices for MATCH_WORD features
   if (lang == "la") {
      a1 <- tolower(list(sample1))
      dict1 <- sub("($)", "\\\\>", sub("(^)", "\\\\<", tolower(unlist(strsplit(sample2, " ")))))

      a2 <- tolower(list(sample2))
      dict2 <- sub("($)", "\\\\>", sub("(^)", "\\\\<", tolower(unlist(strsplit(sample1, " ")))))
   }
   else if (lang == "en") {
      a1 <- tolower(gsub("[[:punct:]]", "", sample1))
      a1 <- paste(a1, collapse = ' ')
      a2 <- tolower(gsub("[[:punct:]]", "", sample2))
      a2 <- paste(a2, collapse = ' ')
      dict1 <- sub("($)", "\\\\>", sub("(^)", "\\\\<", tolower(unlist(strsplit(a2, " ")))))
      dict2 <- sub("($)", "\\\\>", sub("(^)", "\\\\<", tolower(unlist(strsplit(a1, " ")))))
   }

   dict1 <- setNames(nm=dict1)
   matches1 <- sapply(dict1, grepl, a1)

   dict2 <- setNames(nm=dict2)
   matches2 <- sapply(dict2, grepl, a2)

   # convert sample1 and sample2 to stemmed versions
   sample1Stemmed <- ""
   sample2Stemmed <- ""

   if (lang == "la") {

      words1 <- strsplit(sample1, " ")
      words2 <- strsplit(sample2, " ")

      if (!is.null(stemDict[[words1[[1]][1]]])) {
         sample1Stemmed <- stemDict[[words1[[1]][1]]]
      }
      else {
         sample1Stemmed <- paste(sample1Stemmed, words1[[1]][1])
      }

      for (i in 2:length(words1[[1]])) {
         if (!is.null(stemDict[[words1[[1]][i]]])) {
            sample1Stemmed <- paste(sample1Stemmed, stemDict[[words1[[1]][i]]])
         }
         else {
            sample1Stemmed <- paste(sample1Stemmed, words1[[1]][i])
         }
      }

      if (!is.null(stemDict[[words2[[1]][1]]])) {
         sample2Stemmed <- stemDict[[words2[[1]][1]]]
      }
      else {
         sample2Stemmed <- paste(sample2Stemmed, words2[[1]][1])  
      }

      for (i in 2:length(words2[[1]])) {
         if (!is.null(stemDict[[words2[[1]][i]]])) {
            sample2Stemmed <- paste(sample2Stemmed, stemDict[[words2[[1]][i]]])
         }
         else {
            sample2Stemmed <- paste(sample2Stemmed, words2[[1]][i])
         }
      }
   } else if (lang == "en") {

      words1 <- unlist(strsplit(a1, " "))
      words2 <- unlist(strsplit(a2, " "))

      sample1Stemmed <- wordStem(words1[1])
      for (i in 2:length(words1)) {
         sample1Stemmed <- paste(sample1Stemmed, wordStem(words1[i]))
      }

      sample2Stemmed <- wordStem(words2[1])
      for (i in 2:length(words2)) {
         sample2Stemmed <- paste(sample2Stemmed, wordStem(words2[i]))
      }
   }

   corpusStemmed <- corpus$corpusStemmed
   corpusIndFiles <- corpus$corpusIndFiles

   # MATCH_WORD_1: number of exact-word matches in sample1
   MATCH_WORD_1 <- match_word(matches2)
   vecStr <- paste(vecStr, " ", index, ":", MATCH_WORD_1, sep="")
   index <- index + 1

   # MATCH_WORD_2: number of exact-word matches in sample2
   MATCH_WORD_2 <- match_word(matches1)
   vecStr <- paste(vecStr, " ", index, ":", MATCH_WORD_2, sep="")
   index <- index + 1

   # MATCH_WORD_BOTH: total number of exact-word matches
   MATCH_WORD_BOTH <- MATCH_WORD_1 + MATCH_WORD_2
   vecStr <- paste(vecStr, " ", index, ":", MATCH_WORD_BOTH, sep="")
   index <- index + 1

   # MATCH_STEM_1: number of stem-based matches in sample1
   if (lang == "la") {
      matchListFull1 <- matchStemLa(sample2Stemmed, sample1Stemmed)
      MATCH_STEM_1 <- length(matchListFull1)
   }
   else if (lang == "en") {
      dictEnStem2 <- sub("($)", "\\\\>", sub("(^)", "\\\\<", unlist(strsplit(sample1Stemmed, " "))))
      dictEnStem2 <- setNames(nm=dictEnStem2)
      matchesEnStem2 <- sapply(dictEnStem2, grepl, sample2Stemmed)
      matchListFull1 <- names(matchesEnStem2[matchesEnStem2 == TRUE])
      matchListFull1 <- sub("\\\\<", "", sub("\\\\>", "", matchListFull1))
      MATCH_STEM_1 <- length(matchListFull1)
   }
   vecStr <- paste(vecStr, " ", index, ":", MATCH_STEM_1, sep="")
   index <- index + 1

   # MATCH_STEM_2: number of stem-based matches in sample2
   if (lang == "la") {
      matchListFull2 <- matchStemLa(sample1Stemmed, sample2Stemmed)
      MATCH_STEM_2 <- length(matchListFull2)
   }
   else if (lang == "en") {
      dictEnStem1 <- sub("($)", "\\\\>", sub("(^)", "\\\\<", unlist(strsplit(sample2Stemmed, " "))))
      dictEnStem1 <- setNames(nm=dictEnStem1)
      matchesEnStem1 <- sapply(dictEnStem1, grepl, sample1Stemmed)
      matchListFull2 <- names(matchesEnStem1[matchesEnStem1 == TRUE])
      matchListFull2 <- sub("\\\\<", "", sub("\\\\>", "", matchListFull2))
      MATCH_STEM_2 <- length(matchListFull2)
   }
   vecStr <- paste(vecStr, " ", index, ":", MATCH_STEM_2, sep="")
   index <- index + 1

   # MATCH_STEM_BOTH: number of stem-based matches in both
   MATCH_STEM_BOTH <- MATCH_STEM_1 + MATCH_STEM_2
   vecStr <- paste(vecStr, " ", index, ":", MATCH_STEM_BOTH, sep="")
   index <- index + 1

   # MATCH_STEM_UNIQ: number of unique forms among words matching by stem
   if (lang == "la") {
      MATCH_STEM_UNIQ <- matchStemUniqLa(sample1Stemmed, sample2Stemmed)
   }
   else if (lang == "en") {
      MATCH_STEM_UNIQ <- length(unique(matchListFull1))
   }
   vecStr <- paste(vecStr, " ", index, ":", MATCH_STEM_UNIQ, sep="")
   index <- index + 1

   # MATCH_WORD_UNIQ: number of unique forms among words matching by word
   allMatches <- matches1[matches1 == TRUE]
   matchesExt <- allMatches[allMatches == TRUE]
   MATCH_WORD_UNIQ <- length(unique(names(matchesExt)))
   vecStr <- paste(vecStr, " ", index, ":", MATCH_WORD_UNIQ, sep="")
   index <- index + 1

   # Assume matchListx holds unique words at this point
   # F_M_DOC_AVG_1: Word Matches Doc. Specific Mean Freq. in Sample 1
   matchList1 <- unique(matchListFull1)
   F_M_DOC_AVG_1 <- f_avg(matchList1, document1)
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_AVG_1$ftr, sep="")
   index <- index + 1

   # F_M_DOC_AVG_2: Word matches doc. specific mean freq. in sample 2
   matchList2 <- unique(matchListFull2)
   F_M_DOC_AVG_2 <- f_avg(matchList2, document2)
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_AVG_2$ftr, sep="")
   index <- index + 1

   # F_M_DOC_AVG_BOTH: Word matches doc. specific mean freq. both
   if (length(matchList1) == 0 || length(matchList2) == 0) {
      F_M_DOC_AVG_BOTH <- 0
   } 
   else {
      F_M_DOC_AVG_BOTH <- (F_M_DOC_AVG_1$runningSum + F_M_DOC_AVG_2$runningSum) / (length(matchList1) + length(matchList2))
   }
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_AVG_BOTH, sep="")
   index <- index + 1

   # F_M_DOC_MIN_1: Word matches doc. specific min. freq. in sample 1
   F_M_DOC_MIN_1 <- f_min(matchList1, F_M_DOC_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_MIN_1, sep="")
   index <- index + 1

   # F_M_DOC_MIN_2: Word matches doc. specific min. freq. in sample 2
   F_M_DOC_MIN_2 <- f_min(matchList2, F_M_DOC_AVG_2$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_MIN_2, sep="")
   index <- index + 1

   # F_M_DOC_MIN_BOTH: Word matches doc. specific min. freq. both
   if (F_M_DOC_MIN_2 < F_M_DOC_MIN_1) {
      vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_MIN_2, sep="")
   }
   else {
      vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_MIN_1, sep="")
   }
   index <- index + 1

   # F_M_DOC_INV_1: Word matches doc. specific sum of (1/F) for each freq. F in sample 1
   F_M_DOC_INV_1 <- f_inv(matchList1, F_M_DOC_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_INV_1, sep="")
   index <- index + 1

   # F_M_DOC_INV_2: Word matches doc. specific sum of (1/F) for each freq. F in sample 2
   F_M_DOC_INV_2 <- f_inv(matchList2, F_M_DOC_AVG_2$matchListFreqs) 
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_INV_2, sep="")
   index <- index + 1

   # F_M_DOC_INV_BOTH: Word matches doc. specific inv. freq. both
   F_M_DOC_INV_BOTH <- F_M_DOC_INV_1 + F_M_DOC_INV_2
   vecStr <- paste(vecStr, " ", index, ":", F_M_DOC_INV_BOTH, sep="")
   index <- index + 1

   # F_M_COR_AVG_1: Word matches corpus-wide mean freq. in sample 1
   F_M_COR_AVG_1 <- f_avg(matchList1, corpusStemmed)
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_AVG_1$ftr, sep="")
   index <- index + 1

   # F_M_COR_AVG_2: Word matches corpus-wide mean freq. in sample 2
   F_M_COR_AVG_2 <- f_avg(matchList2, corpusStemmed)
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_AVG_2$ftr, sep="")
   index <- index + 1

   # F_M_COR_AVG_BOTH: Word matches corpus-wide mean freq. both
   if (length(matchList1) == 0 || length(matchList2) == 0) {
      F_M_COR_AVG_BOTH <- 0
   }
   else {
      F_M_COR_AVG_BOTH <- (F_M_COR_AVG_1$runningSum + F_M_COR_AVG_2$runningSum) / (length(matchList1) + length(matchList2))
   }
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_AVG_BOTH, sep="")
   index <- index + 1

   # F_M_COR_MIN_1: Word matches corpus-wide min. freq. in sample 1
   F_M_COR_MIN_1 <- f_min(matchList1, F_M_COR_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_MIN_1, sep="")
   index <- index + 1

   # F_M_COR_MIN_2: Word matches corpus-wide min. freq. in sample 2
   F_M_COR_MIN_2 <- f_min(matchList2, F_M_COR_AVG_2$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_MIN_2, sep="")
   index <- index + 1

   # F_M_COR_MIN_BOTH: Word matches corpus-wide min. freq. both
   if (F_M_COR_MIN_2 < F_M_COR_MIN_1) {
      vecStr <- paste(vecStr, " ", index, ":", F_M_COR_MIN_2, sep="")
   }
   else {
      vecStr <- paste(vecStr, " ", index, ":", F_M_COR_MIN_1, sep="")
   }
   index <- index + 1

   # F_M_COR_INV_1: Word matches corpus-wide sum of (1/F) for each freq. F in sample 1
   F_M_COR_INV_1 <- f_inv(matchList1, F_M_COR_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_INV_1, sep="")
   index <- index + 1

   # F_M_COR_INV_2: Word matches corpus-wide sum of (1/F) for each freq. F in sample 2
   F_M_COR_INV_2 <- f_inv(matchList2, F_M_COR_AVG_2$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_INV_2, sep="")
   index <- index + 1

   # F_M_COR_INV_BOTH: Word matches corpus-wide inv. freq. both
   F_M_COR_INV_BOTH <- F_M_COR_INV_1 + F_M_COR_INV_2
   vecStr <- paste(vecStr, " ", index, ":", F_M_COR_INV_BOTH, sep="")
   index <- index + 1

   # allWordsx reflects unique word list
   # F_P_DOC_AVG_1: All words doc. specific mean freq. in sample 1
   allWords1 <- unique(unlist(strsplit(sample1Stemmed, " ")))
   F_P_DOC_AVG_1 <- f_avg(allWords1, document1)
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_AVG_1$ftr, sep="")
   index <- index + 1

   # F_P_DOC_AVG_2: All words doc. specific mean freq. in sample 2
   allWords2 <- unique(unlist(strsplit(sample2Stemmed, " ")))
   F_P_DOC_AVG_2 <- f_avg(allWords2, document2)
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_AVG_2$ftr, sep="")
   index <- index + 1

   # F_P_DOC_AVG_BOTH: All words doc. specific mean freq. both
   F_P_DOC_AVG_BOTH <- (F_P_DOC_AVG_1$runningSum + F_P_DOC_AVG_2$runningSum) / (length(allWords1) + length(allWords2))
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_AVG_BOTH, sep="")
   index <- index + 1 

   # F_P_DOC_MIN_1: All words doc. specific min. freq. in sample 1
   F_P_DOC_MIN_1 <- f_min(allWords1, F_P_DOC_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_MIN_1, sep="")
   index <- index + 1

   # F_P_DOC_MIN_2: All words doc. specific min. freq. in sample 2
   F_P_DOC_MIN_2 <- f_min(allWords2, F_P_DOC_AVG_2$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_MIN_2, sep="")
   index <- index + 1

   # F_P_DOC_MIN_BOTH: All words doc. specific min. freq. both
   if (F_P_DOC_MIN_2 < F_P_DOC_MIN_1) {
      vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_MIN_2, sep="")
   }
   else {
      vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_MIN_1, sep="")
   }
   index <- index + 1
  
   # F_P_DOC_INV_1: All words doc. specific sum of (1/F) for each freq. F in sample 1
   F_P_DOC_INV_1 <- f_inv(allWords1, F_P_DOC_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_INV_1, sep="")
   index <- index + 1

   # F_P_DOC_INV_2: All words doc. specific sum of (1/F) for each freq. F in sample 2
   F_P_DOC_INV_2 <- f_inv(allWords2, F_P_DOC_AVG_2$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_INV_2, sep="")
   index <- index + 1

   # F_P_DOC_INV_BOTH: All words doc. specific inv. freq. both
   F_P_DOC_INV_BOTH <- F_P_DOC_INV_1 + F_P_DOC_INV_2
   vecStr <- paste(vecStr, " ", index, ":", F_P_DOC_INV_BOTH, sep="")
   index <- index + 1

   # F_P_COR_AVG_1: All words corpus-wide mean freq. in sample 1
   F_P_COR_AVG_1 <- f_avg(allWords1, corpusStemmed)
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_AVG_1$ftr, sep="")
   index <- index + 1

   # F_P_COR_AVG_2: All words corpus-wide mean freq. in sample 2
   F_P_COR_AVG_2 <- f_avg(allWords2, corpusStemmed)
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_AVG_2$ftr, sep="")
   index <- index + 1

   # F_P_COR_AVG_BOTH: All words corpus-wide mean freq. both
   F_P_COR_AVG_BOTH <- (F_P_COR_AVG_1$runningSum + F_P_COR_AVG_2$runningSum) / (length(allWords1) + length(allWords2))
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_AVG_BOTH, sep="")
   index <- index + 1

   # F_P_COR_MIN_1: All words corpus-wide min. freq. in sample 1
   F_P_COR_MIN_1 <- f_min(allWords1, F_P_COR_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_MIN_1, sep="")
   index <- index + 1

   # F_P_COR_MIN_2: All words corpus-wide min. freq. in sample 2
   F_P_COR_MIN_2 <- f_min(allWords2, F_P_COR_AVG_2$matchListFreqs) 
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_MIN_2, sep="")
   index <- index + 1

   # F_P_COR_MIN_BOTH: All words corpus-wide min. freq. both
   if (F_P_COR_MIN_2 < F_P_COR_MIN_1) {
      vecStr <- paste(vecStr, " ", index, ":", F_P_COR_MIN_2, sep="")
   }
   else {
      vecStr <- paste(vecStr, " ", index, ":", F_P_COR_MIN_1, sep="")
   }
   index <- index + 1

   # F_P_COR_INV_1: All words corpus-wide sum of (1/F) for each freq. F in sample 1
   F_P_COR_INV_1 <- f_inv(allWords1, F_P_COR_AVG_1$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_INV_1, sep="")
   index <- index + 1

   # F_P_COR_INV_2: All words corpus-wide sum of (1/F) for each freq. F in sample 2
   F_P_COR_INV_2 <- f_inv(allWords2, F_P_COR_AVG_2$matchListFreqs)
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_INV_2, sep="")
   index <- index + 1

   # F_P_COR_INV_BOTH: All words corpus-wide inv. freq. both
   F_P_COR_INV_BOTH <- F_P_COR_INV_1 + F_P_COR_INV_2
   vecStr <- paste(vecStr, " ", index, ":", F_P_COR_INV_BOTH, sep="")
   index <- index + 1

   # TF_M_T_AVG_1: Mean TF-IDF of matching words for sample 1
   TF_M_T_AVG_1 <- tf_avg(matchList1, document1, corpusIndFiles)
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_AVG_1$ftr, sep="")
   index <- index + 1 

   # TF_M_T_AVG_2: Mean TF-IDF of matching words for sample 2
   TF_M_T_AVG_2 <- tf_avg(matchList2, document2, corpusIndFiles)
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_AVG_2$ftr, sep="")
   index <- index + 1

   # TF_M_T_AVG_BOTH: Mean TF-IDF of matching words for both
   if (length(matchList1) == 0 || length(matchList2) == 0) {
      TF_M_T_AVG_BOTH <- 0
   }
   else {
      TF_M_T_AVG_BOTH <- (TF_M_T_AVG_1$runningTfIdf + TF_M_T_AVG_2$runningTfIdf) / (length(matchList1) + length(matchList2))
   } 
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_AVG_BOTH, sep="")
   index <- index + 1

   # TF_M_T_SUM_1: Sum of TF-IDF scores of matching words for sample 1
   TF_M_T_SUM_1 <- TF_M_T_AVG_1$runningTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_SUM_1, sep="")
   index <- index + 1

   # TF_M_T_SUM_2: Sum of TF-IDF scores of matching words for sample 2
   TF_M_T_SUM_2 <- TF_M_T_AVG_2$runningTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_SUM_2, sep="")
   index <- index + 1

   # TF_M_T_SUM_BOTH: Sum of TF-IDF scores of matching words for both
   TF_M_T_SUM_BOTH <- TF_M_T_SUM_1 + TF_M_T_SUM_2
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_SUM_BOTH, sep="")
   index <- index + 1

   # TF_M_T_MAX_1: Max of TF-IDF scores of matching words for sample 1
   TF_M_T_MAX_1 <- TF_M_T_AVG_1$maxTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_MAX_1, sep="")
   index <- index + 1

   # TF_M_T_MAX_2: Max of TF-IDF scores of matching words for sample 2
   TF_M_T_MAX_2 <- TF_M_T_AVG_2$maxTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_MAX_2, sep="")
   index <- index + 1

   # TF_M_T_MAX_BOTH: Max of TF-IDF scores of matching words for both
   if (TF_M_T_MAX_1 > TF_M_T_MAX_2) {
      TF_M_T_MAX_BOTH <- TF_M_T_MAX_1
   }
   else {
      TF_M_T_MAX_BOTH <- TF_M_T_MAX_2
   }
   vecStr <- paste(vecStr, " ", index, ":", TF_M_T_MAX_BOTH, sep="")
   index <- index + 1

   # TF_P_T_AVG_1: Mean TF-IDF of all words for sample 1
   TF_P_T_AVG_1 <- tf_avg(allWords1, document1, corpusIndFiles)
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_AVG_1$ftr, sep="")
   index <- index + 1

   # TF_P_T_AVG_2: Mean TF-IDF of all words for sample 2
   TF_P_T_AVG_2 <- tf_avg(allWords2, document2, corpusIndFiles)
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_AVG_2$ftr, sep="")
   index <- index + 1

   # TF_P_T_AVG_BOTH: Mean TF-IDF of all words for both
   TF_P_T_AVG_BOTH <- (TF_P_T_AVG_1$runningTfIdf + TF_P_T_AVG_2$runningTfIdf) / (length(allWords1) + length(allWords2))
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_AVG_BOTH, sep="")
   index <- index + 1

   # TF_P_T_SUM_1: Sum of TF-IDF scores of all words for sample 1
   TF_P_T_SUM_1 <- TF_P_T_AVG_1$runningTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_SUM_1, sep="")
   index <- index + 1

   # TF_P_T_SUM_2: Sum of TF-IDF scores of all words for sample 2
   TF_P_T_SUM_2 <- TF_P_T_AVG_2$runningTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_SUM_2, sep="")
   index <- index + 1

   # TF_P_T_SUM_BOTH: Sum of TF-IDF scores of all words for both
   TF_P_T_SUM_BOTH <- TF_P_T_SUM_1 + TF_P_T_SUM_2
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_SUM_BOTH, sep="")
   index <- index + 1

   # TF_P_T_MAX_1: Max of TF-IDF scores of all words for sample 1
   TF_P_T_MAX_1 <- TF_P_T_AVG_1$maxTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_MAX_1, sep="")
   index <- index + 1

   # TF_P_T_MAX_2: Max of TF-IDF scores of all words for sample 2
   TF_P_T_MAX_2 <- TF_P_T_AVG_2$maxTfIdf
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_MAX_2, sep="")
   index <- index + 1

   # TF_P_T_MAX_BOTH: Max of TF-IDF scores of all words for both
   if (TF_P_T_MAX_1 > TF_P_T_MAX_2) {
      TF_P_T_MAX_BOTH <- TF_P_T_MAX_1
   }
   else {
      TF_P_T_MAX_BOTH <- TF_P_T_MAX_2
   }
   vecStr <- paste(vecStr, " ", index, ":", TF_P_T_MAX_BOTH, sep="")
   index <- index + 1

   # SPAN_1: The distance between the furthest two matching words in sample 1
   SPAN_1 <- span(matchListFull1, sample1Stemmed)
   vecStr <- paste(vecStr, " ", index, ":", SPAN_1$ftr, sep="")
   index <- index + 1

   # SPAN_2: The distance between the furthest two matching words in sample 2
   SPAN_2 <- span(matchListFull2, sample2Stemmed)
   vecStr <- paste(vecStr, " ", index, ":", SPAN_2$ftr, sep="")
   index <- index + 1

   # SPAN_BOTH: The combined distance between the furthest two matching words for both
   SPAN_BOTH <- SPAN_1$ftr + SPAN_2$ftr
   vecStr <- paste(vecStr, " ", index, ":", SPAN_BOTH, sep="")
   index <- index + 1

   # D_F_DOC_1: is the distance between the lowest-freq words using doc-specific freqs for sample 1
   D_F_DOC_1 <- d_f(matchList1, document1, SPAN_1$allWordsRep)
   vecStr <- paste(vecStr, " ", index, ":", D_F_DOC_1, sep="")
   index <- index + 1

   # D_F_DOC_2: is the distance between the lowest-freq words using doc-specific freqs for sample 2
   D_F_DOC_2 <- d_f(matchList2, document2, SPAN_2$allWordsRep)
   vecStr <- paste(vecStr, " ", index, ":", D_F_DOC_2, sep="")
   index <- index + 1

   # D_F_DOC_BOTH: is the distance between the lowest-freq words using doc-specific freqs for both samples
   D_F_DOC_BOTH <- D_F_DOC_1 + D_F_DOC_2
   vecStr <- paste(vecStr, " ", index, ":", D_F_DOC_BOTH, sep="")
   index <- index + 1

   # D_F_COR_1: is the distance between the lowest-freq words using corpus-wide freqs for sample 1
   D_F_COR_1 <- d_f(matchList1, corpusStemmed, SPAN_1$allWordsRep)
   vecStr <- paste(vecStr, " ", index, ":", D_F_COR_1, sep="")
   index <- index + 1

   # D_F_COR_2: is the distance between the lowest-freq words using corpus-wide freqs for sample 2
   D_F_COR_2 <- d_f(matchList2, corpusStemmed, SPAN_2$allWordsRep)
   vecStr <- paste(vecStr, " ", index, ":", D_F_COR_2, sep="")
   index <- index + 1

   # D_F_COR_BOTH: is the distance between the lowest-freq words using corpus-wide freqs for both samples
   D_F_COR_BOTH <- D_F_COR_1 + D_F_COR_2
   vecStr <- paste(vecStr, " ", index, ":", D_F_COR_BOTH, sep="")
   index <- index + 1

   # D_TF_T_1: is the distance between the two highest tfidf words for sample 1
   D_TF_T_1 <- d_tf(matchList1, document1, SPAN_1$allWordsRep, corpusIndFiles)
   vecStr <- paste(vecStr, " ", index, ":", D_TF_T_1, sep="")
   index <- index + 1

   # D_TF_T_2: is the distance between the two highest tfidf words for sample 2
   D_TF_T_2 <- d_tf(matchList2, document2, SPAN_2$allWordsRep, corpusIndFiles)
   vecStr <- paste(vecStr, " ", index, ":", D_TF_T_2, sep="")
   index <- index + 1

   # D_TF_T_BOTH: is the distance between the two highest tfidf words for both samples
   D_TF_T_BOTH <- D_TF_T_1 + D_TF_T_2
   vecStr <- paste(vecStr, " ", index, ":", D_TF_T_BOTH, sep="")
   index <- index + 1

   # LD: Levenshtein edit distance
   LD <- levenshteinDist(sample1, sample2)
   vecStr <- paste(vecStr, " ", index, ":", LD, sep="")
   index <- index + 1

   # CHR_1_GR_CNT: fraction of matching character-level unigrams out of total
   words1 = txt.to.words(sample1)
   words2 = txt.to.words(sample2)
   chars1 = txt.to.features(words1, features = "c")
   chars2 = txt.to.features(words2, features = "c")
   unigrams1 <- make.ngrams(chars1, ngram.size = 1)
   unigrams2 <- make.ngrams(chars2, ngram.size = 1)
   commonUnigrams <- length(vintersect(unigrams1, unigrams2))
   allUnigrams <- length(vunion(unigrams1, unigrams2))
   CHR_1_GR_CNT <- commonUnigrams / allUnigrams
   vecStr <- paste(vecStr, " ", index, ":", CHR_1_GR_CNT, sep="")
   index <- index + 1

   # CHR_2_GR_CNT: fraction of matching character-level bigrams out of total
   bigrams1 <- make.ngrams(chars1, ngram.size = 2)
   bigrams2 <- make.ngrams(chars2, ngram.size = 2)
   commonBigrams <- length(vintersect(bigrams1, bigrams2))
   allBigrams <- length(vunion(bigrams1, bigrams2))
   CHR_2_GR_CNT <- commonBigrams / allBigrams
   vecStr <- paste(vecStr, " ", index, ":", CHR_2_GR_CNT, sep="")
   index <- index + 1

    # CHR_3_GR_CNT: fraction of matching character-level trigrams out of total
   trigrams1 <- make.ngrams(chars1, ngram.size = 3)
   trigrams2 <- make.ngrams(chars2, ngram.size = 3)
   commonTrigrams <- length(vintersect(trigrams1, trigrams2))
   allTrigrams <- length(vunion(trigrams1, trigrams2))
   CHR_3_GR_CNT <- commonTrigrams / allTrigrams
   vecStr <- paste(vecStr, " ", index, ":", CHR_3_GR_CNT, sep="")
   index <- index + 1

   # CHR_2_GR_FRQ: similarity of bigram frequencies between two strings
   uniqueBigrams <- union(bigrams1, bigrams2)
   sample1Vec <- c()
   sample2Vec <- c()
   for (i in 1:length(uniqueBigrams)) {
      countBigram1 <- length(bigrams1[bigrams1 == uniqueBigrams[i]])
      unigram <- strsplit(uniqueBigrams[i], "")[[1]][1]

      if (countBigram1 == 0) {
         sample1Vec[i] <- 0
      } else {
         countUnigram1 <- length(unigrams1[unigrams1 == unigram])
         sample1Vec[i] <- countBigram1 / countUnigram1
      }

      countBigram2 <- length(bigrams2[bigrams2 == uniqueBigrams[i]])

      if (countBigram2 == 0) {
         sample2Vec[i] <- 0
      } else {
         countUnigram2 <- length(unigrams2[unigrams2 == unigram])
         sample2Vec[i] <- countBigram2 / countUnigram2
      }
   }
   CHR_2_GR_FRQ <- cosine(sample1Vec, sample2Vec)
   vecStr <- paste(vecStr, " ", index, ":", CHR_2_GR_FRQ, sep="")
   index <- index + 1

   # CHR_3_GR_FRQ: similarity of trigram frequencies between two strings
   uniqueTrigrams <- union(trigrams1, trigrams2)
   sample1Vec <- c()
   sample2Vec <- c()
   for (i in 1:length(uniqueTrigrams)) {
      countTrigram1 <- length(trigrams1[trigrams1 == uniqueTrigrams[i]])
      char1 <- strsplit(uniqueTrigrams[i], "")[[1]][1]
      char2 <- strsplit(uniqueTrigrams[i], "")[[1]][3]
      bigram <- paste(char1, char2)

      if (countTrigram1 == 0) {
         sample1Vec[i] <- 0
      } else {
         countBigram1 <- length(bigrams1[bigrams1 == bigram])
         sample1Vec[i] <- countTrigram1 / countBigram1
      }

      countTrigram2 <- length(trigrams2[trigrams2 == uniqueTrigrams[i]])

      if (countTrigram2 == 0) {
         sample2Vec[i] <- 0
      } else {
         countBigram2 <- length(bigrams2[bigrams2 == bigram])
         sample2Vec[i] <- countTrigram2 / countBigram2
      }
   }
   CHR_3_GR_FRQ <- cosine(sample1Vec, sample2Vec)
   vecStr <- paste(vecStr, " ", index, ":", CHR_3_GR_FRQ, sep="")
   index <- index + 1

   # return feature vector in LIBSVM format
   return(vecStr)
}

match_word <- defmacro(matches, expr = {
   ftr <- length(matches[matches == TRUE])
   return(ftr)
})

f_avg <- defmacro(matchList, txt, expr = {
   if (length(matchList) == 0) {
      return(list("runningSum" = 0, "matchListFreqs" = 0, "ftr" = 0))
   }
   else {
      runningSum <- 0
      matchListFreqs <- c()
      for (i in 1:length(matchList)) {
         matchListFreqs[i] <- (length(txt[txt == matchList[i]]) / length(txt))
         runningSum <- runningSum + matchListFreqs[i]
      }
      ftr <- runningSum / length(matchList)
      return(list("runningSum" = runningSum, "matchListFreqs" = matchListFreqs, "ftr" = ftr))
   }
})

f_min <- defmacro(matchList, matchListFreqs, expr = {
   if (length(matchList) == 0) {
     ftr <- 0
   }
   else {
      ftr <- 1
      for (i in 1:length(matchList)) {
         freq <- matchListFreqs[i]
         if (freq < ftr) {
            ftr <- freq
         }
      }
   }
   return(ftr)
})

f_inv <- defmacro(matchList, matchListFreqs, expr = {
   if (length(matchList) == 0) {
      return(0)
   }
   else {
      ftr <- 0
      for (i in 1:length(matchList)) {
         freq <- matchListFreqs[i]
         ftr <- ftr + (1/freq)
      }
      return(ftr)
   }
})

tf_avg <- defmacro(matchList, txt, cor, expr = {
   if (length(matchList) == 0) {
      return(list("runningTfIdf" = 0, "maxTfIdf" = 0, "ftr" = 0)) 
   } else {
      runningTfIdf <- 0
      maxTfIdf <- -1
      for (i in 1:length(matchList)) {
         tf <- length(txt[txt == matchList[i]])

         numMatches <- 0
         for (j in 1:length(ls(cor))) {
            currDoc <- get(paste("doc", j, sep=""), corpusIndFiles)
            if (!is.null(currDoc[[matchList[i]]])) {
               numMatches <- numMatches + 1
            }
         }

         idf <- log(length(ls(cor)) / numMatches)
         runningTfIdf <- runningTfIdf + (tf * idf)
         if ((tf * idf) > maxTfIdf) {
            maxTfIdf <- (tf * idf)
         }
      }
      ftr <- runningTfIdf / length(matchList)
      return(list("runningTfIdf" = runningTfIdf, "maxTfIdf" = maxTfIdf, "ftr" = ftr))
   }
})

span <- defmacro(matchListFull, sampleStemmed, expr = {
   if (length(matchListFull) < 2) {
      return(list("allWordsRep" = list(), "ftr" = 0))
   } else {
      ftr <- 0
      pairs <- combn(matchListFull, 2)
      allWordsRep <- unlist(strsplit(sampleStemmed, " "))
      for (i in 1:(length(pairs) / 2)) {
         tmp1 <- sort(which(allWordsRep %in% pairs[1,i]))
         tmp2 <- sort(which(allWordsRep %in% pairs[2,i]))
         distVal <- abs(tmp1[1] - tmp2[length(tmp2)])
         if (distVal > ftr) {
            ftr <- distVal
         }
      }
      return(list("allWordsRep" = allWordsRep, "ftr" = ftr))
   }
})

d_f <- defmacro(matchList, txt, allWordsRep, expr = { 
   if (length(matchList) < 2) {
      return(0)
   }
   else {
      firstLowestFreq <- 1
      firstLowestWord <- ""
      secondLowestFreq <- 1
      secondLowestWord <- ""
      for (i in 1:length(matchList)) {
         freq <- (length(txt[txt == matchList[i]]) / length(txt))
         if (freq < firstLowestFreq) {
            secondLowestFreq <- firstLowestFreq
            secondLowestWord <- firstLowestWord
            firstLowestFreq <- freq
            firstLowestWord <- matchList[i]
         } else if (freq < secondLowestFreq) {
            secondLowestFreq <- freq
            secondLowestWord <- matchList[i]
         }
      }
      tmp1 <- sort(which(allWordsRep %in% firstLowestWord))
      tmp2 <- sort(which(allWordsRep %in% secondLowestWord))
      ftr <- abs(tmp1[1] - tmp2[length(tmp2)])       # take the max distance
      return(ftr)
   }
})

d_tf <- defmacro(matchList, txt, allWordsRep, cor, expr = {
   if (length(matchList) < 2) {
      return(0)
   }
   else {
      firstHighestTfIdf <- -1
      firstHighestWord <- ""
      secondHighestTfIdf <- -1
      secondHighestWord <- ""
      for (i in 1:length(matchList)) {
         tf <- length(txt[txt == matchList[i]])

         numMatches <- 0
         for (j in 1:length(ls(cor))) {
            currDoc <- get(paste("doc", j, sep=""), cor)
            if (!is.null(currDoc[[matchList[i]]])) {
               numMatches <- numMatches + 1
            }
         }

         idf <- log(length(cor) / numMatches)
         if ((tf * idf) > firstHighestTfIdf) {
            secondHighestTfIdf <- firstHighestTfIdf
            secondHighestWord <- firstHighestWord
            firstHighestTfIdf <- (tf * idf)
            firstHighestWord <- matchList[i]
         } else if ((tf * idf) > secondHighestTfIdf) {
            secondHighestTfIdf <- (tf * idf)
            secondHighestWord <- matchList[i]
         }
      }
      tmp1 <- sort(which(allWordsRep %in% firstHighestWord))
      tmp2 <- sort(which(allWordsRep %in% secondHighestWord))
      ftr <- abs(tmp1[1] - tmp2[length(tmp2)])       # take the max distance
      return(ftr)
   }
})
