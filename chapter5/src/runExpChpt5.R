# runExpChpt5.R
# Run the open set influence detection experiment from Chapter 5.
# The hypothesis is that Wordsworth's poetry will be close to that
# of Copwer in sounds space, while writers with no clear relationship
# to Cowper will be much farther away. To test this, we look at a
# one-class model trained on data only from Copwer.
#
# Usage: function(<functNgrams>, <numNgrams>)
# <functNgrams> is a list of functional n-grams that will be used for 
#               influence detection
# <numNgrams> is the number of overall n-grams to compute (this should
#             be a number larger than the number of n-grams in functNgrams)
#
# Example: > source("runExpChpt5.R")
#          > runExpChpt5(c("th", "he", "er", "re"), 50)
#
# Expected output:
# [1] "Model's fit to Cowper: 0.901961"
# [1] "Wordsworth's proximity to Cowper: 0.192308"
# [1] "Browning's proximity to Cowper: 0.833333"
# [1] "Carroll's proximity to Cowper: 0.687500"
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

runExpChpt5 <- function(functNgrams, numNgrams) {
   # dependencies
   library("e1071")

   source("sampleText.R")
   source("functionalNgramGenerator.R")
   source("genFeatureVecs.R")

   # generate consistent samples for learning
   cowper <- sampleText("../data/cowper/cowper.task.all", 100)
   wordsworth <- sampleText("../data/wordsworth/wordsworth.prelude.all", 100)
   browning <- sampleText("../data/browning/browning.portuguese.all", 100)
   carroll <- sampleText("../data/carroll/carroll.alice.all", 100)

   # generate functional n-gram features
   cowper_v <- list()
   wordsworth_v <- list()
   browning_v <- list()
   carroll_v <- list()

   for (i in 1:length(cowper)) {
      cowper_v[[i]] <- functionalNgramGenerator(cowper[[i]], 2, numNgrams)
   }

   for (i in 1:length(wordsworth)) {
      wordsworth_v[[i]] <- functionalNgramGenerator(wordsworth[[i]], 2, numNgrams)
   }

   for (i in 1:length(browning)) {
      browning_v[[i]] <- functionalNgramGenerator(browning[[i]], 2, numNgrams)
   }

   for (i in 1:length(carroll)) {
      carroll_v[[i]] <- functionalNgramGenerator(carroll[[i]], 2, numNgrams)
   }

   # format training data
   cowper_x <- genFeatureVecs(cowper_v, functNgrams)
   cowper_y <- factor(seq(1,1, length.out=length(cowper)))

   # format testing data
   wordsworth_x <- genFeatureVecs(wordsworth_v, functNgrams)
   browning_x <- genFeatureVecs(browning_v, functNgrams)
   carroll_x <- genFeatureVecs(carroll_v, functNgrams)

   # train a 1-class SVM
   m <- svm(cowper_x, cowper_y, type="one-classification", nu=0.1, gamma=0.05)

   # make predictions
   predictions_cowper <- predict(m, cowper_x)
   predictions_wordsworth <- predict(m, wordsworth_x)
   predictions_browning <- predict(m, browning_x)
   predictions_carroll <- predict(m, carroll_x)

   # compute a few statistics
   cowper_fit <- length(grep("TRUE", predictions_cowper)) / length(predictions_cowper)
   wordsworth_proximity <- 1 - (length(grep("TRUE", predictions_wordsworth)) / length(predictions_wordsworth))
   browning_proximity <- 1 - (length(grep("TRUE", predictions_browning)) / length(predictions_browning))
   carroll_proximity <- 1 - (length(grep("TRUE", predictions_carroll)) / length(predictions_carroll))

   print(sprintf("Model's fit to Cowper: %f", cowper_fit))
   print(sprintf("Wordsworth's proximity to Cowper: %f", wordsworth_proximity))
   print(sprintf("Browning's proximity to Cowper: %f", browning_proximity))
   print(sprintf("Carroll's proximity to Cowper: %f", carroll_proximity))
}
