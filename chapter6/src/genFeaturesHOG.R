# genFeaturesHOG.H
# Trivial wrapper around genFeaturesHOG.py (avoids the need to call off to python from a shell)
#
# Cite as:
# W.J. Scheirer and C.W. Forstall, Quantitative Intertextuality, Springer, 2015.

genFeaturesHOG <- function(imagePathFile, outputFile) {
   cmd <- paste("python genFeaturesHOG.py ", imagePathFile, " ", outputFile)
   system(cmd)
}
