# wsvm.R
# Wrappers around the w-svm's svm-train and svm-predict
# wsvmScaleTrain saves scaling parameters to scaleParams, and scaled feature vectors
#   to scaledFile
# wsvmScaleTest saves scaled feature vectors to scaledFile
# wsvmTrain saves a model to modelFile
# wsvmPredict returns a list of probability scores

wsvmScaleTrain <- function(trainingFile, scaleParams, scaledFile) {
   cmd <- paste("./libsvm-openset/svm-scale -s ", scaleParams, " ", trainingFile, " > ", scaledFile)
   system(cmd)
}

wsvmScaleTest <- function(testingFile, scaleParams, scaledFile) {
   cmd <- paste("./libsvm-openset/svm-scale -r ", scaleParams, " ", testingFile, " > ", scaledFile)
   system(cmd) 
}

wsvmTrain <- function(trainingFile, modelFile, oneclassC=-1, binaryC=-1, oneclassGamma=-1, binaryGamma=-1) {
   cmd <- "./libsvm-openset/svm-train -s 8 "

   if (oneclassC != -1) {
      cmd <- paste(cmd, "-o ", oneclassC)
   }

   if (binaryC != -1) {
      cmd <- paste(cmd, "-c ", binaryC)
   }

   if (oneclassGamma != -1) {
      cmd <- paste(cmd, "-a ", oneclassGamma)
   }

   if (binaryGamma != -1) {
      cmd <- paste(cmd, "-g ", binaryGamma)
   }

   cmd <- paste(cmd, trainingFile, " ", modelFile)

   system(cmd)
}

wsvmPredict <- function(testingFile, modelFile, oneclassThresh=0.001, binaryThresh=0.5) {
   cmd <- paste("./libsvm-openset/svm-predict -o -P ", binaryThresh, " -C ", oneclassThresh, " ", testingFile, " ", modelFile, " out.tmp")
   system(cmd)
   scores <- readLines("out.tmp")
   file.remove("out.tmp")
   return(scores)
}
