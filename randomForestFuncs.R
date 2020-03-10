##############################################################################
## Title : Functions for performing 2 class classification using random forest
## Date : 23rd March 2017
## Mod Date : 29th Jan 2018
## Author : Priyanka Chakraborty
###############################################################################

library(randomForest)



##generate sample folds keeping proportion of both the classes same as the dataset
generateFolds = function(class1indx,class2indx,foldNum){
  
  sampleFoldList = list()
  
  ##equal probability for all indices
  fold1 = sample(1:foldNum,length(class1indx),replace = TRUE,prob = rep(1/foldNum, foldNum))
  
  iterate = TRUE
  fold2 = NULL
  
  while(iterate){
    fold2 = sample(1:foldNum,length(class2indx),replace = TRUE,prob = rep(1/foldNum, foldNum))
    if(length(unique(fold2)) == foldNum) iterate = FALSE  
  }
  
  for(i in 1:foldNum){
      
      bucketNum1 = which(fold1 == i)
      bucketNum2 = which(fold2 == i)
      sampleFoldList[[i]] = union(class1indx[bucketNum1],class2indx[bucketNum2])
      
  }
  
  return(sampleFoldList)

}


##use sampleFolds and create training and test sets
##return test set, training set and class of corresponding samples
createSets = function(sampleFolds,dataMat,sampleGrps){
  
    testSet = dataMat[sampleFolds, ]
    trainingSet = dataMat[-sampleFolds, ]
    #colnames(testSet) = colnames(dataMat)[sampleFolds]
    #colnames(trainingSet) = colnames(dataMat)[-sampleFolds]
    
    testSetClass = as.factor(sampleGrps[sampleFolds])
    trainingSetClass = as.factor(sampleGrps[-sampleFolds])
    
    return(list(testSet,trainingSet,testSetClass,trainingSetClass))

}

  

randomForestFunc = function(testSet, trainingSet, testClass, trainingClass){
  
  rfResult = randomForest(x=trainingSet,y=trainingClass, xtest = testSet, ytest=testClass)
  return(rfResult)
  
}


calMisClassification = function(rfConfMat){
    
  misClassification = (rfConfMat[1,2] + rfConfMat[2,1])/sum(rfConfMat)
  return(misClassification)  
}

#wrapper function for performing classification using random forest
##arguments = data Matrix, sample class description(could be binary), indices of each class,number of folds 
rfClassificationFunc = function(dataMat,sampleClassDes,classIdx1,classIdx2,foldNum){
  
  sampleFold = misClassificationRate = rfInput =  list()
  rfSamplePred = rep(0,nrow(dataMat))
  names(rfSamplePred) = rownames(dataMat)
  
  for(i in 1:100){ ##generate sample folds 100 times
      
    sampleFold = generateFolds(classIdx1,classIdx2,foldNum)
    confMat = matrix(0,nrow = 2, ncol = 2,byrow = FALSE)
    
    
    for(j in 1:foldNum){
        print(j)
        rfInput = createSets(sampleFold[[j]],dataMat,sampleClassDes)
        rf = randomForestFunc(rfInput[[1]],rfInput[[2]],rfInput[[3]],rfInput[[4]])
        rfSamplePred[sampleFold[[j]]] = as.numeric(as.character(rf$test$predicted))
        
        confMat = rf$test$confusion[1:2,1:2]+confMat
    }
    misClassificationRate[[i]] = calMisClassification(confMat)
    
  }
  
  ##return misclassification for 100 sampleFolds
  return(misClassificationRate)
  
}