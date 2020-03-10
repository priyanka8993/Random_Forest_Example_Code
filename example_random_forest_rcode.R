data("iris")

irisSubset = subset(iris,Species %in% c("setosa","virginica"))


idx1 = which(irisSubset[ ,5] == "setosa")
idx2 = which(irisSubset[ ,5] == "virginica")


##call function
source(/randomForestFuncs.R')
misClassification = rfClassificationFunc(irisSubset[,-5],as.character(irisSubset[,5]),idx1,idx2,5)


## summary statistic of all 100 sample folds misclassification
summary(unlist(misClassification))
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0       0       0       0       0       0 