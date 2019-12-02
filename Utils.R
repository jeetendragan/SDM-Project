drawStackedConfusionMat <- function(confusionMat){
  n = dim(confusionMat$table)[1]
  classificationMat = matrix(0, nrow = 2, ncol = n)
  classNames = c()
  for(i in 1:n){
    classNames = cbind(classNames, sprintf("C%i", i))
    classificationMat[1, i] = confusionMat$table[i, i]
    classificationMat[2, i] = sum(confusionMat$table[, i]) - confusionMat$table[i, i]
  }
  #classNames
  colnames(classificationMat) = classNames
  rownames(classificationMat) = c("Correct classifications", "Incorrect Classifications")
  #classificationMat
  barplot(data, 
          col = c("green","red"), 
          border="white",
          space=0.04, 
          font.axis=2, 
          xlab="Class", ylab = "Classification count")
}