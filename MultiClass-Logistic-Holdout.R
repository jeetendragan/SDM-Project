library(caret)
require(nnet)

train = read.csv('Final-Train.csv')
set.seed(7)
totalRows = dim(train)[1]
trainIndices = sample( c(1:totalRows), totalRows*0.80)
finalTrain = train_imp[trainIndices, ]
finalTest = train_imp[-trainIndices, ]

hist(finalTrain$Response, main = "Class distribution(Training Data)")
hist(finalTest$Response, main = "Class distribution(Test Data)")

model <- multinom(Response ~ ., data = finalTrain)
prediction = predict(model, finalTest)
#predicted.classes <- model %>% predict(test.data)
confusionMatrix(data = as.factor(prediction),as.factor(finalTest$Response))
