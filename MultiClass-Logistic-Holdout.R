library(caret)
train = read.csv('Final-Train.csv')
set.seed(1)
totalRows = dim(train)[1]
trainIndices = sample( c(1:totalRows), totalRows*0.75)
finalTrain = train_imp[trainIndices, ]
finalTest = train_imp[-trainIndices, ]

model <- multinom(Response ~ ., data = finalTrain)
prediction = predict(model, finalTest)
#predicted.classes <- model %>% predict(test.data)
confusionMatrix(data = as.factor(prediction),as.factor(finalTest$Response))