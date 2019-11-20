rm(list = ls())
train = read.csv('train.csv')
head(train)
sapply(train, function(x) sum(is.na(x)))
sapply(train_imp, function(x) sum(is.na(x)))
which(is.na(train$Employment_Info_4))
summary(train$Employment_Info_4)
cor(train$Response,train$Employment_Info_4)
rm(train_imp)
#removing the columns with NAs greater than 6000(more than 10% of data)
train_imp=train
x = list()
count =1
for (i in 1:128){
  if (sum(is.na(train[,i]))>6000){
    x[count] = i
    count = count+1
  } 
}
x = unlist(x)
train_imp = train[,-x]
train_imp = na.omit(train_imp)
str(train_imp$Product_Info_2)
train_imp$Product_Info_2 = as.numeric(train_imp$Product_Info_2)

write.csv(train_imp, "Final-Train.csv")
