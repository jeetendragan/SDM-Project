###################################################################################
# Subset selection in the insurancedata
###################################################################################

install.packages("leaps")
library(leaps)
dim(insdata)
insdata<- data
names(insdata)
regfit.forward.full <- regsubsets(Response~. , data=insdata ,method="forward" , nvmax=116)
my_sum<-summary(regfit.forward.full)
my_sum
my_sum$bic
plot(my_sum$bic,xlab="Number of variables",ylab="Bic" ,type="l") # model size 35 
plot(my_sum$cp,xlab="Number of variables",ylab="Cp" ,type="l") # model size 35

regfit.exhaustive.full <- regsubsets(Response~. , data=insdata ,method="exhaustive" , nvmax=116,really.big=T)
my_sum1<-summary(regfit.exhaustive.full)
my_sum1
my_sum1$bic
plot(my_sum1$bic,xlab="Number of variables",ylab="Bic" ,type="l") # model size 35 
plot(my_sum1$cp,xlab="Number of variables",ylab="Cp" ,type="l") # model size 35



coefi<- coef(regfit.forward.full , id = 35)




#################################################################################
install.packages("rpart")
library(rpart)
insdata<- data

model.control<-rpart.control(minsplit=1000,xval=10,cp=0)
fit.insdata<- rpart(Response~. , data=insdata,method="class",control=model.control)
names(fit.insdata)
fit.insdata$cptable

plot(fit.insdata,uniform=T,compress=T)
text(fit.insdata,cex=.1045820)

insdata<- data
v =c(1:35);
v[1]= which(colnames(insdata)=="Product_Info_4")
v[2]= which(colnames(insdata)=="Ins_Age")
v[3]= which(colnames(insdata)=="BMI")
v[4]= which(colnames(insdata)=="Employment_Info_3")
v[5]= which(colnames(insdata)=="InsuredInfo_1")
v[6]= which(colnames(insdata)=="InsuredInfo_2")
v[7]= which(colnames(insdata)=="InsuredInfo_5")
v[8]= which(colnames(insdata)=="InsuredInfo_6")
v[9]= which(colnames(insdata)=="InsuredInfo_7")
v[10]= which(colnames(insdata)=="Insurance_History_1")
v[11]= which(colnames(insdata)=="Insurance_History_2")
v[12]= which(colnames(insdata)=="Insurance_History_3")
v[13]= which(colnames(insdata)=="Medical_History_3")
v[14]= which(colnames(insdata)=="Medical_History_4")
v[15]= which(colnames(insdata)=="Medical_History_5")
v[16]= which(colnames(insdata)=="Medical_History_7")
v[17]= which(colnames(insdata)=="Medical_History_11")
v[18]= which(colnames(insdata)=="Medical_History_13")
v[19]= which(colnames(insdata)=="Medical_History_17")
v[20]= which(colnames(insdata)=="Insurance_History_18")
v[21]= which(colnames(insdata)=="Insurance_History_19")
v[22]= which(colnames(insdata)=="Medical_History_20")
v[23]= which(colnames(insdata)=="Medical_History_23")
v[24]= which(colnames(insdata)=="Medical_History_27")
v[25]= which(colnames(insdata)=="Medical_History_28")
v[26]= which(colnames(insdata)=="Medical_History_30")
v[27]= which(colnames(insdata)=="Medical_History_31")
v[28]= which(colnames(insdata)=="Medical_History_33")
v[29]= which(colnames(insdata)=="Medical_History_35")
v[30]= which(colnames(insdata)=="Medical_History_40")
v[31]= which(colnames(insdata)=="Medical_Keyword_3")
v[32]= which(colnames(insdata)=="Medical_Keyword_9")
v[33]= which(colnames(insdata)=="Medical_Keyword_25")
v[34]= which(colnames(insdata)=="Medical_Keyword_38")
v[35]= which(colnames(insdata)=="Medical_Keyword_41")
install.packages("caret")
library(caret)
best_model_df = cbind(insdata[,v],insdata$Response)
names(best_model_df)[36] = "Response"
k=10
n <- nrow(best_model_df)
set.seed(123)
datay=best_model_df[,36] #response variable
library(MASS)

#partition the data into K subsets
f <- ceiling(n/k)
s <- sample(rep(1:k, f), n)  
#generate indices 1:10 and sample n of them  
# K fold cross-validated error

CV=NULL

for (i in 1:k) { #i=1
  test.index <- seq_len(n)[(s == i)] #test data
  train.index <- seq_len(n)[(s != i)] #training data
  instesty <- best_model_df[test.index, 36]
  model.control<-rpart.control(minsplit=1000,xval=10,cp=0)
  best_subset<- rpart(Response~. , data=best_model_df[train.index,],method="class",control=model.control)
  best_subset$cptable
  plot(best_subset,uniform=T,compress=T)
  text(best_subset,cex=.5)
  names(best_model_df)
  plot(best_subset$cptable[,4],main="Cp for model selection" ,ylab ="cp error")
  
  
  ###########################################
  # Prune the tree
  ###########################################
  min.cp= which.min(best_subset$cptable[,4])
  pruned.fit.ins=prune(best_subset,cp=best_subset$cptable[min.cp,1])
  
  ##########################################
  #Plot the pruned and full tree
  ##########################################
  
  plot(pruned.fit.ins,compress=T,main="Pruned tree for Insurance data")
  text(pruned.fit.ins,cex=.5)
  
  pred_train<-predict(pruned.fit.ins,newdata=insdata[test.index,],type="class")
  error = mean(pred_train != insdata[test.index,]$Response)
  CV=c(CV,error)
  #table(pred_train,insdata$Response)
  #confusionMatrix(data=as.factor(pred_train),reference=as.factor(insdata$Response))
  
}

mean(CV)
