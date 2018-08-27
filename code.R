rm(list=ls())
library(randomForest)
library(VIM)
library(party)
library(corrplot)
set.seed(50)
df=read.csv("C:/Users/robapp/Downloads/Auto1-DS-TestData.csv",  sep=",", header=T, na.string="?")


#data missing ispection
summary(df)
dfna=ifelse(is.na(df),1,0)
dfna1=df[which(rowSums(dfna)==0),]
#number of row without missing data
nrow(df[which(rowSums(dfna)==0),])
#exclusion of the possibility to delete the na row because is the 25.5% of the dataset

#KNN application to imputate the data missing
df1=kNN(df)[,1:26]

#data train creation
train=sample(1:nrow(df1),0.7*nrow(df1))
test=df1[-train,]

#number of parameter
npar=ncol(df1)-1

#defining output element
rf.err<-double(npar)
rf.predict.error=double(npar)
#random forest OOB
for(mtry in 1 : npar) 
{
  rf=randomForest(price ~ . , data = df1 , subset = train, mtry=mtry, ntree=2000, importance =TRUE)
  rf.err[mtry] = mean(rf$mse) #Error of all Trees fitted
 
  cat(mtry," ")
  
}

#As we can see, the lower rate is with 9 predictor, so we predict with this model
matplot(1:mtry ,rf.err, pch=19 , col=c("red"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")

#model with 9 predictor
rf1=randomForest(price ~ . , data = df1 , subset = train, mtry= match(min(rf.err),rf.err), ntree=2000, importance =TRUE)

plot(rf1)
rf1$importance
#Test error
pred<-predict(rf1,test) 
print(paste0("Random forest prediction error rate= ", mean(abs(test$price-pred)/pred)))
