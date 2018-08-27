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

npar=ncol(df1)-1
rf.err<-double(npar)
rf.predict.error=double(npar)
for(mtry in 1 : npar) 
{
  rf=randomForest(price ~ . , data = df1 , subset = train, mtry=mtry, ntree=2000, importance =TRUE)
  rf.err[mtry] = mean(rf$mse) #Error of all Trees fitted
  
  #Predictions on Test Set 
  pred<-predict(rf,test) 
  rf.predict.error[mtry]=mean(abs(test$price-pred)/pred)
  
  cat(mtry," ")
  
}

matplot(1:mtry ,rf.err, pch=19 , col=c("red"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")

#Test error
rf.predict.error



