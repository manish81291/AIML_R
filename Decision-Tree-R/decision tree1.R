library(MASS)
head(birthwt)
str(birthwt)
tail(birthwt)
table(birthwt$low)
library(rpart)9i
install.packages("rpart.plot")
library("rpart.plot")
treee=rpart(low~.-bwt,data=birthwt)
treee
prp(treee)
preddr=predict(treee,methon="class")
preddr
table(birthwt$low,preddr)
(146)/(146+43)
prop.table(table(birthwt$low))
130/189
preddr1=predict(treee)
preddr1
library(ROCR)
ROCRpred=prediction(preddr1[,2],birthwt$low)
summary(ROCRpred)
ROCRpref=performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=T,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
a=table(birthwt$low,preddr1[,2]<0.3)
a
a[1,2]+a[2,1]

a[3][3]
View(a)
(122+23)/(122+23+8+36)
(118+28)/(118+28+31+12)
(112+31)/(112+31+18+28)
for(x in 0.4:0.9){
a=table(birthwt$low,preddr1[,2]<x)
q=(a[1,2]+a[2,1])
accuracy=q/(sum(a))
plot(x,accuracy)
}

titan=read.csv("train.csv")
View(titan)
str(titan)
titan$Survived=as.factor(titan$Survived)
titan$Survived=as.factor(titan$Survived)
DRtitan=rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked,data=titan,method="class")
prp(DRtitan)
DRtitan1=rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked,data=titan,method="class",minbucket=8)
prp(DRtitan1)
preddr=predict(DRtitan,type="class")
preddr
table(titan$Survived,preddr)
testtitan=read.csv("test.csv")
pred=predict(DRtitan,newdata=testtitan,type="class")
mysubmit=testtitan
mysubmit$Survived=0
mysubmit
mysubmit=mysubmit[,c(1,12)]
mysubmit=testtitan$PassengerId
mysubmit$Survived=pred
write.csv(mysubmit,"DRfirstsub.csv",row.names = F)
mysubmit

DRtitan1=rpart(Survived~Pclass+Sex+Age+SibSp+Embarked,data=titan,method="class")
preddr1=predict(DRtitan1,type="class")

pred1=predict(DRtitan1,newdata=testtitan,type="class")
mysubmit1=testtitan
mysubmit1$Survived=0
mysubmit1
mysubmit1=mysubmit1[,c(1,12)]
mysubmit1=testtitan$PassengerId
mysubmit1$Survived=pred1
write.csv(mysubmit,"DRsecondsub.csv",row.names = F)

df=DRtitan
transform(df, test=do.call(rbind, strsplit(Name, ',', fixed=TRUE)))

?strsplit

trk=strsplit("rama|krishna",split = '|',fixed = T)
class(trk)
trk[[1]][2]

y=as.character(titan$Name)
x=strsplit(y,split = '[,.]',fixed = T)
class(x)
z=as.data.frame(x)
x
View(x)
for(i in 1:891){
  z=x[[i]][2]
}
z

?write.csv
summary(birthwt)
str(birthwt)
birthwt$low=as.factor(birthwt$low)
birthwt$race=as.factor(birthwt$race)
birthwt$smoke=as.factor(birthwt$smoke)
birthwt$ui=as.factor(birthwt$ui)
birthwt$ht=as.factor(birthwt$ht)
str(birthwt)

?birthwt()
setwd("C:\\Users\\Ramakrishna\\Desktop\\R\\29-04-2017 Decision trees")
classification=glm(low~.-bwt,data=birthwt,family=binomial)
summary(classification)
predict=predict(classification,type = "response")
summary(predict)
table(birthwt$low,predict<=0.5)
140/186
library(ROCR)
ROCRpred=prediction(predict,birthwt$low)
summary(ROCRpred)
ROCRpref=performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=T,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
