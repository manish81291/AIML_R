setwd("C:\\Users\\Ramakrishna\\Desktop\\R\\30-04-2017 Decision Tree, overfitting,Random Forest")
library(MASS)
View(birthwt)
titantrain=read.csv("train.csv")
titantrain$Pclass=as.factor(titantrain$Pclass)
titantrain$Embarked=as.factor(titantrain$Embarked)
library("caTools")
set.seed(100)
spl=sample.split(titantrain$Survived,SplitRatio = 0.6)
table(spl)
titantrain1=subset(titantrain,spl==T)
titantest=subset(titantrain,spl==F)
View(titantrain1)
View(titantest)
dim(titantrain1)
dim(titantest)
DRtitan=rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked,data=titantrain1,method="class",minbucket=10)
prp(DRtitan)
predtrain=predict(DRtitan,type="class")
predtest=predict(DRtitan,newdata=titantest,type="class")
a=table(titantest$Survived,predtest)
`b=table(titantrain1$Survived,predtrain)
(a[1,1]+a[2,2])/357
(b[1,1]+b[2,2])/534
install.packages("randomForest")
library("randomForest")
rf=randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked,data=titantrain1,nodesize=20,ntree=500)
?birthwt
str(birthwt)
rf=randomForest(low~-age+lwt+race+smoke+ptl+ht+ui+ftv,data=birthwt,nodesize=30,ntree=500)
predtrain=predict(rf)
a=table(birthwt$low,predtrain)
birthwt$low <- as.factor(birthwt$low)
(a[1,1]+a[2,2])/189
install.packages("caret")
install.packages("e1071")
library("ggplot2")
library("caret")
library("e1071")
numfolds=trainControl(method="cv",number=5)
cpGird=expand.grid(.cp=seq(0.01,0.5,0.001))
tr=train(low~.-bwt,data=birthwt,method="rpart",trControl=numfolds,tuneGrid=cpGird)
tr
DT=rpart(low~.-bwt,data=birthwt)
printcp(DT)
plotcp(DT)
pfit<- prune(DT, cp=   DT$cptable[which.min(DT$cptable[,"xerror"]),"CP"])
plotcp(pfit)
printcp(pfit)
prp(pfit)
preddr=predict(DT,type="class")
b=table(birthwt$low,preddr)
(b[1,1]+b[2,2])/189

DTwithCP=rpart(low~.-bwt,data=birthwt,cp=0.5)
preddrwithCP=predict(DTwithCP,type="class")
a=table(birthwt$low,preddrwithCP)
(a[1,1]+a[2,2])/189


rf=randomForest(low~.-bwt,data=birthwt,ntree=500,cp=0.5)
predtrain=predict(rf)
a=table(birthwt$low,predtrain)
(a[1,1]+a[2,2])/189

titanKfoldCVtrain=read.csv("train.csv")
View(titanKfoldCVtrain)
str(titanKfoldCVtrain)
titanKfoldCVtrain$Survived=as.factor(titanKfoldCVtrain$Survived)
numfolds=trainControl(method="cv",number=5)
cpGird=expand.grid(.cp=seq(0.01,0.5,0.001))
colSums(is.na(titanKfoldCVtrain))
tr=train(Survived~Pclass+Sex+SibSp+Parch,data=titanKfoldCVtrain,method="rpart",trControl=numfolds,tuneGrid=cpGird)
tr

DTwithCP=rpart(Survived~Pclass+Sex+SibSp+Parch+Age,data=titanKfoldCVtrain,cp=0.018)
preddrwithCP=predict(DTwithCP,type="class")
a=table(titanKfoldCVtrain$Survived,preddrwithCP)
a
(468+248)/(468+248+81+94)
View(test)
preddrwithCP=predict(DTwithCP,newdata=test,type="class")
preddrwithCP
table(test$Survived,preddrwithCP)
Myfirstsub=test$PassengerId
myfirstsub$Survived=NULL
mysubmit=test
mysubmit$Survived=0
mysubmit
mysubmit=mysubmit[,c(1,12)]
mysubmit=test$PassengerId
mysubmit$Survived=preddrwithCP
write.csv(mysubmit,"DRfirstsub.csv",row.names = F)
