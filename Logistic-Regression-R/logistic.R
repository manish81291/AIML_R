
setwd("C:\\Users\\DELL-PC\\Desktop\\R Classes\\WK5")
quality=read.csv("quality.csv")
head(quality)
str(quality)
table(quality$PoorCare)
#split data into train and test
# install.packages("caTools")
library(caTools)
set.seed(20)
spl=sample.split(quality$PoorCare,SplitRatio=0.70)
spl
train=subset(quality,spl==TRUE)
test=subset(quality,spl==FALSE)
nrow(train);nrow(test)
#logistic model
logreg=glm(PoorCare~OfficeVisits+Narcotics,data=train,family="binomial")
summary(logreg)
logreg1=glm(PoorCare~OfficeVisits+Narcotics+TotalVisits,data=train,family="binomial")
summary(logreg1)

x=glm(PoorCare~OfficeVisits,data=train,family="binomial")

summary(x)
# The null deviance shows how well the response is predicted
# by the model with nothing but an intercept.
# 
# The residual deviance shows how well the response 
# is predicted by the model when the predictors are included.

# # We can also use the residual deviance to 
# test whether the null hypothesis is true (i.e. Logistic regression 
#                                           model provides an adequate 
#                                           fit for the data). 
# This is possible because the deviance is given by the chi-squared value
# at a certain degrees of freedom. In order to test for significance,
# we can find out associated p-values using the below formula in R:
#   
#   p-value = 1 - pchisq(deviance, degrees of freedom)
#predict on train data
predtrain=predict(logreg,type="response")
predtrain
tapply(predtrain,train$PoorCare,mean)
#*******************************************
#predict on test data
predtest=predict(logreg,newdata=test,type="response")
predtest
#*********************************************
#Sensitivity or recall
#specificity
#accuracy
#precision
#**********************************************
#ROC CURVE
library(ROCR)
ROCR=prediction(predtrain,train$PoorCare)
perf=performance(ROCR,"tpr","fpr")
plot(perf,colorize=T,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
perf=performance(ROCR,"auc")

#**********************************************

# Hosmer-lemeshow test, it measures goodness for fit
# meansures difference b/w observed data(DV) & Model
# P value should be higher, then model is good
library(ResourceSelection)

#NULL : we have no significant difference between the model and the observed data
hoslem.test(train$PoorCare,fitted(logreg))





