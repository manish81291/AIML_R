setwd("C:\\Users\\DELL-PC\\Desktop\\R Classes\\WK4\\Linear Regression")
wine=read.csv("wine.csv")
str(wine)
summary(wine)
model1=lm(Price~ AGST,data=wine)
model1
names(model1)
summary(model1)
model1$coefficients
#SSE
SSE=sum(model1$residuals^2)

#addd new variable
model2=lm(Price~AGST+HarvestRain,data=wine)
summary(model2)
SSE2=sum(model2$residuals^2)
SSE2
#use all variables
model=lm(Price~.,data=wine)
summary(model)
SSE3=sum(model$residuals^2)
SSE3
#****************************************
final_model=lm(log(Price)~log(AGST)+HarvestRain+WinterRain+FrancePop,data=wine)
summary(final_model)
vif(final_model)
#plot linear regression
par(mfrow=c(2,2))
plot(final_model)
plot(wine$Age,wine$FrancePop)
pairs(wine)
outlierTest(final_model)

cutoff <- 4/((nrow(wine)-length(final_model$coefficients)-2)) 
plot(final_model, which=4, cook.levels=cutoff)
par(mfrow=c(1,1))

library(MASS)
sresid <- studres(final_model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

crPlots(final_model)
ncvTest(final_model)

library(car)
cor(wine)
cor(wine)[2,]
scatterplotMatrix(wine)
#********************************************************

winetest=read.csv("wine_test.csv")
test <- winetest[-2]
predictTest=predict(final_model,newdata=test)
exp(predictTest)
SSE=sum((winetest$Price-exp(predictTest))^2)
SST=sum((winetest$Price-mean(wine$Price))^2)
testR2=1-(SSE/SST)
testR2
#********************************************************
#use log transformation
x=c(10,13,15,34,44,22,35,89,123,43,12)
y=c(2.30,2.56,2.70,3.52,3.78,3.09,3.55,4.48,4.81,3.76,2.48)
plot(x,y)
abline(lm(y~x,type="l"))
#***********log transformation
plot(log(x),y)
abline(lm(y~log(x),type="l"))

data("mtcars")
str(mtcars)

model1 <- lm(mpg~.,data=mtcars)
summary(model1)

stepAIC(model1 )

