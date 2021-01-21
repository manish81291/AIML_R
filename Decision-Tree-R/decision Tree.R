data("kyphosis")
setwd("C:\\Users\\Ramakrishna\\Desktop\\R\\22-04-2017 decision Tree & RF")
library(rpart)
data("iris")
head(iris)
levels(iris)
summary(iris)
install.packages("gam")
library(gam)
fit=rpart(iris,data=iris)
summary(fit)
iris
0.13773*0.22893
plot(fit)
pred=predict(fit,type="class")
class.pred=table(x,iris$Species)
x=predict(fit)
class.pred
fit1=rpart(Species~.,data=iris)
?rpart
summary(fit1)
pred= (predict(fit1,iris))
nrow(iris$Species)
head(iris)
nrow(iris)
iris$Species
levels(iris$Species)
table(pred,iris)
