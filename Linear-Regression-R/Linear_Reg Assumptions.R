data("mtcars")
str(mtcars)

model <- lm((mpg)~ wt + (qsec) + am,data=mtcars)
summary(model)

#Residual Vs fitted: Red line shows if linearity assuption is met,
#Red line is should be fairly flat
# Variance also can be looked into cloud of points
plot(model,1)

# Normal Q-Q plot , points should follow on a diagonal line
# Nomality assumption plot
plot(model,2)

## Variance / homosscadisticity
plot(model,3)

## The fourth plot helps us find influential cases
#If you notice some of your data points cross that distance line, 
#you're not so good/ you have influential data points.
plot(model,4)
plot(model,5)
plot(model)

model <- lm(log(mpg)~ wt + (qsec) + am,data=mtcars[c(1:16,18:32),])
summary(model)


#To check Outliers in the model
#null that there are no outliers
#This function runs a Bonferonni adjusted outlier test.
#We reject the null that there are no outliers (p < .05)

# Observation 202 is the most extreme observation in our data, 
#but it isn't an outlier, 
#as the null cannot be rejected (adjusted p > 0.05).
outlierTest(model)

# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(model$coefficients)-2)) 
plot(model, which=4, cook.levels=cutoff)

library(MASS)

# Normality test :  plots the distribution of residuals

# Assumptions: NULL : samples are drawn from a normal distribution
shapiro.test(model$residuals)


sresid <- studres(model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

## Evaluate homoscedasticity
#the null states that there is constant variance.
ncvTest(model)

#Testing the Linearity Assumption: crplots
crPlots(model)

# Independence test, NULL : There is a independence in the errors
durbinWatsonTest(model)

library(gvlma)
 gvlma(model)

library(sjPlot)
sjp.lm(model,type="ma")


# Changed the variable transform. 
model <- lm(log(mpg)~ wt + (qsec)+am ,data=mtcars)
summary(model)

gvlma(model)
