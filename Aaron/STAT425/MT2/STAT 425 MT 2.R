#Problem 1
library(faraway)
library(lmtest)
library(nlme)
library(splines)
data("cheddar")
mod1 = lm(taste ~ Acetic + H2S + Lactic, data = cheddar)

#a
#create time column
cheddar$time = c(rep(1:30))
plot(cheddar$time, mod1$residuals, xlab = "Time", ylab = "Residuals")
#There seems to be a slight negative correlation between residuals and time, however, the nature of that relationship is uncertain.
#It is necesarry to perform tests to determine if the nature of this relationship is significant or not.

#b
#DW test for autocorrelation
dwtest(mod1)
#Cannot reject H0: True autocorrelation is greater than zero, at the alpha = .05 level, the DW test suggests errors are correlated for the linear model
#Create a GLS model, but allowing for an AR(1) correlation among the errors
mod1.gls = gls(taste ~ Acetic + H2S + Lactic, correlation = corARMA(p=1),data = cheddar)
summary(mod1.gls)
intervals(mod1.gls, which = "var-cov")
#From the confidence interval, we can see that the autocorrelation parameter is not significantly different from zero.
#The confidence interval for phi does include zero.
#After fitting a GLS model we can see that there is no significant evidence for correlation among the errors for that model.

#c
mod1.ls = lm(taste~., data = cheddar)
summary(mod1.ls)
#Our coefficient for time seems to have a negative relationship with the response.

#d
#The GLS model allows for an effect of time by not assuming normally distribusted residuals. There is a linear relationship somewhere, GLS takes that into account before creating a model.
#The LS model allows for an effect of time by using a linear explanatory variable in the regression. This way, the effect of time can be reflected in predictions.

#Problem 2
data("cars")

#a
plot(dist ~ speed, xlab="Speed", ylab="Distance", data = cars)

#b
mod2 = lm(dist ~ speed, data = cars)
abline(mod2, col="Blue")

#c
mod2.quad = lm(dist ~ poly(speed,2,raw=TRUE), data = cars)
x = sort(cars$speed)
y = mod2.quad$fitted.values[order(cars$speed)]
lines(x,y, col = "Red")

#d
mod2.sqrt = lm(sqrt(dist) ~ speed, data = cars)
summary(mod2.sqrt)
z = mod2.sqrt$fitted.values[order(cars$speed)]
lines(x,z, col = "Green")

#e
plot(dist ~ speed, xlab="Speed", ylab="Distance", data = cars)
#cubic spline fit with 6 degrees of freedom
mod2.spline = lm(dist ~ ns(speed, df = 5), data = cars)
lines(spline(cars$speed, predict(mod2.spline)), col = "Orange", lty = 1)
#Compared to the previous fits, the spline fit seems to follow the data better than the square root fit. 
#The model with the square root of the response barely followed the data at all. Least accurate out of all of our models.
#A Spline fit seemed to be much better, but it is also more flexible than the linear or quadratic models, which may mean the spline model is less accurate.
