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

#The GLS model allows for an effect of time by not assuming normally distribusted residuals. There is a linear relationship somewhere, GLS takes that into account before creating a model.
#The LS model allows for an effect of time by using a linear explanatory variable in the regression. This way, the effect of time can be reflected in predictions.

