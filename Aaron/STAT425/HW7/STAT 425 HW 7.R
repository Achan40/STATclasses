options(warn=-1)
suppressPackageStartupMessages({
  library(faraway)
  library(ggplot2)
  library(splines)
  library(leaps)
  library(tidyverse)
})

#Problem 1
data("prostate")

#a
mod1 = lm(lpsa ~ ., data = prostate)
back.elm = step(mod1, lpsa ~ ., direction = "backward")
back.elm
#The best model is: (lpsa~lcavol+lweight+age+lbph+svi) when using backward elimination.

#b
msize = 2:9
n = nrow(prostate)
rsub = regsubsets(lpsa ~ ., data = prostate)
rs = summary(rsub)

Bic = n*log(rs$rss/n) + msize*log(n)
Aic = n*log(rs$rss/n) + 2*msize

min(Bic)
min(Aic)
#AIC and BIC select different models. However, the lowest AIC occurs with the model that has five predictors: (lpsa~lcavol+lweight+age+lbph+svi), the same model as in part A.
#Although BIC provides us with a model that is less flexible, our backward elimination step provided us with the best model which also happened to have the lowest AIC.
#Therefore, the best model should be (lpsa~lcavol+lweight+age+lbph+svi) in this case.

#c
plot(msize, rs$adjr2, main = "Parameters vs. Adjusted R-Square", xlab = "Parameters", ylab = "Adjusted R-Square")
which.max(rs$adjr2)
max(rs$adjr2)
#Empirically, Adj R^2 is at its highest when at 7 parameters even though the plot seems to show adj R^2 highest at 8 parameters.

#d
rs$cp
#This does not account for intercept
rs$cp[1:8]+coef(mod1)[1]
#With the intercept in mind, Mallows Cp seems to be the best with 6 predictors in the model.

#Problem 2
#a
set.seed(130)
fun = function(x) sin(2*pi*x^3)^3
x = seq(0,1,by=0.01)
y = fun(x) + 0.1*rnorm(101)

#spline 
fit = lm(y ~ bs(x,12))
fhat = fit$fitted
fsum = summary(fit)
fsum

plot(x,y)
#spline fit line
lines(x,fhat,col = "Red")

#b
BIC(fit)
AIC(fit)

#c
#function to create list of BIC values
calc_mod_BIC = function(n) {
  mod = lm(y ~ bs(x,n))
  BIC(mod)
}

#list of BIC values for knots 3:20
BIC.values = map_dbl(3:20, calc_mod_BIC)

#function to create list of AIC values
calc_mod_AIC = function(n) {
  mod = lm(y ~ bs(x,n))
  AIC(mod)
}

#list of AIC values for knots 3:20
AIC.values = map_dbl(3:20, calc_mod_AIC)

knots = 3:20
#create data frame so we can use gg plot
dat = data.frame(knots, BIC.values, AIC.values)

#instead of reorganizing data, simply add manual colors to legend
colors = c("BIC" = "blue", "AIC" = "green")
ggplot(data = dat, aes(x = knots)) +
  geom_point(aes(y = BIC.values, color = "BIC")) +
  geom_point(aes(y = AIC.values, color = "AIC")) +
  labs(x = "Knots", y = "Value", color = "Legend") +
  scale_color_manual(values = colors)

#checking for best model
min(BIC.values)#knots = 14
min(AIC.values)#knots = 19
#In this case, we would probably use the model with knots = 14, corresponding to the lowest BIC value. This is because knots = 19 (where AIC is lowest) would be a very flexible model.
#It would not do us well for predictions to choose a model that is too flexible, as our accuracy could be lower than a less flexible model.

#d
#selected model is model with knots = 14
fitbest = lm(y ~ bs(x,14))
fhatbest = fit$fitted
summary(fitbest)

plot(x,y)
#spline fit line
lines(x,fhatbest,col = "Green")
