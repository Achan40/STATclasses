options(warn=-1)
suppressPackageStartupMessages({
  library(faraway)
  library(ggplot2)
  library(splines)
  library(tidyverse)
})

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
AIC(fit)
BIC(fit)

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

#d
plot(x,y)