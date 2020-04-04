library(faraway)
library(ggplot2)
library(car)
library(splines)
library(lmtest)
#Problem 1
#a
data(aatemp)
ggplot(aatemp, aes(x = year, y = temp))+
  geom_point() +
  geom_smooth(method = "lm", se = F)
#graphically, there seems to be a linear trend, we can also fit a linear model to detemine the trend
mod1 = lm(temp ~ year, data = aatemp)
summary(mod1)
#small R^2, weak linear trend

#b
#create a lag model to predict current values of a response based on both the current values of a predictor and the lagged values of the predictor.
plot(aatemp$temp[-1],aatemp$temp[-length(aatemp$temp)])#plot of temperature of some year vs plot of temperature of previous year
lag.mod1 = lm(temp[-1]~temp[-length(temp)], data = aatemp)
abline(lag.mod1)
#There does seem to be a positive trend, which could indicate correlation.
summary(lag.mod1)
durbinWatsonTest(lag.mod1)
#Using the durbin watson test we get a p-value of .652, the linear association is not significant.

#c
mod.poly.10 = lm(temp ~ poly(year,10), data = aatemp)
summary(mod.poly.10)
mod.poly.6 = lm(temp ~ poly(year,6), data = aatemp)
summary(mod.poly.6)
mod.poly.5 = lm(temp ~ poly(year,5), data = aatemp)
summary(mod.poly.5)
mod.poly.4 = lm(temp ~ poly(year,4), data = aatemp)
summary(mod.poly.4)
mod.poly.3 = lm(temp ~ poly(year,3), data = aatemp)
summary(mod.poly.3)
#The first significant highes degree term is when poly(year,3)
plot(aatemp$year, aatemp$temp, pch = 1, xlab = "Year", ylab = "Temp")
lines(aatemp$year, fitted(mod.poly.3), lwd = 1)
#Predicted temperature in 2020
predict(mod.poly.3,data.frame(year = 2020))

#d
#basis funcitons
fit1 = lm(temp~bs(year,df = 4), data = aatemp)
fit2 = lm(temp~bs(year,df = 5), data = aatemp)
fit3 = lm(temp~bs(year,df = 6), data = aatemp)
fit4 = lm(temp~bs(year,df = 7), data = aatemp)
fit5 = lm(temp~bs(year,df = 8), data = aatemp)
fit6 = lm(temp~bs(year,df = 9), data = aatemp)

plot(aatemp$year, aatemp$temp, pch = 1, xlab = "Year", ylab = "Temp")
lines(spline(aatemp$year, predict(fit1)), lty = 1)
lines(spline(aatemp$year, predict(fit2)), lty = 2)
lines(spline(aatemp$year, predict(fit3)), lty = 3)
lines(spline(aatemp$year, predict(fit4)), lty = 4)
lines(spline(aatemp$year, predict(fit5)), lty = 5)
lines(spline(aatemp$year, predict(fit6)), lty = 6)
lines(aatemp$year, fitted(mod.poly.3), col = "red",lwd = 1) #selected polynomial model
#as the df for the basis functions increase, the models become more flexible. 
#These models seem to fit better to the past data but may not have validity as good as the selected polynomical model for future predictions.

#Problem 2
data(infmort)
infmort = infmort[complete.cases(infmort),]#remove rows w NA observations
#full model
all.infmort = glm(mortality ~ ., data = infmort)
summary(all.infmort)

#lisiting potential influential observations
all.infmort.hat = hatvalues(all.infmort)
#hat values > 2*(k+1)/n
all.infmort.hat.val = which(all.infmort.hat > (2*(4+1)/nrow(infmort))) 
#list of high leverage points with values
all.infmort.hatsorted = all.infmort.hat[all.infmort.hat.val];sort(all.infmort.hatsorted,decreasing = T)[1:5]
#graph
lev = influence(all.infmort)$hat
halfnorm(lev, 4, labs = row.names(infmort), ylab="Leverages")

#Cooks distance to check for outliers
cook = cooks.distance(all.infmort)
halfnorm(cook, 1,labs = row.names(infmort), ylab = "Cook's distance")
max(cook)
#Saudi_Arabia is the largest outlier, with a Cook's Distance value of approx. 1.373, no other observaiton comes close.

summary(all.infmort)
#The categorical variables for region indicate the effect on infant mortality depending on region (if in Africa, all others have value zero).
#Being in America has the largest on infant mortality, decreasing it by an average of 8.365e+01.
#The intercept starts at 215.2, not very useful for interpretation on its own.
#income is a numeric variable, for every increase in a unit of income per country, infant mortaility decreases by an average of 5.290e-03, makes sense as the richer a country is, the less we expect to see infants die.
#a 1 for oil exports indicating "yes the country has exports" seems to have a large effect on infant mortality, but that may be because of our outlier Saudia Arabia.
#Removing Saudia Arabia data from our data may make our model fit better in general as The Cooks distance value is enourmous.

#Problem 3
data(pulp)
mod.pulp = aov(bright~operator, data = pulp) #Fitting One-way ANOVA
summary(mod.pulp)
#At the alpha=.05 level, we can reject the H0 that there is no difference between the operators. Concluding that there is a diference among the operators

#Use Tukey HSD test to determine the nature of the differences between operators
TukeyHSD(mod.pulp)
#pairs d-b are significant since it's pvalue of .037 is less than alpha=.05, c-b is a close followup as well, but not within our level of alpha.