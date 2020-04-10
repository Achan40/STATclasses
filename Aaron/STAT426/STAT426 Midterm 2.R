#Problem 2
setwd("/Users/chanm/Desktop")
heart = data.frame(read.csv(file = 'heart.csv',header = T))
#rate data is accounted by log(n)
heart$logTatR = log(heart$TatR)

#Model Poisson with Rates log(uij) = alpha + B1(age) + B2(ValveType) where uij is the expected value of deaths
heartfit = glm(Deaths ~ Age + ValveType + offset(logTatR), family = poisson(link = "log"), data = heart)
summary(heartfit)
#Profile Likelihood confidence Interval
confint(heartfit)

#G^2 using canonical Link (link = "log")
heartfit.can = glm(rate ~ Age + ValveType, data = heart, family=gaussian(link = "log"))
#X^2 using canonical Link

#G^2 using identity Link (link = "identity")
heartfit.idn = glm(rate ~ Age + ValveType, data = heart, family=gaussian(link = "identity"))
#X^2 using identity Link