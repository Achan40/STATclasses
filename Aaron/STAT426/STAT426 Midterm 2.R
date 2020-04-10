#Problem 2
setwd("/Users/chanm/Desktop")
heart = data.frame(read.csv(file = 'heart.csv',header = T))
#rate data is accounted by log(n)
heart$logTatR = log(heart$TatR)

#a
#Model Poisson with Rates log(uij) = alpha + B1(age) + B2(ValveType) where uij is the expected value of deaths
heartfit = glm(Deaths ~ Age + ValveType + offset(logTatR), family = poisson(link = "log"), data = heart)
summary(heartfit)
#Profile Likelihood confidence Interval
confint(heartfit)

#b
#G^2 using canonical Link (link = "log")
G2 = sum(residuals(heartfit)^2)
G2
1 - pchisq(deviance(heartfit), df.residual(heartfit))
#X^2 using canonical Link
X2 = sum(residuals(heartfit, type = "pearson")^2)
X2
1 - pchisq(X2, df.residual(heartfit))
#X^2 and G^2 values are fairly similar
#The model with the canonical link shows no lack of fit at the alpha =.05 level
#Do not reject H0: homogenoeus association, cannot say that there is lack of fit
#There is small lack of fit however, since pvalue of the X^2 and G^2 is close to our alpha. It could be due to interaction between the categorical predictor variables.

#G^2 using identity Link (link = "identity")
heartfit.idn = glm(Deaths ~ Age + ValveType + offset(logTatR), family = poisson(link = "identity"), data = heart)
G2 = sum(residuals(heartfit.idn)^2)
G2
1 - pchisq(deviance(heartfit.idn), df.residual(heartfit.idn))
#X^2 using identity Link
X2 = sum(residuals(heartfit.idn, type = "pearson")^2)
X2
1 - pchisq(X2, df.residual(heartfit.idn))
#X^2 and G^2 values are fairly similar
#The model with the identity link shows no lack of fit at the alpha =.05 level as well
#There is much less lack of fit for the identity model than the canonical link model as we can tell by the pvalues of the G^2 and X^2 tests
#Do not reject H0: homogenoeus association, cannot say that there is lack of fit
