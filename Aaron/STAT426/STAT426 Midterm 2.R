#Problem 1
setwd("/Users/chanm/Desktop")
kypho = data.frame(read.csv(file = "kyphosis.csv",header = T))
#a
kyphofit = glm(Kyphosis ~ Age, data = kypho, family = binomial)
summary(kyphofit)
#The model has the form: -0.545878+0.003379(age)
#Age has a pvalue of 0.567, greater than our alpha=0.05, we cannot reject H0: age is an insignificant variable. We can conclude that age is an insignificant variable at alpha=.05 when determining the presence of kyphosis.

#b
#conditional density plot of Kyphosis ~ Age model
kypho$Kyphosis = factor(kypho$Kyphosis)#need to turn into factor for cd plot to correctly work
cdplot(Kyphosis ~ Age, data = kypho, yaxlabels = c("K Absent(0)","K Present(1)"))
#from the chart, we can determine that kyphosis being present is at the highest dispersion at age 100, and begins to decline from then on. 

#create model with squared term 
kyphofit2 = glm(Kyphosis ~ Age + I(Age^2), data = kypho, family = binomial)
summary(kyphofit2)
#Age^2 resulted in a p-value of 0.0361 which is less than our alpha=0.05. We can reject H0: age^2 is an insignificant variable. Age^2 is a significant term when determining kyphosis.

kypho$Kyphosis = as.numeric(levels(kypho$Kyphosis))[kypho$Kyphosis]#convert back to numeric so that y-axis display correctly
plot(kypho$Age,kypho$Kyphosis,type = "p", main = "Kyphosis vs. Age", xlab = "Age", ylab = "Kyphosis")
x = sort(kypho$Age)
y = predict(kyphofit2, list(Age = x), type = "response")
#add the new model lines
lines(x,y, col = "Blue")
#Our model with the second order term tells us that as Age increases, the probability for kyphosis present increases until about age 100
#After Age 100, the probabilty for kyphosis present decreases almnost as sharply as it rose from age 0 to age 100.

#Problem 2
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
