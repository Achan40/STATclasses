setwd("/Users/chanm/Downloads")
sore=data.frame(read.csv(file="sore.csv",header=T))

#Problem 
#a
#Backward Elimination
modA = glm(Y ~ ., data = sore,family = binomial)
summary(modA)
modA$aic
drop1(modA, test = "Chisq")
#Eliminating either variables will cause AIC to go up (not desireable).
#AIC goes from 36.138 (all variables) to 46.578 if D is removed, and from 36.138 to 37.651 if T is removed.
#The best model is the one that includes both variables
#The LRT gives us a value of 12.4396 if we were to eliminate D, and a value of 3.5134 if we were to eliminate T.

#b
modB = glm(Y ~ D, data = sore, family = binomial)
modB2 = glm(Y ~ T, data = sore, family = binomial)

#Correlation Measures
cor(sore$Y, fitted(modA))
cor(sore$Y, fitted(modB))
cor(sore$Y, fitted(modB2))
#modA has the best correlation measure

#Likelihood Measures
mod0 = glm(Y ~ 1, data = sore, family = binomial)
as.numeric((logLik(modA) - logLik(mod0)) / (0 - logLik(mod0)))
as.numeric((logLik(modB) - logLik(mod0)) / (0 - logLik(mod0)))
as.numeric((logLik(modB2) - logLik(mod0)) / (0 - logLik(mod0)))
#Mod A has the highest likelihood measure as well

#Part C: Classification Tables
p0 = 0.5
tab1 = table(y = sore$Y, yhat = as.numeric(fitted(modA) > p0))
tab1[2,2]/(tab1[2,2]+tab1[2,1])#sensitivity
tab1[1,1]/(tab1[1,1]+tab1[1,2])#specificity

tab2 = table(y=sore$Y,yhat=as.numeric(fitted(modB)>p0))
tab2[2,2]/(tab2[2,2]+tab2[2,1])#sensitivity
tab2[1,1]/(tab2[1,1]+tab2[1,2])#specificity

tab3 = table(y=sore$Y,yhat=as.numeric(fitted(modB2)>p0))
tab3[2,2]/(tab3[2,2]+tab3[2,1])#sensitivity
tab3[1,1]/(tab3[1,1]+tab3[1,2])#specificity

pi0 = mean(sore$Y)
tab1a = table(y = sore$Y,yhat=as.numeric(fitted(modA)>pi0))
tab1a[2,2]/(tab1a[2,2]+tab1a[2,1])#sensitivity
tab1a[1,1]/(tab1a[1,1]+tab1a[1,2])#specificity

tab2a = table(y=sore$Y,yhat=as.numeric(fitted(modB)>pi0))
tab2a[2,2]/(tab2a[2,2]+tab2a[2,1])#sensitivity
tab2a[1,1]/(tab2a[1,1]+tab2a[1,2])#specificity

tab3a = table(y=sore$Y,yhat=as.numeric(fitted(modB2)>pi0))
tab3a[2,2]/(tab3a[2,2]+tab3a[2,1])#sensitivity
tab3a[1,1]/(tab3a[1,1]+tab3a[1,2])#specificity

#Sensitivity was always the highest for model 1
#Specificity was the same every time except for when pi0 = mu(Y) while using Model 1

#d
#ROC Curve
pihat = fitted(modA)
f.neg = c(0, cumsum(tapply(sore$Y,pihat,sum)))
t.neg = c(0, cumsum(table(pihat))) - f.neg
plot(1 - t.neg / max(t.neg),1 - f.neg / max(f.neg),type="l",main="ROC Curve",xlab="1-Specificity",ylab="Sensitivity",asp=1)
abline(a = 0,b = 1,lty = 2,col = "blue")

#Concordance Index
mean(outer(pihat[sore$Y==1],pihat[sore$Y==0],">") + 0.5*outer(pihat[sore$Y==1],pihat[sore$Y==0],"=="))
#Approx. 0.865 is the area under the curve

#e
par(mfrow=c(2,2))
plot(modA)
