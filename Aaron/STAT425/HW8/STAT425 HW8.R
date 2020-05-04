library(faraway)

#Problem 2
#a
data(wbca)
mod = glm(Class ~ ., family = binomial,data = wbca)
summary(mod)

#Residual Deviance
mod$deviance
#With degrees of freedom 671

#b
#Backward regression
modstep = step(mod, scope = list(upper=~., lower=~1))
summary(modstep)
#The best model using AIC as the criteria, the best model is the one with 7 variables: Class ~ Adhes + BNucl + Chrom + Mitos + NNucl + Thick + UShap
#The AIC of this model is the lowest at 105.66