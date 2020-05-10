library(faraway)

#Problem 2
#a
data(wbca)
mod = glm(Class ~ ., family = binomial,data = wbca)
summary(mod)

#Residual Deviance
mod$deviance
#With degrees of freedom 671
#This information can be used to determine if models fit the data
#Deviance measures how well a model fits to the data, higher deviance means our model does not fit our data very well
#However, we still have to take in account the siginificance of individual variables

#b
modstep = step(mod, scope = list(upper=~., lower=~1))
summary(modstep)
#The best model using AIC as the criteria, the best model is the one with 7 variables: Class ~ Adhes + BNucl + Chrom + Mitos + NNucl + Thick + UShap
#The AIC of this model is the lowest at 105.66

#c
pred = factor(predict(mod,type="response") > 0.5, labels = c("pred-Mal.","pred-Ben."))
pred.t = table(pred,wbca$Class)
pred.t[2,1]/(pred.t[1,1]+pred.t[2,1])
#Using 0.5 as the cutoff, about 4.2% of the 238 patients with a malignant tumor have been incorrectly categorized as having a benign tumor
pred.t[1,2]/(pred.t[2,2]+pred.t[1,2])
#Using 0.5 as the cutoff, about 2.03% of the 443 patients with a benign tumor have been incorrectly categorized as having a malignant tumor+
