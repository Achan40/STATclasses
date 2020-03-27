library(faraway)
#Problem 1
data(salmonella)
salmonella[order(salmonella$dose),]
mod1 = lm(colonies ~ log(dose + 1), data = salmonella)
summary(mod1)

#lack of fit (sigma^2 unknown).

#make a plot 
plot(colonies ~ log(dose + 1), data = salmonella)
abline(coef(mod1))
#get fitted values
mod1f = lm(colonies ~ factor(dose), data = salmonella)
cbind(salmonella,mod1f$fitted)[order(salmonella$dose),]

#Anova test (testing for lack of fit)
lof_Anova = anova(mod1,mod1f)
lof_Anova
#H0=There is no lack of fit vs. Ha=There is lack of fit
1-pf(lof_Anova$F[2],4,12)
#Cannot reject H0, we cannot say there is lack of fit at alpha=.95 level

#Problem 2
data(gammaray)
mod2 = lm(flux ~ time, data = gammaray, weights = 1/error^2 )
summary(mod2)

#Problem 3
data(longley)
mod3 = lm(Employed~.,data = longley)
summary(mod3)

#a
#condition number
m3_mat = model.matrix(mod3)[,-1]
e = eigen(t(m3_mat) %*% m3_mat)
e$val
sqrt(e$val[1]/e$val)
#standardize matrix
m3_mat = model.matrix(mod3)[,-1]
m3_mat = m3_mat - matrix(apply(m3_mat, 2, mean), 16,6, byrow = T)
m3_mat = m3_mat / matrix(apply(m3_mat, 2, sd), 16,6, byrow = T)
apply(m3_mat,2,mean)
apply(m3_mat,2,var)
#condition number
e = eigen(t(m3_mat) %*% m3_mat)
sqrt(e$val[1]/e$val)

#b
#pairwise correlation
round(cor(longley), dig=2)
#Year is correlated highly corrlated with all variables except Unemployed and Armed.Forces
#Armed forces is not very correlated with any of the other variables
#GNP and GNP.deflator are esstially correlated with other predictors exactly, porbably should not have both of them in our model at the same time.

#c
round(vif(m3_mat), dig=2)
#GNP
sqrt(1788.51)
#Year
sqrt(758.98)
#Population
sqrt(399.15)
#We have very large values for VIF's (top 3): 42.29078, 27.54959, 19.97874
#The se for the coef associated with GNP is 42.29078 larger than it would have been without collinearity
#The se for the coef associated with Year is 27.54959 larger than it would have been without collinearity
#The se for the coef associated with Population is  19.97874 larger than it would have been without collinearity
#We'd have to remove quite a few variables to reduce collinearilty, as can be seen from the correlation table:
#Removing Year, GNP, and Population might be a good start since these variables tend to be positively linear correlated.