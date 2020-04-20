#Problem 1
setwd("/Users/chanm/Desktop")
dp = data.frame(read.table(file = "deathpenalty.txt", header = T))

#H0: No association b/t death penalty and race, Ha: Association b/t death penalty and race
dp$Defendant = factor(dp$Defendant, levels = unique(dp$Defendant)) 
#Change Factor to have values Yes = 1, No = 2
dp$DeathPenalty = factor(dp$DeathPenalty, levels = unique(dp$DeathPenalty))
#Change Factor to have values White = 1, Black = 2
dp$Victim = factor(dp$Victim,levels=unique(dp$Victim))

#Create contingency table
tab = xtabs(Freq~Defendant+Victim+DeathPenalty, data=dp)
ftable(tab)

mantelhaen.test(tab)
#Perform CMH test
#Test statistic of 381.38 with a p-val of less than 2.2e-16 
#At alpha = 0.05, we can reject H0 and we can say that at the 95%$ level there is association b/t death penalty verdict and race

chisq.test(ftable(tab))
#The chi-sq test returns a p-value of 0.0001957
#Similarly to CMH, we can say that at alpha=.05 H0 can be rejected and that these variables cannot be considered independent.

#Problem 2
#Set a seed so our random number generation will be the same when we knit
set.seed(100)
draw = rnorm(n = 8, mean = 0, sd = 1)
dat = data.frame(Y = c(0,0,0,0,1,1,1,1),
               x1 = c(10,20,30,40,60,70,80,90),
               x2 = draw)
mod = glm(Y ~ x1+x2, data = dat, family = binomial)

#Veiw plot of data for complete separation
plot(predict(mod), dat$Y, xlab = "Linear Predictor", ylab = "Y")
abline(v = 0, col = "red")

#Check numerical values for complete separation
cbind(dat$Y, predict(mod))
#The data seems to have complete separation, but not quasi-complete separation

summary(mod)
#With coefficients
coef(mod)
#and corresponding standard error: 157648.190,3292.922,18293.541 

#Problem 3
hs = data.frame(read.table(file = "horseshoe.txt", header = T))

#Fit a probit model
modprob = glm(y ~ width+color, data = hs, family = binomial(link = "probit"))
summary(modprob)
#At alpha=.05 all variable are significant
#For a one unit increase of width, the z-score is estimated to increase by 0.27581
#For a one unit increase of color, the z-score is estimated to decrease by 0.29188
confint(modprob)

modlogt = glm(y ~ width+color, data = hs, family = binomial(link = "logit"))
summary(modlogt)
#At alpha=.05 all variable are significant
#For a one unit increase of width, z-score is estimated to increase by 0.4583 with a logit model
#For a one unit increase of color, z-score is estimated to decrease by 0.5090 with a logit model
#Null and Residual Deviances appear to be the same in both models
confint(modlogt)