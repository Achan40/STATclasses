setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT426")
CancerR = data.frame(read.csv(file = 'cancer-remission.csv',header = T))
#Problem 2
#add a binary remission column 1=remission 0=no remission
CancerR$Remission = c(rep(0,5),1,1,1,0,1,1,0,1,1)
mod1 = glm(Remission ~ LI/100,data = CancerR, family = binomial)
summary(mod1)
#a
predict(mod1, data.frame(LI = 8), type = "response")

#b
coef(mod1)
-coef(mod1)[1]/coef(mod1)[2]

#c

#################################################
testing = data.frame(lapply(CancerR, rep, CancerR$NC))
mod2 = glm(Remission ~ LI,data = testing, family = binomial)
summary(mod2)
