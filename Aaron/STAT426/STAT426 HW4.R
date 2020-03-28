setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT426")
CancerR = data.frame(read.csv(file = 'cancer-remission.csv',header = T))
#Problem 2
#add a counts for number of Remission and number not in Remission
CancerR$Remission_Y = CancerR$NR
CancerR$Remission_N = CancerR$NC - CancerR$NR 
mod1 = glm(cbind(Remission_Y,Remission_N) ~ LI,data = CancerR, family = binomial)
summary(mod1)
#a
predict(mod1, data.frame(LI = 8), type = "response")

#b
coef(mod2)
-coef(mod2)[1]/coef(mod2)[2]

#c

