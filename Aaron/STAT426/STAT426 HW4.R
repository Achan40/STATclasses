setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT426")
CancerR = data.frame(read.csv(file = 'cancer-remission.csv',header = T))
#Problem 1
ungr = data.frame(trials = c(1:12),
                x = c(0,0,0,0,1,1,1,1,2,2,2,2),
                y = c(1,0,0,0,1,1,0,0,1,1,1,1))

gr =  data.frame(x = c(0,1,2),
                n = c(4,4,4),
                y = c(1,2,4))
#a
#ungrouped
mod1a = glm(y ~ x,family = binomial,data = ungr)
summary(mod1a)
#M0:-1.503 & M1:-1.503 + 2.06x
#Null Deviance (-2L0) = 16.301 & Residual Devicance (-2L1) = 11.028

#grouped
gr$yes = gr$y
gr$no = gr$n - gr$y
mod1a2 = glm(cbind(yes,no) ~ x, family=binomial, data=gr)
summary(mod1a2)
#M0:-1.503 & M1:-1.503 + 2.06x
#Null Deviance (-2L0) = 6.2568 & Residual Devicance (-2L1) = 0.9844


#b
NullD.ungr = mod1a$null.deviance
ResD.ungr = mod1a$deviance
NullD.gr = mod1a2$null.deviance
ResD.gr = mod1a2$deviance

NullD.ungr
ResD.ungr
NullD.gr
ResD.gr
#Both deviances for grouped data are less than deviances for the ungrouped one.
#The grouped model has 3 parameters instead of 2,the reason why there is a change in the deviances value between grouped and ungrouped models

#c
NullD.ungr - NullD.gr #difference of 10.04386
ResD.ungr - ResD.gr #difference of 10.04386

1 - pchisq(deviance(mod1a),df.residual(mod1a))
1 - pchisq(deviance(mod1a2),df.residual(mod1a2))
#The models fit data fairly well


#Problem 2
#add a counts for number of Remission and number not in Remission
CancerR$Remission_Y = CancerR$NR
CancerR$Remission_N = CancerR$NC - CancerR$NR 
mod2 = glm(cbind(Remission_Y,Remission_N) ~ LI,data = CancerR, family = binomial)
summary(mod2)
a = mod2$coefficients[1]
b = mod2$coefficients[2]
#a
pihat.a = predict(mod2, data.frame(LI = 8), type = "response")
pihat.a

#b
-a/b

#c
b*pihat.a*(1-pihat.a)
#When LI = 8, the rate of change is approx 0.0092
pihat.c = predict(mod2, data.frame(LI = 26), type = "response")
b*pihat.c*(1-pihat.c)
#When LI = 26, the rate of change is approx 0.0362

#d
predict(mod2, data.frame(LI = 14), type = "response")
predict(mod2, data.frame(LI = 28), type = "response")
#pihat increases from .15 to .57 between LI values of 14 and 28 respectively

#e
exp(b)
#When LI increases by one, odds for remission are estimated to increase by 1.16

#f
vcov(mod2)

#g
exp(confint.default(mod2))
#95% Confidence interval for odds ratio when increasing LI by one unit is 1.028968610,1.2984476

#h
#testing H0:B=0
library(aod)
waldt = wald.test(b = coef(mod2), Sigma = vcov(mod2), Terms = 2)
waldt
waldt$result
#with a pvalue of 0.0146386 we reject H0 at alpha=0.05. We cannot say that the injection does not have effect on the patient
