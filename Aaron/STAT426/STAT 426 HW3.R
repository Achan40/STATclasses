#Problem 1
#a
setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT426")
horseshoe = read.table("horseshoe.txt", header=TRUE)
head(horseshoe)

horseshoe.ols = lm(y ~ weight, data = horseshoe)
summary(horseshoe.ols)

predict(horseshoe.ols, data.frame(weight = 5200), type = "response")

#b
horseshoe.ML = 

#c
horseshoe.log = glm(y ~ weight, family = binomial,data = horseshoe) 

predict(horseshoe.log, data.frame(weight = 5200), type = "response")

#Problem 2
#a
#Organizing data
Smoke.dat = data.frame(
  Age = c(rep("35-44",2),rep("45-54",2),rep("55-64",2),rep("65-74",2),rep("75-85",2)),
  PersonYears = c(18793,52407,10673,43248,5710,28612,2585,12663,1462,5317),
  Smoke = c(rep(c("no","yes"),5)),
  Deaths = c(2,32,12,104,28,206,28,186,31,102)
)

#Calculate cornonary death rate
DRate = Smoke.dat[,4]/Smoke.dat[,2]
#Add the new column to the data
Smoke.dat = cbind(Smoke.dat,DRate)
Smoke.dat

#Ratio of Death Rates for Smoking to Non-smoking 
DRate_Smoke = Smoke.dat[Smoke.dat$Smoke == "yes",]$DRate
DRate_NoSmoke = Smoke.dat[Smoke.dat$Smoke == "no",]$DRate
Ratio = DRate_Smoke/DRate_NoSmoke
#By Age group
ratios.dat = data.frame(
  Age = c("35-44","45-54","55-64","65-74","75-85")
)
ratios.dat = cbind(ratios.dat,Ratio)
ratios.dat

#b
mainEfftects = glm(Deaths ~ Age + Smoke, family = poisson, offset = log(PersonYears), data = Smoke.dat)
summary(mainEfftects)
