#Problem 1
#a
setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT426")
horseshoe = read.table("horseshoe.txt", header=TRUE)
head(horseshoe)

horseshoe.ols = lm(y ~ weight, data = horseshoe)
summary(horseshoe.ols)

predict(horseshoe.ols, data.frame(weight = 5200), type = "response")
#For crab with 0 weight the estimated probability of crab with a satellite is -.01449, does not make too much sense though since each crab must have a weight
#As weight increases by 1kg, the estimated probability of a creab having a satellite increases by .0003227
#For the predicted probability of a crab having a satellite with weight = 5200, a probability of 1.533186 does not make too much sense

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
#A higer ratio means that death rate is higher for smokers than for non-smokers.
#A Ratio < 1 means that the death rate is lower for smokers than non-smokers.
#When age increases, the Ratios decrease meaning that deaths because of smoking decrease as age increases.

#b
mainEfftects = glm(Deaths ~ Age + Smoke, family = poisson, offset = log(PersonYears), data = Smoke.dat)
mainEfftects
#Our ratio of rates table contradicts the main effects models findings, since the effects of smoking on death is less strong as age increases.
#This shows that the main effects model assumes a constant ratio of nonsmokers to smokers coronary death rates over age.
#There for the main effects model is the the best to model the smoking-death relationship.

#c
#In a, we can see a linear relationship between death rate and age, so we can have them interact.
#The model: log(u/t) = a + B1age + B2smoking + B3(smoking*age)

#Assign Scores
Smoke.datNew = data.frame(
  AgeScore = c(rep(1,2),rep(2,2),rep(3,2),rep(4,2),rep(5,2)),
  PersonYears = c(18793,52407,10673,43248,5710,28612,2585,12663,1462,5317),
  Smoke = c(rep(c("no","yes"),5)),
  Deaths = c(2,32,12,104,28,206,28,186,31,102)
)

SmokeQuant = glm(Deaths ~ AgeScore*Smoke, family = poisson, offset = log(PersonYears), data = Smoke.datNew)
SmokeQuant

#The main effects model has a lower deviance and AIC than the quant model.
#The quat model might not bet the best since the log of the rate does not change with age linearly.
#Adding an age^2 might better fit the model since to make age no longer a linear relationship.
