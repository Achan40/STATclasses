#Problem 1
library(VGAM)

#Create data
GSS = data.frame(
  Gender = c(rep("Male",2),rep("Female",2)),
  Race = c(rep(c("White","Black"),2)),
  Party = c(rep("D",4),rep("R",4),rep("I",4)),
  Count = c(132,42,172,56,176,6,129,4,127,12,130,15)
)

GSS

#Organize data
gss = reshape(GSS, v.names = "Count", timevar = "Party",
               idvar = c("Gender","Race"), direction = "wide")
gss

#model
mod = vglm(cbind(Count.D, Count.R, Count.I) ~ Gender + Race, family = multinomial, data = gss)
#Independent is the baseline category in this case (we put it last)
summary(mod)
#Looking at race effects, the odds that someone identifies as Democrat(D) vs Republican(R) and is White, is: exp(-1.1183).
#Or Approximately 0.3268 times the odds of being Black.
#As for gender effects, the odds that someone identifies as Democrat(D) vs Republican(R) and is male, is: exp(-0.2202).
#This is approximately 0.8024 times the odds of being Female.

#Problem 2
setwd("/Users/chanm/Desktop")
auto = data.frame(read.csv(file = "auto.csv", header = T))

mod2 = vglm(cbind(X1,X2,X3,X4,X5) ~ I(G == "Female") + I(L == "Rural") * I(S == "No"), family = cumulative(parallel = TRUE), data = auto)
summary(mod2)

#a
exp(coef(mod2)[1:4])/(1+exp(coef(mod2)[1:4]))
#For a male who wears his seatbelt and has an accident in an urban area, he has about a 96.47% chance of not being injured.

#b
exp(coef(mod2)[5]+c(-1,1)*1.96*(0.02721))
#As we take samples n->infinity 95% of those samples that the odds of a female being in category j or less, where j=1,2,3,4, are between 0.54904 and 0.6108396 the times for males.

#c
exp(coef(mod2)[7]+coef(mod2)[8])
#The estimated cumulative odds ratio between the response and seat-belt use for rural locations is approx 0.41289.
exp(coef(mod2)[7])
#The estimated cumulative odds ratio between the response and seat-belt use for urban locations is approx 0.4676.
#The effect of seatbelt use on the odds of injuries is more noticeable in rural areas than urban.