library(VGAM)

#Problem 1
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
##If we were to look at race effects, the odds that someone identifies as Democrat versus Republican and is a White individual, it is exp(-1.1183) which is approximately 0.3268 times the odds of being a Black individual.
#Also, if we were to look at gender effects, the odds that someone identifies as Democrat versus Republican and is a male, it is exp(-0.2202) which is approximately 0.8024 times the odds of being a female.

#Problem 2
setwd("/Users/chanm/Desktop")
auto = data.frame(read.csv(file = "auto.csv", header = T))

mod2 = vglm(cbind(X1,X2,X3,X4,X5) ~ G + L + S + L * S, family = cumulative(parallel = TRUE), data = auto)
