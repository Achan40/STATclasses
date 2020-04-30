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

mod2 = vglm(cbind(X1,X2,X3,X4,X5) ~ G + L + S + L * S, family = cumulative(parallel = TRUE), data = auto)
