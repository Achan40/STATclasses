library(readxl)
library(VGAM)

setwd("/Users/chanm/Desktop")
belief = read_excel("belief.xlsx", skip = 1)
lcct = read_excel("lcct.xlsx", skip = 1)

#Problem 1
#a
#Cumulative logit model w main effects for therapy and gender (Uncollapsed model)
cumLoga = vglm(cbind(Progressive,No_Change,Partial_Remision,Complete_Remision) ~ Therapy + Gender, family = cumulative(parallel = T, link = "logitlink"), data = lcct)
summary(cumLoga)

#b
#Combining first two response and last two response
lcctCol = subset(lcct, select = c(Therapy,Gender))
lcctCol$ProandNo = rowSums(lcct[,c("Progressive","No_Change")])
lcctCol$ParandComp = rowSums(lcct[,c("Partial_Remision","Complete_Remision")])

#Collapsed model
cumLoga2 = vglm(cbind(ProandNo,ParandComp) ~ Therapy + Gender, family = cumulative(parallel = T, link = "logitlink"), data = lcctCol)
summary(cumLoga2)

#c

#d
#Uncollapsed model with and interaction term
cumLoga3 = vglm(cbind(Progressive,No_Change,Partial_Remision,Complete_Remision) ~ Therapy + Gender + Therapy:Gender, family = cumulative(parallel = T, link = "logitlink"), data = lcct)
summary(cumLoga3)

#Problem 2
#a
#Cumulative logit model treating belief in heaven as ordinal
cumLogb = vglm(cbind(Yes,Unsure,No) ~ Race + Gender, family = cumulative(parallel = T, link = "logitlink"), data = belief)
summary(cumLogb)

#b
#Cumulative probit model treating belief in heaven as ordinal
cumProb = vglm(cbind(Yes,Unsure,No) ~ Race + Gender, family = cumulative(parallel = T, link = "probitlink"), data = belief)
summary(cumProb)
