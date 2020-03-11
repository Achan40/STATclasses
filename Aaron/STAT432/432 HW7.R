#7.1
# load packages
library("tidyverse")
library("rpart")
library("caret")
library("mlbench")
library("e1071")

# set seed 
set.seed(225)

# simulate data
sim_data = as_tibble(mlbench.spirals(n = 500, sd = 0.15))

# test-train split
sim_trn_idx = sample(nrow(sim_data), size = 0.8 * nrow(sim_data))
sim_trn = sim_data[sim_trn_idx, ]
sim_tst = sim_data[-sim_trn_idx, ]

# estimation-validation split
sim_est_idx = sample(nrow(sim_trn), size = 0.8 * nrow(sim_trn))
sim_est = sim_trn[sim_est_idx, ]
sim_val = sim_trn[-sim_est_idx, ]

# check data
sim_trn

mod=knn3(classes~., k=13, data=sim_est)
mod1=knn3(classes~., k=13, data=sim_trn)

#accuracy = (truepositive+truenegative)/(positive+negative)
calcACC=function(actual,predicted){
  mean(actual==predicted)
}
calcACC(actual=sim_val$classes, predicted=predict(mod,sim_val,type="class"))
#1-accuracy
calc_misclass=function(actual,predicted){
  mean(actual!=predicted)
}        
calc_misclass(actual=sim_val$classes, predicted=predict(mod,sim_val,type="class"))        

#sensitivity and specificity
mod=knn3(classes~., k=13, data=sim_est)
pred.11=as.factor(ifelse(predict(mod,sim_val,type="prob")[,2]>0.5,2,1))
confMa=confusionMatrix(pred.11,reference=sim_val$classes)
confMa
#prevalence = no information rate
#sensitivity and specificity switched??

#When you study this later, take a look at the notes so that we can actually do this without
#loading in a package that may/not be used on the exam

#7.2
# load packages
library("mlbench")
library("tibble")
library("rpart")

# set seed 
set.seed(18)

# load data, remove NA rows, coerce to tibble
data("PimaIndiansDiabetes2")
diabetes = as_tibble(na.omit(PimaIndiansDiabetes2))

# split data
dbt_trn_idx = sample(nrow(diabetes), size = 0.8 * nrow(diabetes))
dbt_trn = diabetes[dbt_trn_idx, ]
dbt_tst = diabetes[-dbt_trn_idx, ]

# check data
dbt_trn
summary(dbt_trn)
summary(dbt_tst)
mod.12=rpart(diabetes~., data=dbt_trn)
result.12=as.factor(ifelse(predict(mod.12,dbt_tst,type="prob")[,2]>0.5,2,1))

table.dat = table(dbt_tst$diabetes,result.12)
table.dat
#accuracy= (TP+TN)/(P+N)
(table.dat[1,1]+table.dat[2,2])/(table.dat[1,1]+table.dat[2,1]+table.dat[1,2]+table.dat[2,2])
#sensivity= TP/P
(table.dat[2,2]/sum(table.dat[2,]))
#specificty= TN/N
1-table.dat[1,2]/sum(table.dat[1,])

#7.3
prior = c(.46, .36, .18)
meanp = c(1, 3, 4)
sdp = c(2, 2, 2)
xes = c(3.16, 3.89, 3.59)

probx1 = dnorm(xes[1],meanp[1],sdp[1])*prior[1] + dnorm(xes[1],meanp[2],sdp[2])*prior[2] + dnorm(xes[1],meanp[3],sdp[3])*prior[3] 
probx2 = dnorm(xes[2],meanp[1],sdp[1])*prior[1] + dnorm(xes[2],meanp[2],sdp[2])*prior[2] + dnorm(xes[2],meanp[3],sdp[3])*prior[3] 
probx3 = dnorm(xes[3],meanp[1],sdp[1])*prior[1] + dnorm(xes[3],meanp[2],sdp[2])*prior[2] + dnorm(xes[3],meanp[3],sdp[3])*prior[3] 

probyax = dnorm(xes[1],meanp[1],sdp[1]) * prior[1]
probybx = dnorm(xes[2],meanp[2],sdp[2]) * prior[2]
probycx = dnorm(xes[3],meanp[3],sdp[3]) * prior[3]

probyax/probx1
probybx/probx2
probycx/probx3

#7.4
prior = c(.81,.19)
priorA = c(2.9,3.9,3.9) 
priorB = c(6.6,7.2,8.1)
sdS = 2
X = c(1.3,1.8,3.6)

A = dnorm(X[1],priorA[1],sdS)*dnorm(X[2],priorA[2],sdS)*dnorm(X[3],priorA[3],sdS)*prior[1]
B = dnorm(X[1],priorB[1],sdS)*dnorm(X[2],priorB[2],sdS)*dnorm(X[3],priorB[3],sdS)*prior[2]

C = A+B
A/C
B/C

#7.5
priora = c(.02, .5, .86)
priorb = c(.98, .5, .14)
meanp = c(4, 6)
sdp = .7
x = 5

px5a = dnorm(x,meanp[1],sdp)*priora[1] + dnorm(x,meanp[2],sdp)*priorb[1]
px5b = dnorm(x,meanp[1],sdp)*priora[2] + dnorm(x,meanp[2],sdp)*priorb[2]
px5c = dnorm(x,meanp[1],sdp)*priora[3] + dnorm(x,meanp[2],sdp)*priorb[3]

pyxa = dnorm(x,meanp[1],sdp)*priora[1]
pyxb = dnorm(x,meanp[1],sdp)*priora[2]
pyxc = dnorm(x,meanp[1],sdp)*priora[3]

pyxa/px5a
pyxb/px5b
pyxc/px5c

#7.6
prior = c(.6, .32, .08)
meanp = c(1, 4, 6)
sdp = c(2, 2, 1)
xes = c(4.13, 4.95, 6.72)

probx1 = dnorm(xes[1],meanp[1],sdp[1])*prior[1] + dnorm(xes[1],meanp[2],sdp[2])*prior[2] + dnorm(xes[1],meanp[3],sdp[3])*prior[3] 
probx2 = dnorm(xes[2],meanp[1],sdp[1])*prior[1] + dnorm(xes[2],meanp[2],sdp[2])*prior[2] + dnorm(xes[2],meanp[3],sdp[3])*prior[3] 
probx3 = dnorm(xes[3],meanp[1],sdp[1])*prior[1] + dnorm(xes[3],meanp[2],sdp[2])*prior[2] + dnorm(xes[3],meanp[3],sdp[3])*prior[3] 
#yeah I don't really know this one... just guess and check?
probx1
probx2
probx3

#7.7
# load packages
library("mlbench")
library("tibble")
library("klaR")

# set seed 
set.seed(26031)

# simulate dataset
sim_est = as_tibble(mlbench.2dnormals(n = 100, cl = 3))

# change class names to A, B, and C
sim_est$classes = factor(dplyr::case_when(
  sim_est$classes == 1 ~ "A",
  sim_est$classes == 2 ~ "B",
  sim_est$classes == 3 ~ "C"))

# check data
sim_est

NaviB_mod = NaiveBayes(classes ~ .,data = sim_est)
NaviB_mod$tables$x.1[2,1]
NaviB_mod$tables$x.2[3,2]
NaviB_mod$apriori

predicty = data.frame(x.1 = .76,x.2 = -1.68)
predict(NaviB_mod, predicty, type = "raw")

#7.8
# load packages
library("MASS")
library("nnet")
library("klaR")
library("mlbench")
library("tibble")
library("caret")
library("tidyverse")

# set seed 
set.seed(66522)

# simulate dataset
sim_est = as_tibble(mlbench.2dnormals(n = 0100, cl = 4, sd = 1.5))
sim_val = as_tibble(mlbench.2dnormals(n = 1000, cl = 4, sd = 1.5))

# check data
sim_est

# helper function for misclassification rate
calc_misclass = function(act, pred) {
  mean(act != pred)
}

modlist = list(
  NavieB = NaiveBayes(classes ~., data = sim_est),
  NavieB_Flat = NaiveBayes(classes ~., data = sim_est, prior = c(1,1,1,1)/4),
  NavieB_Flat_KDE = NaiveBayes(classes ~., data = sim_est, prior = c(1,1,1,1)/4, usekernel = T),
  LDA = lda(classes ~., data = sim_est),
  LDA_Flat = lda(classes ~., data = sim_est, prior = c(1,1,1,1)/4),
  QDA = qda(classes ~., data = sim_est),
  QDA_Flat = qda(classes ~., data = sim_est, prior = c(1,1,1,1)/4),
  MULTI = multinom(classes ~ ., data = sim_est)
)

#calculation for misclassification [est to val]
calc_misclass(sim_val$classes, predict(modlist[[1]], sim_val)$class)
calc_misclass(sim_val$classes, predict(modlist[[2]], sim_val)$class)
calc_misclass(sim_val$classes, predict(modlist[[3]], sim_val)$class)
calc_misclass(sim_val$classes, predict(modlist[[4]], sim_val)$class)
calc_misclass(sim_val$classes, predict(modlist[[5]], sim_val)$class)
calc_misclass(sim_val$classes, predict(modlist[[6]], sim_val)$class)
calc_misclass(sim_val$classes, predict(modlist[[7]], sim_val)$class)
calc_misclass(sim_val$classes, predict(modlist[[8]], sim_val))
