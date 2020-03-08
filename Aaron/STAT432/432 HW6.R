#6.1
prior = c(.57,.43)
priorA = c(2.8,3.2,3.9) 
priorB = c(6.2,6.8,8.6)
sdS = 2
X = c(3.7,3.8,4.9)

A = dnorm(X[1],priorA[1],sdS)*dnorm(X[2],priorA[2],sdS)*dnorm(X[3],priorA[3],sdS)*prior[1]
B = dnorm(X[1],priorB[1],sdS)*dnorm(X[2],priorB[2],sdS)*dnorm(X[3],priorB[3],sdS)*prior[2]

C = A+B
A/C
B/C

#6.2
# load packages
library("tidyverse")
library("rpart")
library("caret")
library("mlbench")

# set seed 
set.seed(14)

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

# values of k to consider
k = seq(1, 51, by = 2)

#accuracy function
calc_mod_accuracy = function(d) {
  mod = knn3(classes ~., data = sim_est,k=d)
  mean(sim_val$classes==predict(mod,sim_val,type="class"))
}
#make a list (most variable k=1,most biased k=51)
validation_acc_list = map_dbl(k, calc_mod_accuracy)
validation_acc_list
sd(validation_acc_list)

#test accuracy
tstmod = knn3(classes~.,data = sim_trn,k=19)
#create accuracy function
calc_accuracy = function(actual, predicted) {
  mean(actual == predicted)#average of how many actual values equal to predicted values gives us the accuracy
}

calc_accuracy(sim_tst$classes,predict(tstmod,sim_tst,type="class"))

#6.3
#a&b plug in x1 and x2
#c&d take the exponential after plugging in x1&2
x1 = .04
x2 = -.06
1.49+ -2.33*(x1) + 1.87*(x2)
exp(1.49+ -2.33*(x1) + 1.87*(x2))

#6.4
library(boot)
x1 = c(.84,.68,.07,.62)
x2 = c(-.7,.55,.96,.86)
logs = c(4.65,-4.06,1.51)
xa = logs[1]+logs[2]*(x1[1])+logs[3]*(x2[1])
xb = logs[1]+logs[2]*(x1[2])+logs[3]*(x2[2])
xc = logs[1]+logs[2]*(x1[3])+logs[3]*(x2[3])
xd = logs[1]+logs[2]*(x1[4])+logs[3]*(x2[4])
inv.logit(xa)#for parts a and b
inv.logit(xb)
1-inv.logit(xc)#for parts c and d
1-inv.logit(xd)

#6.5
#log(p(x)/1-p(x))=log(.5/.5)=0
#given x1 we can now solve for x2, plug and solve
logs = c(2.29,-.76,.77)
x1 = c(-.8,.21)
x2 = c(-.48,.78)

(-logs[1]-logs[2]*x1[1])/logs[3]
(-logs[1]-logs[2]*x1[2])/logs[3]

(-logs[1]-logs[3]*x2[1])/logs[2]
(-logs[1]-logs[3]*x2[2])/logs[2]

#6.6
# function to generate data
gen_logistic_data = function(sample_size = 100) {
  x1 = round(runif(n = sample_size), 2)
  x2 = round(runif(n = sample_size), 2)
  nu = 2 + 3 * x1 + -5 * x2
  y = rbinom(n = sample_size, size = 1, prob = boot::inv.logit(nu))
  data.frame(y, x1, x2)
}

# simulating the data
set.seed(42)
some_data = gen_logistic_data()

# checking the data
head(some_data)

B0 = c(-2.84,-2.98,-2.32)
B1 = c(3.46,.88,1.99)
B2 = c(-3.46,-7.45,6.83)

a=B0[1]+B1[1]*(some_data$x1)+B2[1]*(some_data$x2)
b=B0[2]+B1[2]*(some_data$x1)+B2[2]*(some_data$x2)
c=B0[3]+B1[3]*(some_data$x1)+B2[3]*(some_data$x2)
pa=1/(1+exp(-a))
pb=1/(1+exp(-b))
pc=1/(1+exp(-c))
sum(log(dbinom(some_data$y,1,pa)))
sum(log(dbinom(some_data$y,1,pb)))
sum(log(dbinom(some_data$y,1,pc)))
#sum of squares of B1 and B2 of largest log-likelihood
sqrt(sum(B1[1]^2 + B2[1]^2))

#6.7
#bayes classifier cutoff is .5, plug and chugg. if prob is <.5 then classifier =0
eqlog = c(4.57, -1.46, -1.84)
x1 = c(.13, .68, .74, .49)
x2 = c(-.58, .34, -.92, .93)

eqlog[1] + eqlog[2]*x1[1] + eqlog[3]*x2[1]
eqlog[1] + eqlog[2]*x1[2] + eqlog[3]*x2[2]
eqlog[1] + eqlog[2]*x1[3] + eqlog[3]*x2[3]
eqlog[1] + eqlog[2]*x1[4] + eqlog[3]*x2[4]

#6.8
# load packages
library(mlbench)
library(tibble)

# simulate data
set.seed(42)
sim_data = as_tibble(mlbench.2dnormals(n = 300, sd = 1.5))

# check data
sim_data

#create model
logregress = glm(classes ~ .,data = sim_data,family = "binomial")
summary(logregress)
#dataframe for prediction
b.data = data.frame(x.1 = -.99, x.2 = -.73)
c.data = data.frame(x.1 = -.39, x.2 = -.62)
d.data = c(.04)

#for classes=2
predict(logregress,b.data,type = "response")

#for classes=1
1-predict(logregress,c.data,type = "response")

#see scratch paper for explaination (or q6.5)
equationlog = coef(logregress)
(-equationlog[1]-equationlog[2]*d.data)/equationlog[3]

#6.9
# load packages
library(mlbench)
library(tibble)
library(purrr)

# simulate data
set.seed(2598)
sim_est = as_tibble(mlbench.circle(200))
sim_val = as_tibble(mlbench.circle(200))
sim_trn = rbind(sim_est, sim_val)
sim_tst = as_tibble(mlbench.circle(1000))

# check data
sim_est

mod1 = glm(classes ~ 1,data = sim_est,family = "binomial")
mod2 = glm(classes ~ .,data = sim_est,family = "binomial")
mod3 = glm(classes ~ x.1+x.2+I(x.1^2)+I(x.2^2),data = sim_est,family = "binomial")

#all the probabilities are the same in the intercept model, same answer for a and b
predict(mod1,type = "response")

#why is is like this idk
mean(ifelse(predict(mod1,newdata = sim_val,type = "response") > 0.5, 2,1) != sim_val$classes)
mean(ifelse(predict(mod2,newdata = sim_val,type = "response") > 0.5, 2,1) != sim_val$classes)
mean(ifelse(predict(mod3,newdata = sim_val,type = "response") > 0.5, 2,1) != sim_val$classes)

#best model
mod4 = glm(classes ~ x.1+x.2+I(x.1^2)+I(x.2^2),data = sim_trn,family = "binomial")
mean(ifelse(predict(mod4,newdata = sim_tst,type = "response") > 0.5, 2,1) != sim_tst$classes)

#mod2_val_pred = ifelse(predict(mod2) > 0, "2","1" != sim_val$classes)
#mean(mod2_val_pred != sim_val$classes)

#mod3_val_pred = ifelse(predict(mod3) > 0, "2","1" != sim_val$classes)
#mean(mod3_val_pred != sim_val$classes)


#Q6.10
library("mlbench")
library("tibble")

# set seed 
set.seed(30684)

# load data and coerce to tibble
default = as_tibble(ISLR::Default)

# split data
dft_trn_idx = sample(nrow(default), size = 0.8 * nrow(default))
dft_trn = default[dft_trn_idx, ]
dft_tst = default[-dft_trn_idx, ]

# check data
dft_trn

mod.10=glm(default~., data=dft_trn, family="binomial")
result= predict(mod.10,newdata=dft_tst,type="response")
#range(result)

table(dft_tst$default,result>0.01)
#true positives= true,yes (act,pred)
#false positives=  true,no
#false negatives=  false,yes
#true negatives= false,no

#Q6.11
# load packages
library("tidyverse")
library("rpart")
library("caret")
library("mlbench")
library("e1071")

# set seed 
set.seed(71)

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

#6.12
# load packages
library("mlbench")
library("tibble")
library("rpart")

# set seed 
set.seed(21)

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
result.12=as.factor(ifelse(predict(mod.12,dbt_tst,type="prob")[,2]>0.85,2,1))

table(dbt_tst$diabetes,result.12)
#accuracy= (TP+TN)/(P+N)
(51+7)/(51+12+9+7)
#sensivity= TP/P
7/19 #(2,pos)/(1,pos + 2,pos)
#specificty= TN/N
1- 9/60 #1- (2,neg)/(1,neg + 2,neg)