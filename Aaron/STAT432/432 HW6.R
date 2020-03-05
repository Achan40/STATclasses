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
set.seed(8)

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
  mod = knnreg(classes ~., data = sim_est,k=d)
  mean(sim_val$classes==predict(mod,sim_val,type="class"))
}
#make a list
validation_acc_list = map_dbl(k, calc_mod_accuracy)
validation_acc_list
sd(validation_acc_list)

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

B0 = c(-.84,-.64,1.08)
B1 = c(4.25,.72,.25)
B2 = c(3.24,-.36,5)

a=B0[1]+ B1[1]*(some_data$x1)-B2[1]*(some_data$x2)
b=B0[2]+ B1[2]*(some_data$x1)-B2[2]*(some_data$x2)
c=B0[3]+ B1[3]*(some_data$x1)-B2[3]*(some_data$x2)
pa=1/(1+exp(-a))
pb=1/(1+exp(-b))
pc=1/(1+exp(-c))
log(dbinom(some_data$y,1,pa))
log(dbinom(some_data$y,1,pb))
log(dbinom(some_data$y,1,pc))

