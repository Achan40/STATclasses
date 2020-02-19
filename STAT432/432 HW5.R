#5.1
prior = c(.64,.36)
priorA = c(2.4,2.5,3) 
priorB = c(6,7.8,8.7)
sdS = 2
X = c(2,2.8,3.2)

A = dnorm(X[1],priorA[1],sdS)*dnorm(X[2],priorA[2],sdS)*dnorm(X[3],priorA[3],sdS)*prior[1]
B = dnorm(X[1],priorB[1],sdS)*dnorm(X[2],priorB[2],sdS)*dnorm(X[3],priorB[3],sdS)*prior[2]

C = A+B
A/C
B/C

#5.8
.12/(.15+.05+.1+.12)
#5.9
.09/(.12+.01+.09)

#5.11
prior = c(.09,.64,.27)
X=3
dnorm(X,8,1.5)*prior[1]
dnorm(X,8.3,1.5)*prior[2]
dnorm(X,8.7,1.5)*prior[3]
#choose largest?

#5.12
prior = c(.21,.5,.29)
X=4
dpois(X,4.43)*prior[1]
dpois(X,4.8)*prior[2]
dpois(X,8.6)*prior[3]

#5.14&15
# load packages
library("mlbench")
library("tibble")
library("caret")
library("rpart")

# set seed 
set.seed(82451)

# generate data
class_data = mlbench.simplex(n = 800, d = 2, sd = 0.5)
class_data = as_tibble(class_data)

#make KNN model and data to use for predict()
mod1 = knn3(classes~.,k=5,data = class_data)
predictdata_prob = data.frame(x.1 = -1.22,x.2=.83)
predictdata = data.frame(x.1=-.87,x.2=-1.19)

predict(mod1,predictdata_prob,type="prob")
predict(mod1,predictdata,type = "class")

#make descision tree model
mod2 = rpart(classes~.,data = class_data, cp=1,minsplit=5)

predict(mod2,predictdata_prob,type="prob")
predict(mod2,predictdata,type = "class")

#5.16&17&18&19
# load packages
library("mlbench")
library("tibble")
library("caret")
library("rpart")
library("ISLR")
# set seed 
set.seed(35188)

# load data and coerce to tibble
default = as_tibble(ISLR::Default)

# split data
dft_trn_idx = sample(nrow(default), size = 0.8 * nrow(default))
dft_trn = default[dft_trn_idx, ]
dft_tst = default[-dft_trn_idx, ]

# check data
dft_trn

#knnmodel
mod3 = knn3(default~.,data = dft_trn)
predictdata = data.frame(balance=321,income=37640,student = factor(levels(dft_trn$student)))
predict(mod3,predictdata,type = "prob")

#desTree model
mod4 = rpart(default~.,data = dft_trn, cp=.001, minsplit=10)
predictdata = data.frame(balance=1189,income=33455,student = factor(levels(dft_trn$student)))
predict(mod4,predictdata,type = "prob")

#another KNNmodel
mod5 =  knn3(default~.,data = dft_trn, k=9)
#make a function comparing actual values[tst] to predicted[trn]
calc_accuracy = function(actual, predicted) {
  mean(actual == predicted)#average of how many actual values equal to predicted values gives us the accuracy
}
calc_accuracy(dft_tst$default,predict(mod5,dft_tst,type="class"))

#last des Treemodel
mod6 = rpart(default~.,data = dft_trn, cp=.1, minsplit=10)
calc_misclass = function(actual, predicted) {
  mean(actual != predicted)#average of how many actual values equal to predicted values gives us the accuracy
}
calc_misclass(dft_tst$default,predict(mod6,dft_tst,type="class"))
