#4.1
prior = c(0.15,0.4,0.45)
normu = c(5.4,6.1,6.3)
normsd = 1.5
X = 9

probx = dnorm(X,mean = normu[1],sd = normsd)*prior[1]+dnorm(X,mean = normu[2],sd = normsd)*prior[2]+dnorm(X,mean = normu[3],sd = normsd)*prior[3]
probYbarX = dnorm(X,mean = normu[3],sd = normsd)*prior[3]/probx
probYbarX

#4.2
x1 = 4
x2 = 3
x3 = (4+10)/2

estimator = 2*x1 + 3*x2 - 4*x3 -x1 #bias
estimator
varestimator = 4*x1 + 9*4 + 16*(36/12)#variance (sq coefs and var of variable)
varestimator
estimator^2 + varestimator #MSE

#4.5
#most flexible KNN: lowest k
#least bias KNN: lowest k
#least var KNN: highest k

#4.6
#least flexible tree: highest cp
#most bias tree: highest cp
#most variance tree: lowest cp

#4.7
# set seed 
set.seed(80994)

# "simulate" predictions
mu_hat = rnorm(n = 100, mean = 2.75, sd = 2)
meanTrue = 2.4

meanPred = mean(mu_hat)
meanPred - meanTrue#bias, predicted mean - true mean

get_var = function(est){
  mean((est-mean(est))^2)
}
get_var(mu_hat)#create function to find variance (can't use var())

get_mse = function(true,est){
  mean((est-true)^2)
}
get_mse(meanTrue,mu_hat)#function to find mse

#4.8
# load packages
library("caret")
library("mlbench")
library("tidyverse")

# set seed 
set.seed(85999)

# simulate train data
sim_trn_0010 = as.data.frame(mlbench.friedman1(n = 10))
sim_trn_0100 = as.data.frame(mlbench.friedman1(n = 100))
sim_trn_1000 = as.data.frame(mlbench.friedman1(n = 1000))

# simulate test data
sim_tst_1000 = as.data.frame(mlbench.friedman1(n = 1000))

# check data
head(sim_trn_0100)

#set k
K=5

#list of models
mods = list(
  knnreg(y ~ .,data = sim_trn_0010, k=K),
  knnreg(y ~ .,data = sim_trn_0100, k=K),
  knnreg(y ~ .,data = sim_trn_1000, k=K)
)

calc_RMSE = function(act,pred){
  sqrt(mean((act-pred)^2))
}#RMSE function

#calculation for test RMSE [trn to test]
test_pred = map(mods, predict, sim_tst_1000)#make predictions for each model using tst data
map_dbl(test_pred,calc_RMSE, sim_tst_1000$y)#use calc_RMSE function act = sim_tst_1000$y, pred = test_pred
#or (easier way to do this) [trn to test]
calc_RMSE(act = sim_tst_1000$y,pred = predict(mods[[1]],sim_tst_1000))
calc_RMSE(act = sim_tst_1000$y,pred = predict(mods[[2]],sim_tst_1000))
calc_RMSE(act = sim_tst_1000$y,pred = predict(mods[[3]],sim_tst_1000))

#4.9
# set seed 
set.seed(64933)

# simulate data
est_data = as.data.frame((mlbench.friedman1(n = 500, sd = 1)))
val_data = as.data.frame((mlbench.friedman1(n = 500, sd = 1)))

# check data
head(est_data)

#calculate validation RMSE for K=1:100 models [est to val]
calc_mod_val_rmse = function(d) {
  mod = knnreg(y ~ x.1 + x.2 + x.3 + x.4 + x.5, data = est_data,k=d)
  sqrt(mean((val_data$y - predict(mod, val_data)) ^ 2))#RMSE calculation, actual-predicted
}

val_rmse_list = map_dbl(1:100, calc_mod_val_rmse)#apply function calc_mod_val_rmse to each element of vector 1:100 and make a list
sd(val_rmse_list)

#overfitting
val_rmse_list[1]#lowest k value,k=1 underfits
#underfitting
val_rmse_list[100]#highest k value, k=100 overfits

#calculate train RMSE for K=1:100 models [est to est in this case]
calc_mod_trn_rmse = function(d) {
  mod = knnreg(y ~ x.1 + x.2 + x.3 + x.4 + x.5, data = est_data,k=d)
  sqrt(mean((est_data$y - predict(mod, est_data)) ^ 2))
}

trn_rmse_list = map_dbl(1:100, calc_mod_trn_rmse)
sd(trn_rmse_list)

#4.10
#model parameters: b0,b1,residual variance

#4.11
#overfitting: model is too flexible, when a flexible model has a higher valRMSE than less flexible
#models w lower valRMSE's, when we fit better to training data than regression function

#4.12
#KNN models and descision trees are non-parametric models

#4.13
#biased,2,most
#unbiased,5,most
#bias,1,least

#4.15
#in KNN, lower k: more variable

#4.16