#1-----------------------------------------
prior = c(.3,.15,.55)
prior2 = c(2.91,2.93,5.69)
X = 5

prior[2]*dpois(X,prior2[2])
pX = sum(prior*dpois(X,prior2))
sum(prior*dpois(X,prior2))
dpois(X,prior2[2])*prior[2]/pX
#2----------------------------------------
# set seed 
set.seed(90478)

# generate data from known poisson distribution
pois_data = rpois(n = 250, lambda = 3.5)

# check data
head(pois_data, n = 10)

mean(pois_data)
sqrt(mean(pois_data))
lambda = mean(pois_data)
dpois(8,lambda)
mean(pois_data == 4)
#3-------------------------------------------
# set seed 
set.seed(47367)

# generate data from known normal distribution
norm_data = rnorm(n = 500, mean = 4.2, sd = 4.2)

# check data
head(norm_data, n = 10)

mean3=mean(norm_data)
mean3
sqrt3=sqrt(mean((norm_data)^2)-(mean(norm_data))^2)
sqrt3
pnorm(4,mean3,sqrt3,lower.tail = FALSE)
mean(norm_data>3)
#4-----------------------------------------
set.seed(41199)

# generate data from known bernoulli distribution
bern_data = rbinom(n = 20.6350, size = 1, prob = 0.25)

# check data
head(bern_data, n = 10)

binom.loglik = function(parms,bern_data){
  
  out = dbinom(bern_data,1,parms,log=T)  # This evaluates the log of the bernoulli probability for each observation.
  
  return(sum(out))   # We sum it instead of taking the product because we have log-likelihood. 
  
}

binom.loglik(0.5,bern_data) 
binom.loglik(0.44,bern_data) 
binom.loglik(0.34,bern_data) 
#phat mle is the lease prob

#5----------------------------------------------
set.seed(80114)

# generate data from known normal distribution
norm_data = rnorm(n = 1000, mean = 42, sd = 0.42)

# check data
head(norm_data, n = 10)

norm.log = function(parm1,parm2,norm_data){
  out = dnorm(norm_data,mean = parm1,sd = parm2,log = TRUE)
  return(sum(out))
}

norm.log(42.86,.41,norm_data)

#6&7----------------------------------------------
# load packages
library("tibble")

# set seed 
set.seed(50762)

# load data
bstn = as_tibble(MASS::Boston)

# test-train split
bstn_trn_idx = sample(nrow(bstn), size = 0.8 * nrow(bstn))
bstn_trn = bstn[bstn_trn_idx, ]
bstn_tst = bstn[-bstn_trn_idx, ]

# estimation-validation split
bstn_est_idx = sample(nrow(bstn_trn), size = 0.8 * nrow(bstn_trn))
bstn_est = bstn_trn[bstn_est_idx, ]
bstn_val = bstn_trn[-bstn_est_idx, ]

# check data
head(bstn_trn)

fit = lm(medv ~ lstat + ptratio + tax, data=bstn_trn)

#Models
fitDegree = lm(medv ~ poly(lstat,degree = 3),data = bstn_est)
fitDegree2 = lm(medv ~ poly(lstat,degree = 3),data = bstn_trn)
#funciton for RMSE
calc_rmse = function(actual,predicted) {
  sqrt(mean((actual-predicted)^2))
}
#RMSEvalidation (2ways same answer) [est to val]
sqrt(mean((bstn_val$medv - predict(fitDegree, bstn_val)) ^2)) # do not name the second arg
calc_rmse(actual = bstn_val$medv, predicted = predict(fitDegree,bstn_val))
#RSMETrain [est to est]
calc_rmse(actual = bstn_est$medv,predicted = predict(fitDegree,bstn_est))
#RMSETest [trn to test]
calc_rmse(actual = bstn_tst$medv,predicted = predict(fitDegree2,bstn_tst))

#function for MAE
calc_mae = function(actual,predicted){
  mean(abs(actual-predicted))
}
#MAEvalidation
calc_mae(actual = bstn_val$medv, predicted = predict(fitDegree,bstn_val))
#MAEtrain
calc_mae(actual = bstn_est$medv,predicted = predict(fitDegree,bstn_est))
#MAEtest
calc_mae(actual = bstn_tst$medv,predicted = predict(fitDegree2,bstn_tst))

#8------------------------------------------
fitlog = lm(log(medv) ~ lstat, data = bstn_trn)
#predicted values (log scale)
a = predict(fitlog,newdata = data.frame(lstat=c(13.63,9.75,9.6)))
#undo log
exp(a)

#9-------------------------------------------
# load packages
library("tidyverse")

# set seed 
set.seed(55977)

# load data
bwt = as_tibble(MASS::birthwt)

# data prep
bwt = bwt %>% 
  select(-low, -ht, -ui) %>% 
  mutate(race = factor(race, labels = c("white", "black", "other")),
         smoke = factor(smoke, labels = c("non-smoker", "smoker")))

# test-train split
bwt_trn_idx = sample(nrow(bwt), size = 0.8 * nrow(bwt))
bwt_trn = bwt[bwt_trn_idx, ]
bwt_tst = bwt[-bwt_trn_idx, ]

# estimation-validation split
bwt_est_idx = sample(nrow(bwt_trn), size = 0.8 * nrow(bwt_trn))
bwt_est = bwt_trn[bwt_est_idx, ]
bwt_val = bwt_trn[-bwt_est_idx, ]

# check data
head(bwt_trn)
levels(bwt_trn$race)

#models
mod_list = list(
  mod_1 = lm(bwt ~ age + lwt,data = bwt_est),
  mod_2 = lm(bwt ~ age + lwt + race,data = bwt_est),
  mod_3 = lm(bwt ~ age + lwt + race + smoke,data = bwt_est),
  mod_4 = lm(bwt ~ ., data = bwt_est)
)
#model with lowest RMSE in validation (changes w new problem)
mod_final = lm(bwt ~ age + lwt,data = bwt_trn)

#funciton for RMSE
calc_rmse = function(actual,predicted) {
  sqrt(mean((actual-predicted)^2))
}
#calculation for validation RMSE [est to val]
val_pred = map(mod_list, predict, bwt_val)
map_dbl(val_pred,calc_rmse, actual = bwt_val$bwt)
#RMSETest [trn to test]
calc_rmse(actual = bwt_tst$bwt,predicted = predict(mod_final,bwt_tst))

#10------------------------------------
fitting = lm(bwt ~ age + lwt + smoke, data = bwt_trn)
#predicted values, smoker is 2, non-somer is 1
newdata = data.frame(age=36,lwt=132,smoke=factor(levels(bwt_trn$smoke)))
predict(fitting,newdata)/1000

#11-------------------------------------
newdataPredict = data.frame(age=35,lwt=137,smoke=factor(levels(bwt_trn$smoke)))
predict(fitting,newdataPredict,interval = "confidence", level = .91)
predict(fitting,newdataPredict,interval = "prediction",level = .96)
