#3.1
prior = c(.28,.4,.32)
prior2 = c(6.3,6.6,4.3)
X = 3

prior[2]*pexp(X,prior2[2],lower.tail = FALSE)
pX = sum(prior*pexp(X,prior2,lower.tail = FALSE))
pX
pexp(X,prior2[2],lower.tail = FALSE)*prior[2]/pX

#3.2
# set seed 
set.seed(90375)

# generate data from known bernoulli distribution
bern_data = rbinom(n = 250, size = 1, prob = 0.25)

# check data
head(bern_data, n = 10)

binom.loglik = function(parms,bern_data){
  
  out = dbinom(bern_data,1,parms,log=T)  # This evaluates the log of the bernoulli probability for each observation.
  
  return(sum(out))   # We sum it instead of taking the product because we have log-likelihood. 
  
}

binom.loglik(0.78,bern_data) 
binom.loglik(0.78,bern_data) 
binom.loglik(0.47,bern_data) 

#3.5
# load packages
library("tidyverse")

# set seed 
set.seed(31150)

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

#2 is smoke, 1 is non smoke
mod_1 = lm(bwt ~ lwt + age + smoke, data = bwt_trn)
prediction = data.frame(age=29,lwt=118,smoke=factor(levels(bwt_trn$smoke)))
predict(mod_1,prediction)/1000

#3.6&3.7
# load packages
library("tibble")
library("caret")
library("rpart")
# set seed 
set.seed(65518)

# define function to simulate data
gen_nonlin_data = function(sample_size = 200) {
  x = runif(n = sample_size, min = 0, max = 10)
  mu = 0 + 3 * 2 ^ (x - 1)
  eps = rnorm(n = sample_size, mean = 0, sd = 100)
  y = mu + eps
  tibble(x, y)
}

# simulate (training) data
sim_trn = gen_nonlin_data()

# check data (numerically)
head(sim_trn)

#create models
mod_list = list(
  mod_1 = lm(y ~ x,data = sim_trn),
  mod_2 = lm(y ~ I(2^(x-1)),data = sim_trn),
  mod_3 = knnreg(y~x,data=sim_trn,k=20),
  mod_4 = rpart(y~x, data=sim_trn)
)
X = 6
prediction1 = data.frame(x=X)
true_est = 0 + 3*2^(X-1)

predictionS = map(mod_list,predict,prediction1)
predictionS$mod_1-true_est
predictionS$mod_2-true_est
predictionS$mod_3-true_est
predictionS$mod_4-true_est

#3.8
# set seed 
set.seed(72280)

# define function to simulate data
gen_nonlin_data = function(sample_size = 200) {
  x = runif(n = sample_size, min = 0, max = 10)
  mu = 0 + 3 * 2 ^ (x - 1)
  eps = rnorm(n = sample_size, mean = 0, sd = 100)
  y = mu + eps
  tibble(x, y)
}

# simulate data
sim_est = gen_nonlin_data(sample_size = 200)
sim_val = gen_nonlin_data(sample_size = 50)
sim_trn = rbind(sim_est, sim_val)
sim_tst = gen_nonlin_data(sample_size = 50)

#create models
mod_list = list(
  mod_1 = lm(y ~ x,data = sim_est),
  mod_2 = lm(y ~ I(2^(x-1)),data = sim_est),
  mod_3 = knnreg(y~x,data=sim_est,k=20),
  mod_4 = rpart(y~x, data=sim_est)
)

#RMSE function
calc_RMSE = function(act,pred){
  sqrt(mean((act-pred)^2))
}
#calculation for validation RMSE [est to val]
val_pred = map(mod_list, predict, sim_val)
map_dbl(val_pred,calc_RMSE, act = sim_val$y)

#RMSETest [trn to test] model w lowest RMSE
mod_final = mod_2 = lm(y ~ I(2^(x-1)),data = sim_trn)
calc_RMSE(act = sim_tst$y,pred = predict(mod_final,sim_tst))

#3.9&3.10&3.11&3.12&3.13
# set seed 
set.seed(16159)

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

#KNN prediction
mod_k = knnreg(bwt~lwt + age + smoke,data = bwt_trn,k=19)
predictK = data.frame(age = 31, lwt = 124, smoke = factor(levels(bwt_trn$smoke)))
predict(mod_k,predictK)

#descision tree prediction
tree_1 = rpart(bwt~lwt + age + smoke,data = bwt_trn,cp = .5)
predictTree = data.frame(age = 29, lwt = 132, smoke = factor(levels(bwt_trn$smoke)))
predict(tree_1,predictTree)

#Linear and KNN
mods = list(
  mod_inter = lm(bwt~1,data = bwt_est),#model w only an intercept
  mod_k = knnreg(bwt~lwt + age + smoke,data = bwt_trn,k=count(bwt_est))
)

mom1 = data.frame(age = 33, lwt = 129,smoke=factor(levels(bwt_est$smoke)))
mom2 = data.frame(age = 29, lwt = 118,smoke=factor(levels(bwt_est$smoke)))
mom3 = data.frame(age = 37, lwt = 113,smoke=factor(levels(bwt_est$smoke)))

predict(mods,mom1)
predict(mods,mom2)
predict(mods,mom3)

#KNN all variables
mod_k_all = knnreg(bwt~., data = bwt_est,k=1)#have to turn mod_k_all into a list to work w map function
#Validation RMSE [est to val]
calc_RMSE(act = bwt_val$bwt,pred = predict(mod_k_all,bwt_val))
#TrainRMSE [est to est in this case]  
calc_RMSE(act = bwt_est$bwt,pred = predict(mod_k_all,bwt_est))

#descision tree all variable
dtree_all = rpart(bwt~., data = bwt_est, cp=0,minsplit=2)
#Validation RMSE [est to val]
calc_RMSE(act = bwt_val$bwt,pred = predict(dtree_all,bwt_val))
#TrainRMSE [est to est in this case] since only one model 
calc_RMSE(act = bwt_est$bwt,pred = predict(dtree_all,bwt_est))

#KNN all variables different K, least flex, highest K 
mod_list = list(
  mod_k_all1 = knnreg(bwt~.,data = bwt_est, k=14),
  mod_k_all2 = knnreg(bwt~.,data = bwt_est, k=26),
  mod_k_all3 = knnreg(bwt~.,data = bwt_est, k=48)
)

#calculation for validation RMSE [est to val]
val_pred = map(mod_list, predict, bwt_val)
map_dbl(val_pred,calc_RMSE, act = bwt_val$bwt)
#RMSETest [trn to tst] lowest RMSE
mod_final = knnreg(bwt~.,data = bwt_trn, k=48)
calc_RMSE(act = bwt_tst$bwt,pred = predict(mod_final,bwt_tst))

#Descision Tree all variables, different cp
mod_list = list(
  mod_t_all1 = rpart(bwt~.,data = bwt_est, cp=.1, minsplit = 2),
  mod_t_all2 = rpart(bwt~.,data = bwt_est, cp=.01, minsplit = 2),
  mod_t_all3 = rpart(bwt~.,data = bwt_est, cp=.001, minsplit = 2)
)
#calculation for validation RMSE [est to val] least flexible is highest cp
val_pred = map(mod_list, predict, bwt_val)
map_dbl(val_pred,calc_RMSE, act = bwt_val$bwt)
#RMSETest [trn to tst] lowest RMSE
mod_final = rpart(bwt~.,data = bwt_trn, cp=.1, minsplit = 2)
calc_RMSE(act = bwt_tst$bwt,pred = predict(mod_final,bwt_tst))
