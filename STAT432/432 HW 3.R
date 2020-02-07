#3.1
prior = c(.19,.53,.28)
prior2 = c(2.7,5.7,7)
X = 5

prior[2]*pexp(X,prior2[2],lower.tail = FALSE)
pX = sum(prior*pexp(X,prior2,lower.tail = FALSE))
sum(prior*pexp(X,prior2,lower.tail = FALSE))
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

# set seed 
set.seed(77851)

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

# check data (visually)
# plot(sim_trn, pch = 20, col = "darkgrey")
# grid()

mod_1 = lm(y ~ x,data = sim_trn)
mod_2 = lm(y ~ I(2^(x-1)),data = sim_trn)


#3.7

