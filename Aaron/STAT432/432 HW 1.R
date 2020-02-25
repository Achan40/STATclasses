dnorm(4.23,mean = 3.5,sd=2.9)
dnorm(3.13,mean = 3.5,sd=2.9)
dnorm(3.13,mean=6.6,sd=2.3)
dnorm(7.68,mean=6.6,sd=2.3)
#2
a = 4.3
b = 3.8
dnorm(-.96,mean = a,sd = b)
pnorm(-.96,mean = a,sd = b,lower.tail = TRUE)
pnorm(7.24,mean = a,sd = b,lower.tail = TRUE)-pnorm(10.82,mean = a,sd = b,lower.tail = TRUE)
pnorm(10.9,mean = a,sd = b,lower.tail = FALSE)


#3
a=6.8
b=.7
qnorm(.72,mean = a,sd=b)
qnorm(.26,mean = a,sd=b,lower.tail = FALSE)
#4
sqrt(16+4*1.5^2+64*9)
#5
a=6.2
dpois(3,lambda = a)
ppois(4,lambda =a ,lower.tail = TRUE)-ppois(0,lambda =a ,lower.tail = TRUE)
ppois(6,lambda =a ,lower.tail = FALSE)
#6
set.seed(53261)
some_data = some_data = rexp(n=1000,rate = 2.5)
mean(some_data)
median(some_data)
var(some_data)
sd(some_data)
#7
library("mlbench")
library("tibble")
set.seed(88104)
some_data = as_tibble(mlbench.2dnormals(n = 500, cl = 3))
sum(some_data$classes==3)
sd(some_data$x.1)
sum(some_data$x.2>.5)/nrow(some_data)
#8
#9
# set seed 
library("Metrics")
set.seed(50224)
Actual = rnorm(n = 500, mean = 5, sd = 2)
predicted = rnorm(n = 500, mean = 4, sd = 2)
rmse(y,x)
mae(y,x)

(.4*.05)/.343

calc_RMSE = function(Actual,predicted){
  sqrt(mean((Actual-predicted)^2))
}
calc_MAE = function(Actual,predicted){
  mean(abs(Actual-predicted))
}

calc_RMSE(Actual,predicted)
calc_MAE(Actual,predicted)
