dnorm(4.23,mean = 3.5,sd=2.9)
dnorm(3.13,mean = 3.5,sd=2.9)
dnorm(3.13,mean=6.6,sd=2.3)
dnorm(7.68,mean=6.6,sd=2.3)
#2
a = 4.5
b = 4.8
pnorm(15.27,mean = a,sd = b,lower.tail = TRUE)
pnorm(-3.4,mean = a,sd = b,lower.tail = TRUE)-pnorm(-8.37,mean = a,sd = b,lower.tail = TRUE)
pnorm(.48,mean = a,sd = b,lower.tail = FALSE)
#3
a=4.4
b=4.7
qnorm(.76,mean = a,sd=b)
qnorm(.78,mean = a,sd=b)
#4
sqrt(16+4*1.5^2+64*9)
#5
a=8.4
dpois(2,lambda = a)
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
set.seed(59532)
x = rnorm(n = 500, mean = 5, sd = 2)
y = rnorm(n = 500, mean = 4, sd = 2)
rmse(y,x)
mae(y,x)

(.4*.05)/.343
