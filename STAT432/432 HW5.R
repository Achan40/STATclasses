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

#5.14
