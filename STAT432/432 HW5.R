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
