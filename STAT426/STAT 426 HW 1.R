#problem 1
n = 30
#1a
chi = 3.84 #by chi square table
1-exp(-1/(2*n)*chi)
#1b
Z = qnorm(.025,mean = 0,sd=1)
Z2 = Z^2
.5*(Z2/(n+Z2))+Z*sqrt(1/(n+Z2)*((.5)*(.5)*(Z2/(n+Z2)))) #lower
.5*(Z2/(n+Z2))-Z*sqrt(1/(n+Z2)*((.5)*(.5)*(Z2/(n+Z2)))) #upper
