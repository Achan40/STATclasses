#4.1
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