#Problem 1
#create matrix of data
tab1 = matrix(c(955,9,162,188),2,2)
#include totals in margins
tab = addmargins(tab1)
tab

#a
d = ((tab[1,1]+tab[2,1])-tab[1,3])/tab[3,3]
sd = sqrt(((tab[1,2]/tab[3,3])+(tab[2,1]/tab[3,3])-((tab[1,2]/tab[3,3])-(tab[2,1]/tab[3,3]))^2)/tab[3,3])
d + c(-1,1)*1.96*sd
#95% CI: -0.1349,-0.09798
#The interval does not contain 0, therefore, the difference between marginal proportions are statistically signifcant at alpha=.05

#b
#Cacluating by hand
((tab[1,2]-tab[2,1])^2)/(tab[1,2]+tab[2,1])
#Using function
mcnemar.test(tab1,correct=FALSE)
#H0: marginal proportions are equal vs Ha: marginal proportions are not equal
#P-value is very small, we can conclude at the alpha=.05 level that marginanl proportions are not equal

#c
#The pairs of data are matched.
#We are dealing with dichotomous variables where one independent is shared bewteen the connected groups, and the groups in our dependent are mutually exclusive from each other
#These facts suggest that the marginal proportions are strongly dependent.
#The infrences in part a are more precise than if we hand independent samples because the dependent samples allow us to form a relationship between the two groups

#Problem 2
#a
exp(log((tab[3,1]/tab[3,2])/(tab[1,3]/tab[2,3])))

#b
exp(log(tab[2,1]/tab[1,2]))

#c
#The population-averaged effect model has an estimated odds ratio larger than the subject-specific effect model
#The first model accounts for totality, while the subject-specific model accounts for the "false positives" and "false negatives"