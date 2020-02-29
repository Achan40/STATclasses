#Problem 2
#Part 1
#create data table
belief = data.frame(Education = c("1.K-12","1.K-12","1.K-12","2.13-18","2.13-18","2.13-18","3.19 or greater","3.19 or greater","3.19 or greater"),
                    Fund = c("1.Fundamentalist","2.Moderate","3.Liberal","1.Fundamentalist","2.Moderate","3.Liberal","1.Fundamentalist","2.Moderate","3.Liberal"),
                    value  =c(307,461,270,262,508,472,16,56,69))
belief_tab = xtabs(value ~ Education + Fund, data=belief)
belief_tab #Manually ordered x and y so they would appear in the correct order

margin.table(belief_tab,1)
margin.table(belief_tab,2)

#Expected frequencies under independence
muhat_belief = outer(margin.table(belief_tab,1),margin.table(belief_tab,2))/sum(belief_tab)
muhat_belief

#chi-sq test
X2_belief = chisq.test(belief_tab,correct = F)
X2_belief

#Gsquared
Gsq_belief = 2*sum(belief_tab*log(belief_tab/muhat_belief))
Gsq_belief

1-pchisq(Gsq_belief,df=(nrow(belief_tab)-1)*(ncol(belief_tab)-1))
#reject the null hypothesis for independence of Fund and Education based on Gsq and Chisq since the p-value for chi-sq is much less than alpha=5%

#Part2
round(X2_belief$residuals,2)#Pearson Residuals
round(X2_belief$stdres,2)#Standardized Residuals
mosaicplot(belief_tab,shade = T)#Mosaic Plot
#It can be seen that the pearson and standardized residuals of this data set behave similarly
#From the residuals it can be seen that those with lower education generally are more fundamentalist that expected (positive residuals) under the assumption of independence
#It also appears that people with K-12 education and are liberal are less fundamentalist than expected(negative residuals) under the indpendence assumption
#from the mosaic plot, our findigns are the same, liberals with K-12 education are much less fundamentalist than expected, which is visually depicted by a deep red color, no other categories have a color this striking. 

#Analysis of K-12 and 13-18
belief1=belief[1:6,]
belief1

belief1$Education = factor(belief1$Education,levels = levels(belief1$Education)[1:2])
belief_tab1 = xtabs(value ~ Education+Fund, data = belief1)
belief_tab1

muhat_belief1 = outer(margin.table(belief_tab1,1),margin.table(belief_tab1,2))/sum(belief_tab1)
muhat_belief1

#chisqtest 
X2_belief1 = chisq.test(belief_tab1,correct = F)
X2_belief1

#Gsquared
Gsq_belief1 = 2*sum(belief_tab1*log(belief_tab1/muhat_belief1))
Gsq_belief1

1-pchisq(Gsq_belief1,df=(nrow(belief_tab1)-1)*(ncol(belief_tab1)-1))
#only considering K-12 and 13-18, we can reject the indenpendence hypothesis as well since the p-value for chi-sq is much less than alpha=5%


#Problem 3
#Part 1
#create data table
ID = data.frame(Party_ID = c("Democrat","Independent","Republican","Democrat","Independent","Republican"),
                    Race = c("Non-White","Non-White","Non-White","White","White","White"),
                    value = c(192,75,8,459,586,471))
ID_tab = xtabs(value ~ Race+Party_ID, data=ID)
ID_tab

margin.table(ID_tab,1)
margin.table(ID_tab,2)

#Expected frequencies under independence
muhat_ID = outer(margin.table(ID_tab,1),margin.table(ID_tab,2))/sum(ID_tab)
muhat_ID

#chi-sq test
X2_ID = chisq.test(ID_tab,correct = F)
X2_ID

#Gsquared
Gsq_ID = 2*sum(ID_tab*log(ID_tab/muhat_ID))
Gsq_ID

1-pchisq(Gsq_ID,df=(nrow(ID_tab)-1)*(ncol(ID_tab)-1))
#reject the null hypothesis for independence based on Gsq and Chisq since the p-value for chi-sq is zero

#Part2
round(X2_ID$residuals,2)#Pearson Residuals
round(X2_ID$stdres,2)#Standardized Residuals
mosaicplot(ID_tab,shade = T)#Mosaic Plot
#From the Pearson residuals, we can see that number of non-white Democrates are higher than expected under the independence assumption (very positive residuals). Non-white republicans are mildly lower than expected under the independence assumption(negative residuals)
#from the mosaic plot, we can see that the propoprtion of non-white democrats are very large, and their residuals are also strongly positive as well. We can also see that non-white republicans are very small in number and are much lower than expected (strongly negative residuals) under the assumption of independence

#Analysis of white and non-white
ID1=ID[1:6,]
ID1

ID1$Party_ID = factor(ID1$Party_ID,levels = levels(ID1$Party_ID)[1:2])
ID_tab1 = xtabs(value ~ Party_ID+Race, data = ID1)
ID_tab1

muhat_ID1 = outer(margin.table(ID_tab1,1),margin.table(ID_tab1,2))/sum(ID_tab1)
muhat_ID1

#chisqtest 
X2_ID1 = chisq.test(ID_tab1,correct = F)
X2_ID1

#Gsquared
Gsq_ID1 = 2*sum(ID_tab1*log(ID_tab1/muhat_ID1))
Gsq_ID1

1-pchisq(Gsq_ID1,df=(nrow(ID_tab1)-1)*(ncol(ID_tab1)-1))
#considering non-whites and whites, we can reject the indenpendence hypothesis as well since the p-value for chi-sq is much less than alpha=5%


#Problem 4
Fish = data.frame(Malformation=c("Yes","Yes","No","No"),
                       Lead=c("Yes","No","Yes","No"),
                       Affected=c(7,7,7,18))

Fish_tab = xtabs(Affected ~ Lead + Malformation, data=Fish)
Fish_tab
fisher.test(Fish_tab,alternative = "greater")
#Fishers exact test returns a pvalue of 0.1526
#at this p-value, we cannot reject H0:(odds ratio)theta=1 variables are independent. 
#At a 95% confidence level we cannot say that there is a greater chance of malformation when there is lead pollution.
