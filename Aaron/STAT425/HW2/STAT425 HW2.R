#Problem 1
library(faraway)
data("sat")
head(sat)

#a
sat_model1 = lm(total ~ expend+ratio+salary,data = sat)
mod1sum = summary(sat_model1)

#H0: Bsalary = 0
tStat= (mod1sum$coef[4,1]-0)/mod1sum$coef[4,2]#Bsalaryhat and its sterror
pVal= dt(tStat,df=length(sat)-1)
pVal
#Do not reject H0 at alpha=0.05

#H0: Bsalary=Bratio=Bexpend=0
summary(sat_model1)
#p-value = 0.01209 since p-value of 0.01209 is less than alpha=0.05, we can reject H0, and we can say that there is at least one coefficient that is different from 0
#At least one of these predictors has an effect on the response because H0 was rejected due to the p-value being less than 0.05

#b
sat_model2 = lm(total ~ expend+ratio+salary+takers,data = sat)
mod2sum = summary(sat_model2)
#H0: Btakers = 0
tStat2= (mod2sum$coef[5,1]-0)/mod2sum$coef[5,2]#Btakershat and its sterror
pVal2= dt(tStat2,df=length(sat)-1)
pVal2
#Reject H0 at alpha=0.05
#The new model has a p-value that is essentially 0 even with an extra variable, meaning that it is better than the first model
#the Ftest is the Ttest squared
tStat2^2
anova(sat_model2)$"F value"[4]#F value for takers
#The F value for takers is 157.7379, which is essentially our previous t-statistic of squared: (-12.557)^2=157.7379

#Problem 2
data("prostate")
head(prostate)
pro_model1 = lm(lpsa~.,data = prostate)

#a
confint(pro_model1,'age',level=.90)
confint(pro_model1,'age',level=.95)

#b
summary(pro_model1)#remove age,lbph,lcp,gleason,pgg45
pro_model2 = lm(lpsa ~ lcavol+lweight+svi,data = prostate)
summary(pro_model2)

#c
library(ellipse)
library(ggplot2)
CR95 = ellipse(pro_model1,c(4,5))#create confidence region for age&lbph

myCR = data.frame(CR95);
names(myCR) = c("age","lbph");
myCR[,'level']=as.factor(rep(.95,dim(CR95)[1]));

ggplot(data=myCR, aes(x=age, y=lbph, colour=level)) + 
  geom_path(aes(linetype=level), size=1.5) + 
  geom_point(x=coef(pro_model1)[4], y=coef(pro_model1)[5], shape=3, size=3, colour='red') + 
  geom_point(x=0, y=0, shape=1, size=3, colour='red')
#This is an F test, since the origin is withing the region, we do not reject H0: age = lbph we cannot say there is a difference in means

#d
#permutation test corresponding to the t-test for age
n.iter = 2000; 
tstats = numeric(n.iter);
for(i in 1:n.iter){
  newprostate=prostate;
  newprostate[,c(4)]=savings[sample(97),c(4)];
  g = lm(lpsa ~., data=newprostate);
  tstats[i] = summary(g)$coef[4,3]
}
length(tstats[tstats > summary(pro_model1)$coef[4,3]])/n.iter

#Problem 3

#a
data("punting")
head(punting)
punt_model1 = lm(Distance ~ RStr+LStr+RFlex+LFlex,data = punting)
summary(punt_model1)
qt(.90,df=length(punting)-1,lower.tail=TRUE)
qt(.10,df=length(punting)-1,lower.tail=TRUE)
#at the 5% level, only RFlex is a significant predictor

#b
qf(.95,4,8)
summary(punt_model1)$fstat
#collectively, these variable are significant since Fstatistic for the model is further than Fvalue at the 5% level

#c
#H0: RStr = LStr
RStrmu= mean(punting$RStr)
RStrsd= sd(punting$RStr)
LStrmu= mean(punting$LStr)
LStrsd= sd(punting$LStr)
n1= length(punting$RStr)
n2= length(punting$LStr)

sPooledsq= (((n1-1)*RStrsd^2)+((n2-1)*LStrsd^2))/(n1+n2-2)
tvalPool= (RStrmu-LStrmu)/(sqrt(sPooledsq*((1/n1)+(1/n2))))
tvalPool
pvalPool= dt(tvalPool, df=n1+n2-2)
pvalPool
#Given a pval of 0.3589, we can't reject H0 at the 5% level, meaning that right and left strength could have the same effect.

#d
CR95 = ellipse(punt_model1,c(2,3))#create confidence region for age&lbph

myCR = data.frame(CR95);
names(myCR) = c("RStr","LStr");
myCR[,'level']=as.factor(rep(.95,dim(CR95)[1]));

ggplot(data=myCR, aes(x=RStr, y=LStr, colour=level)) + 
  geom_path(aes(linetype=level), size=1.5) + 
  geom_point(x=coef(punt_model1)[4], y=coef(punt_model1)[5], shape=3, size=3, colour='red') + 
  geom_point(x=0, y=0, shape=1, size=3, colour='red')
#The confidence region tests H0: RStr = LStr, since the origin is within the region, we can see that we can't reject H0

#e
punt_model2 = lm(Distance ~ I(RStr+LStr)+RFlex+LFlex,data = punting)
summary(punt_model2)

#Problem 5

#a
pro_model3 = lm(lpsa ~.,data = prostate)
predictdata = data.frame(lcavol=1.44692,lweight=3.62301,age=65,lbph=.3001,svi=0,lcp=-.79851,gleason=7,pgg45=15)
predict(pro_model3,predictdata,interval = "confidence",level = .95)

#b
predictdata2 = data.frame(lcavol=1.44692,lweight=3.62301,age=20,lbph=.3001,svi=0,lcp=-.79851,gleason=7,pgg45=15)
predict(pro_model3,predictdata2,interval = "confidence",level = .95)
#Wider CI since maybe not much data is known for younger males w prostate cancer as it does not happen to younger people often

#c
summary(pro_model3)#remove age,lbph,lcp,gleason,pgg45
pro_model4 = lm(lpsa ~ lcavol+lweight+svi,data = prostate)

predictdata = data.frame(lcavol=1.44692,lweight=3.62301,svi=0)
predict(pro_model4,predictdata,interval = "confidence",level = .95)
#The confidence interval is tigher now, I would prefer this one as it does not depend on age (a predictor that we may not have a lot of data for)