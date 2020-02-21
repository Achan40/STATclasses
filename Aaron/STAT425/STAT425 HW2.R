#Problem 1
library(faraway)
data("sat")
head(sat)

#a
sat_model1 = lm(total ~ expend+ratio+salary,data = sat)
summary(sat_model1)

#b
sat_model2 = lm(total ~ expend+ratio+salary+takers,data = sat)
summary(sat_model2)

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

#b
qf(.95,4,8)

#c

#d
CR95 = ellipse(punt_model1,c(2,3))#create confidence region for age&lbph

myCR = data.frame(CR95);
names(myCR) = c("RStr","LStr");
myCR[,'level']=as.factor(rep(.95,dim(CR95)[1]));

ggplot(data=myCR, aes(x=RStr, y=LStr, colour=level)) + 
  geom_path(aes(linetype=level), size=1.5) + 
  geom_point(x=coef(punt_model1)[4], y=coef(punt_model1)[5], shape=3, size=3, colour='red') + 
  geom_point(x=0, y=0, shape=1, size=3, colour='red')

#e
punt_model2 = lm(Distance ~ I(RStr+LStr)+RFlex+LFlex,data = punting)

#Problem 5

#a
pro_model3 = lm(lpsa ~.,data = prostate)
predictdata = data.frame(lcavol=1.44692,lweight=3.62301,age=65,lbph=.3001,svi=0,lcp=-.79851,gleason=7,pgg45=15)
predict(pro_model3,predictdata,interval = "confidence",level = .95)

#b
predictdata2 = data.frame(lcavol=1.44692,lweight=3.62301,age=20,lbph=.3001,svi=0,lcp=-.79851,gleason=7,pgg45=15)
predict(pro_model3,predictdata2,interval = "confidence",level = .95)

#c
summary(pro_model3)#remove age,lbph,lcp,gleason,pgg45
pro_model4 = lm(lpsa ~ lcavol+lweight+svi,data = prostate)

predictdata = data.frame(lcavol=1.44692,lweight=3.62301,svi=0)
predict(pro_model4,predictdata,interval = "confidence",level = .95)
