library(faraway)
library(car)
library(MASS)
library(lmtest)
#Problem 1
data(sat)

#a
sat.lin = lm(total ~ expend+salary+ratio+takers, data = sat)
#using standard r
plot(fitted(sat.lin), residuals(sat.lin), xlab = "Fitted", ylab = "Residuals", abline(h=0))
#using the car package for a better visualization of studentized residuals
spreadLevelPlot(sat.lin)
#checking homocedasticity
bptest(sat.lin)
sat.lin.test = lm(sat.lin$res^2 ~ expend + salary + ratio + takers, data = sat)
summary(sat.lin.test)$r.sq*nrow(sat)

#From the plots, we can see that errors do not have constant variance, this can be seen better by taking a look at the studentized residuals.
#We fail to reject the null hypothesis of homocedasticity, since the BP test is equal to our computed value
#To get ride of heteroscedasticity, we may have to perform some sort transformation on the data (perhaps a log?).

#b
#normal qq plot of the residuals
qqnorm(residuals(sat.lin), ylab = "Residuals")
qqline(residuals(sat.lin))
#Histogram of the residuals
sresid = studres(sat.lin)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit = seq(min(sresid),max(sresid),length=40)
yfit = dnorm(xfit)
lines(xfit, yfit)
#sharpio-wilks test
shapiro.test(residuals(sat.lin))
#From the normal qq plot and the histogram of the studentized residuals, visually we can see that they are normally distributed
#The result of sharpio-wilks testing H0 = data is normally distributed vs Ha = data is not normally distributed, shows we can accept the H0, data is normally distributed.

#c
#lisiting potential influential observations
sat.lin.hat = hatvalues(sat.lin)
#hat values > 2*(k+1)/n
sat.lin.hat.val = which(sat.lin.hat > (2*(4+1)/nrow(sat))) 
#list of high leverage points with values
sat.lin.hatsorted = sat.lin.hat[sat.lin.hat.val];sort(sat.lin.hatsorted,decreasing = T)[1:4]
#graph
lev = influence(sat.lin)$hat
halfnorm(lev, 4, labs = row.names(sat), ylab="Leverages")
#The largest four leverage points are Utah,Cali,Conneticut, and NewJersey. 
#This can be seen graphically as well as by calculation of hat values.

#d
#plot of studentized residuals
plot(rstudent(sat.lin), ylab = "Studentized Residuals")
#list of largest studentized residuals, largest outliers
sr.ex = rstudent(sat.lin);sort(sr.ex,decreasing = T)[1:5]
#The Utah, North Dakota, and New Hampshire data seem to be the largest outliers.
#May want to consider removing these data points for a better model.

#e
cook = cooks.distance(sat.lin)
halfnorm(cook, 3,labs = row.names(sat), ylab = "Cook's distance")
max(cook)
#Utah seems to be throwing off our data quite a bit (larges cooks distance value), most influential data point. 
#We may be able to get a better model if we remove the Utah data row and refit the model without that higly influential points.
#It makes sense to remove the Utah data as it was also an outlier, and a large leverage point as well as the highest influential point.
#Also consider removing New Hampshire as well as it is an influential point as well as an outlier. 

#f
