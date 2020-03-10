options(warn=-1)
suppressPackageStartupMessages({
  library(faraway)
  library(car)
  library(MASS)
  library(lmtest)
})

#Problem 1
data(sat)

#a
sat.lin = lm(total ~ expend + salary + ratio + takers, data = sat)
#using standard r
plot(fitted(sat.lin), residuals(sat.lin), xlab = "Fitted", ylab = "Residuals", abline(h=0))
#using the car package for a better visualization of studentized residuals
spreadLevelPlot(sat.lin)
#checking homocedasticity
bptest(sat.lin)
sat.lin.test = lm(sat.lin$res^2 ~ expend + salary + ratio + takers, data = sat)
summary(sat.lin.test)$r.sq*nrow(sat)

#From the plots, we can see that errors do have constant variance, this can be seen better by taking a look at the studentized residuals.
#We fail to reject the null hypothesis of homocedasticity, since the BP test is equal to our computed value

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
sat.lin = lm(total ~ expend + salary + ratio + takers, data = sat)
summary(sat.lin)

#remove the effect of each variable and fit a regression on the residuals
r.yExpend = update(sat.lin, ~ expend + salary + ratio + takers - expend)$res;
r.Expend = lm(expend ~ expend + salary + ratio + takers, data = sat[,-7])$res;
tmp1 = lm(r.yExpend ~ r.Expend)

r.ySalary = update(sat.lin, ~ expend + salary + ratio + takers - salary)$res;
r.Salary = lm(salary ~ expend + salary + ratio + takers, data = sat[,-7])$res;
tmp2 = lm(r.ySalary ~ r.Salary)

r.yRatio = update(sat.lin, ~ expend + salary + ratio + takers - ratio)$res;
r.Ratio = lm(ratio ~ expend + salary + ratio + takers, data = sat[,-7])$res;
tmp3 = lm(r.yRatio ~ r.Ratio)

r.yTakers = update(sat.lin, ~ expend + salary + ratio + takers - takers)$res;
r.Takers = lm(takers ~ expend + salary + ratio + takers, data = sat[,-7])$res;
tmp4 = lm(r.yTakers ~ r.Takers)

par(mfrow = c(2,2))
plot(r.Expend, r.yExpend, xlab="Expend residuals", ylab="savings residuals"); abline(tmp1)
plot(r.Salary, r.ySalary, xlab="Salary residuals", ylab="savings residuals"); abline(tmp2)
plot(r.Ratio, r.yRatio, xlab="Ratio residuals", ylab="savings residuals"); abline(tmp3)
plot(r.Takers, r.yTakers, xlab="Takers residuals", ylab="savings residuals"); abline(tmp4)

coefficients(sat.lin)
coef(tmp1)[2]
coef(tmp2)[2]
coef(tmp3)[2]
coef(tmp4)[2]
#The slope for the regression lines fitted to the residuals after removing the effect of each variable one-at-a-time, is the same as the regression coefficient for that variable in the full model

#Problem 2
data(teengamb)

#a
par(mfrow=c(1,1))
tg.lin = lm(gamble ~., data = teengamb)
#using standard r
plot(fitted(tg.lin), residuals(tg.lin), xlab = "Fitted", ylab = "Residuals", abline(h=0))
#using the car package for a better visualization of studentized residuals
spreadLevelPlot(tg.lin)
#checking homocedasticity
bptest(tg.lin)
tg.lin.test = lm(tg.lin$res^2 ~ ., data = teengamb)
summary(tg.lin.test)$r.sq*nrow(teengamb)

#From the plots, we can see that errors do not have constant variance, this can be seen better by taking a look at the studentized residuals.
#We can reject the null hypothesis of homocedasticity, since the BP test is not equal to our computed value
#To get rid of heteroscedasticity, we may have to perform some sort transformation on the data our studentized residuals plot suggests a power transformation of 0.1646836.

#b
#normal qq plot of the residuals
qqnorm(residuals(tg.lin), ylab = "Residuals")
qqline(residuals(tg.lin))
#Histogram of the residuals
sresid = studres(tg.lin)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit = seq(min(sresid),max(sresid),length=40)
yfit = dnorm(xfit)
lines(xfit, yfit)
#sharpio-wilks test
shapiro.test(residuals(tg.lin))
#From the normal qq plot and the histogram of the studentized residuals, visually we can see that they are not normally distributed
#The result of sharpio-wilks testing H0 = data is normally distributed vs Ha = data is not normally distributed, shows we can reject the H0, data is not normally distributed.
#To fix this issue, we may have to tranform the Y variable, or normalize the data.

#c
#lisiting potential influential observations
tg.lin.hat = hatvalues(tg.lin)
#hat values > 2*(k+1)/n
tg.lin.hat.val = which(tg.lin.hat > (2*(4+1)/nrow(sat))) 
#list of high leverage points with values
tg.lin.hatsorted = tg.lin.hat[tg.lin.hat.val];sort(tg.lin.hatsorted,decreasing = T)[1:4]
#graph
lev = influence(tg.lin)$hat
halfnorm(lev, 4, labs = row.names(teengamb), ylab="Leverages")
#The largest four leverage points are 35,42,31, and 33. 
#This can be seen graphically as well as by calculation of hat values.

#d
#plot of studentized residuals
plot(rstudent(tg.lin), ylab = "Studentized Residuals")
#list of largest studentized residuals, largest outliers
sr.ex = rstudent(tg.lin);sort(sr.ex,decreasing = T)[1:3]
#The 24th, 36th and 5th data points seem to be the largest outliers.
#May want to consider removing these data points for a better model. The 24th data point appears to be an exceptional outlier, with a studentized residual value > 6.
#It is highly reccomended to remove that data point to create a more accurate model

#e
cook = cooks.distance(tg.lin)
halfnorm(cook, 3,labs = row.names(teengamb), ylab = "Cook's distance")
max(cook)
#The 24th data point seems to be throwing off our data quite a bit (largest cooks distance value), most influential data point. 
#We may be able to get a better model if we remove that data row and refit the model without that higly influential point.
#It makes sense to remove the 24th point as it was also a large leverage point.

#f
tg.lin = lm(gamble ~., data = teengamb)
summary(tg.lin)

#remove the effect of each variable and fit a regression on the residuals
r.ysex = update(tg.lin, ~ . - sex)$res;
r.sex = lm(sex ~ ., data = teengamb[,-5])$res;
tmp1 = lm(r.ysex ~ r.sex)

r.ystatus = update(tg.lin, ~ . - status)$res;
r.status = lm(status ~ ., data = teengamb[,-5])$res;
tmp2 = lm(r.ystatus ~ r.status)

r.yincome = update(tg.lin, ~ . - income)$res;
r.income = lm(income ~ ., data = teengamb[,-5])$res;
tmp3 = lm(r.yincome ~ r.income)

r.yverbal = update(tg.lin, ~ . - verbal)$res;
r.verbal = lm(verbal ~ ., data = teengamb[,-5])$res;
tmp4 = lm(r.yverbal ~ r.verbal)

par(mfrow = c(2,2))
plot(r.sex, r.ysex, xlab="sex residuals", ylab="savings residuals"); abline(tmp1)
plot(r.status, r.ystatus, xlab="status residuals", ylab="savings residuals"); abline(tmp2)
plot(r.income, r.yincome, xlab="income residuals", ylab="savings residuals"); abline(tmp3)
plot(r.verbal, r.yverbal, xlab="verbal residuals", ylab="savings residuals"); abline(tmp4)

coefficients(tg.lin)
coef(tmp1)[2]
coef(tmp2)[2]
coef(tmp3)[2]
coef(tmp4)[2]
#The slope for the regression lines fitted to the residuals after removing the effect of each variable one-at-a-time, is the same as the regression coefficient for that variable in the full model
