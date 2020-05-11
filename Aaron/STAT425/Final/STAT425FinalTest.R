library(faraway)
library(randomForest)
library(car)
library(leaps)
setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT425/Final")

Resort = data.frame(read.csv(file = "stat425_fpdata.csv",header = T))
#Select only the data for Section 1
Resort = Resort[Resort$hotel == "Resort Hotel",]

#Exploratory Analysis
#Remove unneccsary time variables as well as uneccesarry variables
Resort = subset(Resort, select = -c(arrival_date_year, arrival_date_week_number, arrival_date_day_of_month, hotel, reserved_room_type, meal, market_segment, customer_type))
Resort$is_canceled = as.factor(Resort$is_canceled)

#Testing Non-linearity
#Figure 1
par(mfrow = c(2,2))
plot(Resort$lead_time,Resort$adr, ylab = "adr", xlab = "lead_time")
plot(Resort$stays_in_weekend_nights, Resort$adr, ylab = "adr", xlab = "stays_in_weekend_nights")
plot(Resort$stays_in_week_nights, Resort$adr, ylab = "adr", xlab = "stays_in_week_nights")
plot(Resort$total_of_special_requests, Resort$adr, ylab = "adr", xlab = "total_of_special_requests")
title("Figure 1", line = -1, outer = T)

#Model creation
#Simple model with interation term (base model)
mod1 = glm(is_canceled ~ ., data = Resort, family = binomial)
summary(mod1)
plot(mod1)
#Residuals vs Fitted shows that we can assume a linear relationship between explanatory variables and the response
#Normal QQ plot shows our residuals are not exactly normal, perform shapiro test to be sure
shapiro.test(residuals(mod1))
#The result of sharpio-wilks testing H0 = data is normally distributed vs Ha = data is not normally distributed, shows we reject H0, data is not normally distributed. May have to perform transformation of our data
#Scale-Location plot shows variance is not homogenous, may need to perform a transformation
#Residuals vs Leverage plot shows that we have some high leverage points, some exceed 3 standard deviations, we may have to remove them top get a better model, could be a result of non-normality in our data
#influential points
plot(mod1,4)
#collinearity
vif(mod1)

#Model using backward regression w/ AIC and with interaction terms
mod2 = glm(is_canceled ~ . - children - babies + adults:children + adults:babies,data = Resort, family = binomial)
mod.back = step(mod2, direction = "backward")
summary(mod.back)
plot(mod.back)
#Residuals vs Fitted shows that we can assume a linear relationship between explanatory variables and the response
#Normal QQ plot shows our residuals are not exactly normal, perform shapiro test to be sure
shapiro.test(residuals(mod.back))
#The result of sharpio-wilks testing H0 = data is normally distributed vs Ha = data is not normally distributed, shows we reject H0, data is not normally distributed. May have to perform transformation of our data
#Scale-Location plot shows variance is not homogenous, may need to perform a transformation
#Residuals vs Leverage plot shows that we have some high leverage points, some exceed 3 standard deviations, we may have to remove them top get a better model, could be a result of non-normality in our data
#influential points
plot(mod.back,4)
#collinearity
vif(mod2)

#Train Test Split for Prediction
set.seed(100)
trainInt = sample(1:nrow(Resort), .6*nrow(Resort))
train = Resort[trainInt,]
test = Resort[-trainInt,]

#Calculate accuracy of the model
prob = predict(mod2, test, type = "response")
predClass = ifelse(prob > .5, 1, 0)
mean(predClass == test$is_canceled)

#Adjusted R-square to determine significant variables

r2mod = regsubsets(is_canceled ~ . - children - babies + adults:children + adults:babies,data = Resort)
r2s = summary(r2mod)
r2s$which
msize = 2:9
r2s$which[which.max(r2s$adjr2),]
selectVar = colnames(r2s$which)[r2s$which[which.max(r2s$adjr2),]]
selectVar = selectVar[-1]
selectVar

#Mallow's CP to determine significant variables
r2s$which[which.min(r2s$cp),]
selectVar = colnames(r2s$which)[r2s$which[which.min(r2s$cp),]]
selectVar = selectVar[-1]
selectVar

#Randomforest 

#Split into train and validation data (70/30)
trainInt = sample(1:nrow(Resort), .6*nrow(Resort))
train = Resort[trainInt,]
test = Resort[-trainInt,]

#Starting Model
mlMod = randomForest(is_canceled ~ ., data = Resort, subset = trainInt)
mlMod

test.acc = double(10)
#mtry is the number of variables randomly chosen at each split
for(mtry in 1:10) 
{
  rf = randomForest(is_canceled ~ . , data = Resort, subset = trainInt, mtry = mtry,ntree = 400) 
  prob.rf = predict(rf,test,type = "response") #Predictions on Test Set for each Tree
  test.acc[mtry] = with(test, mean(is_canceled == prob.rf)) #Mean Squared Test Error
}
test.acc
#minmize MSE at mtry = 5
max(test.acc)

mlMod2 = randomForest(is_canceled ~ . - children - babies + adults:children + adults:babies, data = Resort, subset = trainInt, mtry = 5,ntree = 400) 
mlMod2
