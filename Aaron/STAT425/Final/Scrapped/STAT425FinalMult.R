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
mod1 = glm(adr ~ ., data = Resort)
summary(mod1)
plot(mod1)
title("Figure 2", line = -1, outer = T)
#Residuals vs Fitted (tests lin relationship)
#Normal QQ plot (normality of residuals)
shapiro.test(residuals(mod1))
#The result of sharpio-wilks testing H0 = data is normally distributed vs Ha = data is not normally distributed, shows we reject H0, data is not normally distributed. May have to perform transformation of our data
#Scale-Location plot (homogeneity of variance)
#Residuals vs Leverage plot 
#influential points
par(mfrow = c(1,1))
plot(mod1,4)
title("Figure 2.1", line = -1, outer = T)
#collinearity
vif(mod1)

#Model using backward regression w/ AIC and with interaction terms
mod2 = glm(adr ~ . - children - babies + adults:children + adults:babies,data = Resort)
mod.back = step(mod2, direction = "backward")
summary(mod.back)
par(mfrow = c(2,2))
plot(mod.back)
title("Figure 3", line = -1, outer = T)
#Residuals vs Fitted (tests lin relationship)
#Normal QQ plot (normality of residuals)
shapiro.test(residuals(mod.back))
#The result of sharpio-wilks testing H0 = data is normally distributed vs Ha = data is not normally distributed, shows we reject H0, data is not normally distributed. May have to perform transformation of our data
#Scale-Location plot (homogeneity of variance)
#Residuals vs Leverage plot 
#influential points
par(mfrow = c(1,1))
plot(mod.back,4)
title("Figure 3.1", line = -1, outer = T)
#collinearity
vif(mod2)

#Adjusted R-square to determine significant variables
r2mod = regsubsets(adr ~ . - children - babies + adults:children + adults:babies,data = Resort)
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

#Train Test Split for Prediction 60/40
set.seed(10)
trainInt = sample(1:nrow(Resort), .6*nrow(Resort))
train = Resort[trainInt,]
test = Resort[-trainInt,]

#Calculate MSE for the accuracy of the model
pred = predict(mod2, test)
test.err = with(test, mean((adr - pred)^2))
test.err

#Randomforest 

#Split into train and validation data 60/40
trainInt = sample(1:nrow(Resort), .6*nrow(Resort))
train = Resort[trainInt,]
test = Resort[-trainInt,]

#Starting Model
mlMod = randomForest(adr ~ ., data = Resort, subset = trainInt)
mlMod
#plotting error vs number of trees
plot(mlMod)

test.err = double(10)

#mtry is the number of variables randomly chosen at each split
for(mtry in 1:10) 
{
  rf = randomForest(adr ~ . , data = Resort, subset = trainInt, mtry = mtry,ntree = 400) 
  pred = predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry] = with(test, mean((adr - pred)^2)) #Mean Squared Test Error
}
test.err
#minmize MSE at mtry = 7
min(test.err)

mlMod2 = randomForest(adr ~ . - children - babies + adults:children + adults:babies, data = Resort, subset = trainInt, mtry = 7,ntree = 400) 
mlMod2
