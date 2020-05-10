library(faraway)
setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT425/Final")

Resort = data.frame(read.csv(file = "stat425_fpdata.csv",header = T))
#Select only the data for Section 1
Resort = Resort[Resort$hotel == "Resort Hotel",]

#Exploratory Analysis
#Remove unneccsary time variables as well as uneccesarry variables
Resort = subset(Resort, select = -c(arrival_date_year, arrival_date_week_number, arrival_date_day_of_month, hotel, is_canceled, reserved_room_type, meal, market_segment, customer_type))

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
mod1 = glm(adr ~ . - children - babies + adults:children + adults:babies, data = Resort)
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

#Model using backward regression
mod.back = step(mod1, adr ~ . - children - babies + adults:children + adults:babies, direction = "backward")
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

