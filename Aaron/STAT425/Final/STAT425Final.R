library(GGally)
setwd("/Users/chanm/Desktop/STATclasses/Aaron/STAT425/Final")

Resort = data.frame(read.csv(file = "stat425_fpdata.csv",header = T))
#Select only the data for Section 1
Resort = Resort[Resort$hotel == "Resort Hotel",]

#Exploratory Analysis
#Remove unneccsary time variables
Resort = subset(Resort, select = -c(arrival_date_year, arrival_date_week_number, arrival_date_day_of_month))
#ggpairs(Resort)

#Testing Non-linearity
par(mfrow = c(2,2))
plot(Resort$lead_time,Resort$adr, ylab = "adr", xlab = "lead_time")
plot(Resort$stays_in_weekend_nights, Resort$adr, ylab = "adr", xlab = "stays_in_weekend_nights")
plot(Resort$stays_in_week_nights, Resort$adr, ylab = "adr", xlab = "stays_in_week_nights")
plot(Resort$total_of_special_requests, Resort$adr, ylab = "adr", xlab = "total_of_special_requests")
title("Figure 1", line = -1, outer = T)

