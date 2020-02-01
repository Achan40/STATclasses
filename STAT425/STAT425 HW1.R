#problem 1
library(tidyverse)
library(faraway)
data("prostate")
head(prostate)

#numerical (explain by hand)
summary(prostate)
#graphical (explain by hand)
pairs(prostate)

#problem 2 (link to proof)
#https://math.stackexchange.com/questions/129909/correlation-coefficient-and-determination-coefficient/1799567

#problem 3
invoice = read.delim(file.choose())#manually choose the txt file w data
slr_invoice = lm(Time ~ Invoices,data = invoice)
#3a
plot(invoice$Invoices,invoice$Time,xlab = "Invoices",ylab = "Time")
abline(slr_invoice)
#3b
confint(slr_invoice,level = .95)
#3c
#3d
predict_data = data.frame(Invoices = 130)
predict(slr_invoice,predict_data,interval = "prediction",level = .95)

#problem 5
indicators = read.delim(file.choose())
slr_indicators = lm(PriceChange ~ LoanPaymentsOverdue, data = indicators)
#5a
summary(slr_indicators)
#5b
confint(slr_indicators)
#5c
predict_data2 = data.frame(LoanPaymentsOverdue = 4)
predict(slr_indicators,predict_data2,interval = "confidence",level = .95)
