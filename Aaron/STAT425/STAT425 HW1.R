#problem 1
library(tidyverse)
library(faraway)
data("prostate")
help("prostate")

#numerical
summary(prostate)
#graphical
pairs(prostate)

#problem 3
invoice = read.delim("invoices.txt")
slr_invoice = lm(Time ~ Invoices,data = invoice)
#3a
plot(invoice$Invoices,invoice$Time,xlab = "Invoices",ylab = "Time")
abline(slr_invoice)
#3b
confint(slr_invoice,level = .95)
#3c
summary(slr_invoice)
t = ((0.0112916-.01)/0.0008184)
2*pt(-abs(t),df=28)
#DNR null at alpha=.05
#3d
predict_data = data.frame(Invoices = 130)
predict(slr_invoice,predict_data,interval = "prediction",level = .95)

#problem 5
indicators = read.delim("indicators.txt")
slr_indicators = lm(PriceChange ~ LoanPaymentsOverdue, data = indicators)
#5a
summary(slr_indicators)$r.squared
summary(slr_indicators)$adj.r.squared
#5b
confint(slr_indicators,level = .95)
#5c
predict_data2 = data.frame(LoanPaymentsOverdue = 4)
predict(slr_indicators,predict_data2,interval = "confidence",level = .95)
