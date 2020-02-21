#Problem 4
setwd("/Users/chanm/Desktop/STATclasses/STAT426")
UCadmissions = as.data.frame(read.csv(file = 'UC-Admissions.csv'),header=TRUE)
UCadmissions = transform(UCadmissions,
                         Department = relevel(Department,"A"),
                         Gender = relevel(Gender,"Male"),
                         Admission = relevel(Admission,"Y"))
UCadmissions

#organizing data
UCad = xtabs(Freq~Gender+Department+Admission,data = UCadmissions)
UCad

#estimated conditional odds ratios for each Department
oddsRatio = function(x){
  UCad[1,x,1] * UCad[2,x,2] / (UCad[1,x,2] * UCad[2,x,1])
}
oddsRatio(1:6)

#marginal table
mUCad = xtabs(Freq ~ Gender + Admission, data = UCadmissions)
mUCad  

#estimated marginal odds ratio
mUCad[1,1]*mUCad[2,2]/(mUCad[1,2]*mUCad[2,1])

#Simpsons Paradox: Ignoring departments, males are more accepted approx. 1.84108 than females to UC