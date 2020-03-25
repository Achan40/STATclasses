#8.1
# load packages
library("tidyverse")
library("caret")

# set seed 
set.seed(66217)

# load dataset
birthwt = as_tibble(MASS::birthwt)

# change response to named factor variable
birthwt$low = factor(ifelse(birthwt$low == 0, "normal", "low"))

# test-train split
bwt_trn_idx = sample(nrow(birthwt), size = 0.8 * nrow(birthwt))
bwt_trn = birthwt[bwt_trn_idx, ]
bwt_tst = birthwt[-bwt_trn_idx, ]

# check data
bwt_trn

mod2.1 = knn3(low~.-bwt,k=5,data = bwt_trn)
#Accuracy and Balanced Accuracy listed in confusion matrix output
confusionMatrix(table(predicted = predict(mod2.1,bwt_tst,type="class"), actual = bwt_tst$low),positive = "low")

#8.2
library("tidyverse")
library("rpart")
set.seed(22538)

# load dataset
data(GermanCredit, package = "caret")
gc = as_tibble(GermanCredit)

# test-train split
gc_trn_idx = sample(nrow(gc), size = 0.6 * nrow(gc))
gc_trn = gc[gc_trn_idx, ]
gc_tst = gc[-gc_trn_idx, ]

mod2.2 = rpart(Class ~., data = gc_trn)

#alpha=.5
predict2.21 = as.factor(ifelse(predict(mod2.2, gc_tst, type = "prob")[,2] >= .5, "Good", "Bad"))
confusionMatrix(predict2.21, reference = gc_tst$Class, positive = "Bad")
#pos pred value = precision, FDR=1-Precision

#alpha=.8
predict2.22 = as.factor(ifelse(predict(mod2.2, gc_tst, type = "prob")[,2] >= .2, "Good", "Bad"))
confusionMatrix(predict2.22, reference = gc_tst$Class, positive = "Bad")

#8.3
# set seed 
set.seed(9888)

# load dataset
data(GermanCredit, package = "caret")
gc = as_tibble(GermanCredit)

# test-train split
gc_trn_idx = sample(nrow(gc), size = 0.6 * nrow(gc))
gc_trn = gc[gc_trn_idx, ]
gc_tst = gc[-gc_trn_idx, ]

# estimation-validation split
gc_est_idx = sample(nrow(gc_trn), size = 0.6 * nrow(gc_trn))
gc_est = gc_trn[gc_est_idx, ]
gc_val = gc_trn[-gc_est_idx, ]

# check data
head(gc_trn)

#trn is est to est in this case
#val is est to val
#test is trn to test
mod8.3est = rpart(Class ~ ., data = gc_est)
mod8.3trn = rpart(Class ~ ., data = gc_trn)

pred = predict(mod8.3trn, gc_tst, type = "class")
conf_mtx = confusionMatrix(table(predicted = pred, actual = gc_tst$Class),positive = "Bad")$table

TP = conf_mtx[1, 1]
TN = conf_mtx[2, 2]
FP = conf_mtx[1, 2]
FN = conf_mtx[2, 1]
recall = TP / (TP + FN)
precision = TP / (TP + FP)
F1 = 2 * (precision * recall) / (precision + recall)
MCC = (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

F1
MCC

#8.4
library(purrr)

# set seed 
set.seed(67903)

# load data and coerce to tibble
hitters = tibble::as_tibble(ISLR::Hitters)

# store indexes for use in 300 bootstrap resamples
resamp_idx = caret::createResample(hitters$Hits, times = 500)

hits=hitters$Hits

make_boot_resample = function(data,idx){
  data[idx]
}

#using each element of resamp_idx to create a new data set
boot_resamples = map(resamp_idx, ~make_boot_resample(data = hits,idx = .x))
#find the median hits
boot_replicates = map_dbl(boot_resamples, ~median(.x))  #dbl is important
#inner 95% bootstrap replicates
quantile(boot_replicates, prob=c(0.025,0.975))

#8.5
# set seed 
set.seed(60714)

# load data and coerce to tibble
data("GermanCredit", package = "caret")
gc = tibble::as_tibble(GermanCredit)

# store indexes for use in 300 bootstrap resamples
resamp_idx = caret::createResample(gc$Amount, times = 300)

#using each element of resamp_idx to create a new data set
boot_resamples = map(resamp_idx, ~make_boot_resample(data = gc$Amount,idx = .x))
#function to find the p(amount>1000)
probG1k = function(b){
  sum(b>4000)/length(b)
}
#map that function to the resamples
boot_replicates = map_dbl(boot_resamples, ~probG1k(.x))  #dbl is important
#inner 98% bootstrap replicates
quantile(boot_replicates, prob=c(0.01,0.99))

#8.6
# set seed
set.seed(34378)
# load data and coerce to tibble
bstn = tibble::as_tibble(MASS::Boston)
# store indexes for use in 200 bootstrap resamples
resamp_idx = caret::createResample(bstn$medv, times = 200)

#Need to resample on rows, not individual data as before
make_boot_resample=function(data,idx){
  data[idx,]
}

boot_resamples=map(resamp_idx,~make_boot_resample(data = bstn,idx=.x))
boot_replicates=map_dbl(boot_resamples,~predict(lm(medv ~ rm, data = .x), data.frame(rm = 4.049)))
#inner 90% bootstrap replicates
quantile(boot_replicates,prob=c(0.05,0.95))

#8.7
# load packages
library("purrr")
library("tibble")
library("rpart")

# set seed 
set.seed(50200)

# load data
data(Glass, package = "mlbench")

# coerce to tibble
glass = as_tibble(Glass)

# store indexes for 10 fold cross-validation
index_fold = caret::createFolds(glass$Type, k = 5)

#Create Accuracy funciton
calc_ACC = function(actual,predicted){
  mean(actual==predicted)
}
#Accuracy functino for one fold
calc_ACC_fold = function(fold_idx){
  est = glass[-fold_idx, ]
  val = glass[fold_idx, ]
  pred_val = predict(rpart(Type~., data = est, cp =.001,minsplit = 20),val,type = "class") #create model and predict with val data
  calc_ACC(actual = val$Type,predicted = pred_val)
}

#ACC all folds
ACC_all_folds = map_dbl(index_fold,calc_ACC_fold)
#cross validation ACC&se
mean(ACC_all_folds)
sd(ACC_all_folds)

#8.8
# set seed 
set.seed(60145)

# load data and coerce to tibble
bstn = tibble::as_tibble(MASS::Boston)

# store indexes for 10 fold cross-validation
index_fold = caret::createFolds(bstn$medv, k = 10)

# extract the knnreg function from the caret package
knnreg = caret::knnreg

#cross validation takes place within the dataset, need to repeat RMSE n times, create est data for n fold, val data for n folds, etc...
#RMSE function
calc_rmse = function(actual, predicted){
  sqrt(mean((actual-predicted)^2))
}

#RMSE for a specific fold
calc_rmse_fold = function(fold_idx){
  est = bstn[-fold_idx, ]
  val = bstn[fold_idx, ]
  pred_val = predict(knnreg(medv~., data = est, k =7),val)
  calc_rmse(actual = val$medv,predicted = pred_val)
}

#numeric so map_dbl
RMSE_all_folds = map_dbl(index_fold, calc_rmse_fold)

#10fold cross validate is mean of all those RMSE of folds
mean(RMSE_all_folds)
sd(RMSE_all_folds)

#8.9
# load packages
library("purrr")
library("tibble")

# set seed 
set.seed(91113)

# load and coerce data to tibble
bstn = as_tibble(MASS::Boston)

# store indexes for 10 fold cross-validation
index_fold = caret::createFolds(bstn$medv, k = 10)

#function for the metric
calc_metric = function(actual,predicted){
  sum(abs(actual-predicted)>3)/length(actual)
}
#metric for a specific fold
calc_metric_fold = function(fold_idx){
  est = bstn[-fold_idx, ]
  val = bstn[fold_idx, ]
  pred_val = predict(lm(medv~black, data = est),val)
  calc_metric(actual = val$medv,predicted = pred_val)
}

#numeric so map_dbl
metric_all_folds = map_dbl(index_fold, calc_metric_fold)

#10fold cross validate is mean of all those metric of folds
mean(metric_all_folds)
sd(metric_all_folds)
