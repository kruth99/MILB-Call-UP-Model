library(xml2)
library(rvest)
library(tidyverse)
library(rjson)
library(dplyr)
library(pROC)

A_batters <- readRDS("A_batters.RData")
AA_batters <- readRDS("AA_batters.RData")
AAA_batters <- readRDS("AAA_batters.RData")
A_pitchers <- readRDS("A_pitchers.RData")
AA_pitchers <- readRDS("AA_pitchers.RData")
AAA_pitchers <- readRDS("AAA_pitchers.RData")

A_battersX <- model.matrix(Promoted ~ ., data=A_batters)[,-c(1)]


library(randomForest)

rf_mod <- randomForest(A_batters$Promoted ~ ., data=data.frame(A_battersX))

# library(rpart) for missing vals

varImpPlot(rf_mod)

plot(rf_mod)

library(gbm)
# library(xgboost)
gbm_mod = gbm(A_batters$Promoted ~ ., data=data.frame(A_battersX))

summary(gbm_mod)

# PDP Plot
plot(gbm_mod,i.var='BA')
plot(gbm_mod,i.var='Age')
plot(gbm_mod,i.var='OPS')

# Training-Test Set
train_index <- sample(nrow(A_batters), 0.90*nrow(A_batters))
A_batters_train <- A_batters[train_index,]
A_batters_test <- A_batters[-train_index,]

trainX <- model.matrix(Promoted ~., data = A_batters_train)[,-c(1)]
testX <- model.matrix(Promoted ~., data = A_batters_test)[,-c(1)]

rf_train <- randomForest::randomForest(A_batters_train$Promoted ~., data = data.frame(trainX), ntree = 100, na.action = na.omit)

test_preds = predict(rf_train, data.frame(testX), type = "response")
print(test_preds)

# 80% Training
#15% Test
#5% Validation

set.seed(21)
validation_sample = sample(nrow(A_batters), .05*nrow(A_batters))
A_batters_validation = A_batters[validation_sample,]
A_batters_nonval = A_batters[-validation_sample,]
#K-fold cross validation
# Usually k is somewhere between 2 and 20
k = 10
nonval_sample = sample(nrow(A_batters_nonval))
nonval_deciles = quantile(1:nrow(A_batters_nonval),
                          seq(0,1, by=1/k))
cv_list = list()

for(i in 1:k){

  if(i == 6) {
    print("here")
    randomized_dec = nonval_sample[4839:floor(nonval_deciles[i+1])]
  }
  else {
    randomized_dec = nonval_sample[ceiling(nonval_deciles[i]):floor(nonval_deciles[i+1])]
  }
  cv_list[[i]] = A_batters_nonval[randomized_dec,]

}

pred_list1 = list()
pred_list2 = list()
pred_list3 = list()
pred_list4 = list()
pred_list5 = list()
pred_list6 = list()
pred_list7 = list()

for(i in 1:k){

  cv_dat = do.call(rbind, cv_list[-i])
  cvX = model.matrix(Promoted ~., data = cv_dat)[,-c(1)]
  rf_cv1 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 5)
  rf_cv2 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 20)
  rf_cv3 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 100)
  rf_cv4 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 125)
  rf_cv5 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 135)
  rf_cv6 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 150)
  rf_cv7 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 200)
  test_dat = cv_list[[i]]
  test_datX = model.matrix(Promoted ~., data=test_dat)[,-c(1)]
  pred_list1[[i]] = predict(rf_cv1, data.frame(test_datX), type = "response")
  pred_list2[[i]] = predict(rf_cv2, data.frame(test_datX), type = "response")
  pred_list3[[i]] = predict(rf_cv3, data.frame(test_datX), type = "response")
  pred_list4[[i]] = predict(rf_cv4, data.frame(test_datX), type = "response")
  pred_list5[[i]] = predict(rf_cv5, data.frame(test_datX), type = "response")
  pred_list6[[i]] = predict(rf_cv6, data.frame(test_datX), type = "response")
  pred_list7[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  print(i)

}

cv_preds <- do.call(c, pred_list1)
actual <- do.call(rbind, cv_list)

val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("5 trees")

cv_preds = do.call(c, pred_list2)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("20 trees")

cv_preds = do.call(c, pred_list3)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("100 trees")

cv_preds = do.call(c, pred_list4)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("125 trees")

cv_preds = do.call(c, pred_list5)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("135 trees")

cv_preds = do.call(c, pred_list6)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")

cv_preds = do.call(c, pred_list7)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("200 trees")

# Validation Prediction
cvallX = model.matrix(Promoted ~ .,data=A_batters_nonval)[,-c(1)]
rf_cvall = randomForest::randomForest(A_batters_nonval$Promoted ~ ., data=data.frame(cvallX),ntree=150)

valX =model.matrix(Promoted ~ .,data=A_batters_validation)[,-c(1)]
val_preds = predict(rf_cvall,data.frame(valX),type="response")


# MSE is not that accurate with the data
val_preds_mock <- roc(A_batters_validation$Promoted, val_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")


A_pitchersX <- model.matrix(Promoted ~ ., data=A_pitchers)[,-c(1)]


rf_mod <- randomForest(A_pitchers$Promoted ~ ., data=data.frame(A_pitchersX))

# library(rpart) for missing vals

varImpPlot(rf_mod)

plot(rf_mod)

gbm_mod = gbm(A_pitchers$Promoted ~ ., data=data.frame(A_pitchersX))

summary(gbm_mod)

plot(gbm_mod,i.var='ERA')
plot(gbm_mod,i.var='WHIP')
plot(gbm_mod,i.var='Age')


# Training-Test Set
train_index <- sample(nrow(A_pitchers), 0.90*nrow(A_pitchers))
A_pitchers_train <- A_pitchers[train_index,]
A_pitchers_test <- A_pitchers[-train_index,]

trainX <- model.matrix(Promoted ~., data = A_pitchers_train)[,-c(1)]
testX <- model.matrix(Promoted ~., data = A_pitchers_test)[,-c(1)]

rf_train <- randomForest::randomForest(A_pitchers_train$Promoted ~., data = data.frame(trainX), ntree = 100, na.action = na.omit)

test_preds = predict(rf_train, data.frame(testX), type = "response")
print(test_preds)


# 80% Training
#15% Test
#5% Validation

set.seed(21)
validation_sample = sample(nrow(A_pitchers), .05*nrow(A_pitchers))
A_pitchers_validation = A_pitchers[validation_sample,]
A_pitchers_nonval = A_pitchers[-validation_sample,]
#K-fold cross validation
# Usually k is somewhere between 2 and 20
k = 10
nonval_sample = sample(nrow(A_pitchers_nonval))
nonval_deciles = quantile(1:nrow(A_pitchers_nonval),
                          seq(0,1, by=1/k))
cv_list = list()

for(i in 1:k){
  randomized_dec = nonval_sample[ceiling(nonval_deciles[i]):floor(nonval_deciles[i+1])]
  cv_list[[i]] = A_pitchers_nonval[randomized_dec,]
}

pred_list1 = list()
pred_list2 = list()
pred_list3 = list()
pred_list4 = list()
pred_list5 = list()
pred_list6 = list()
pred_list7 = list()
pred_list8 = list()

for(i in 1:k){

  cv_dat = do.call(rbind, cv_list[-i])
  cvX = model.matrix(Promoted ~., data = cv_dat)[,-c(1)]
  rf_cv1 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 5)
  rf_cv2 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 20)
  rf_cv3 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 100)
  rf_cv4 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 125)
  rf_cv5 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 135)
  rf_cv6 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 150)
  rf_cv7 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 200)
  rf_cv8 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 250)
  test_dat = cv_list[[i]]
  test_datX = model.matrix(Promoted ~., data=test_dat)[,-c(1)]
  pred_list1[[i]] = predict(rf_cv1, data.frame(test_datX), type = "response")
  pred_list2[[i]] = predict(rf_cv2, data.frame(test_datX), type = "response")
  pred_list3[[i]] = predict(rf_cv3, data.frame(test_datX), type = "response")
  pred_list4[[i]] = predict(rf_cv4, data.frame(test_datX), type = "response")
  pred_list5[[i]] = predict(rf_cv5, data.frame(test_datX), type = "response")
  pred_list6[[i]] = predict(rf_cv6, data.frame(test_datX), type = "response")
  pred_list7[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  pred_list8[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  print(i)

}

cv_preds <- do.call(c, pred_list1)
actual <- do.call(rbind, cv_list)

val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("5 trees")

cv_preds = do.call(c, pred_list2)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("20 trees")

cv_preds = do.call(c, pred_list3)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("100 trees")

cv_preds = do.call(c, pred_list4)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("125 trees")

cv_preds = do.call(c, pred_list5)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("135 trees")

cv_preds = do.call(c, pred_list6)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")

cv_preds = do.call(c, pred_list7)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("200 trees")

cv_preds = do.call(c, pred_list8)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("250 trees")

# Validation Prediction
cvallX = model.matrix(Promoted ~ .,data=A_pitchers_nonval)[,-c(1)]
rf_cvall = randomForest::randomForest(A_pitchers_nonval$Promoted ~ ., data=data.frame(cvallX),ntree=135)

valX =model.matrix(Promoted ~ .,data=A_pitchers_validation)[,-c(1)]
val_preds = predict(rf_cvall,data.frame(valX),type="response")


# MSE is not that accurate with the data
val_preds_mock <- roc(A_pitchers_validation$Promoted, val_preds, plot = TRUE, print.auc = TRUE)
title("135 trees")

AA_battersX <- model.matrix(Promoted ~ ., data=AA_batters)[,-c(1)]


rf_mod <- randomForest(AA_batters$Promoted ~ ., data=data.frame(AA_battersX))

# library(rpart) for missing vals

varImpPlot(rf_mod)

plot(rf_mod)

gbm_mod = gbm(AA_batters$Promoted ~ ., data=data.frame(AA_battersX))

summary(gbm_mod)

plot(gbm_mod,i.var='BA')
plot(gbm_mod,i.var='OPS')
plot(gbm_mod,i.var='OBP')
plot(gbm_mod,i.var='Age')

# Training-Test Set
train_index <- sample(nrow(AA_batters), 0.90*nrow(AA_batters))
AA_batters_train <- AA_batters[train_index,]
AA_batters_test <- AA_batters[-train_index,]

trainX <- model.matrix(Promoted ~., data = AA_batters_train)[,-c(1)]
testX <- model.matrix(Promoted ~., data = AA_batters_test)[,-c(1)]

rf_train <- randomForest::randomForest(AA_batters_train$Promoted ~., data = data.frame(trainX), ntree = 100, na.action = na.omit)

test_preds = predict(rf_train, data.frame(testX), type = "response")
print(test_preds)


#15% Test
#5% Validation

set.seed(21)
validation_sample = sample(nrow(AA_batters), .05*nrow(AA_batters))
AA_batters_validation = AA_batters[validation_sample,]
AA_batters_nonval = AA_batters[-validation_sample,]
#K-fold cross validation
# Usually k is somewhere between 2 and 20
k = 10
nonval_sample = sample(nrow(AA_batters_nonval))
nonval_deciles = quantile(1:nrow(AA_batters_nonval),
                          seq(0,1, by=1/k))
cv_list = list()

for(i in 1:k){
  randomized_dec = nonval_sample[ceiling(nonval_deciles[i]):floor(nonval_deciles[i+1])]
  cv_list[[i]] = AA_batters_nonval[randomized_dec,]
}

pred_list1 = list()
pred_list2 = list()
pred_list3 = list()
pred_list4 = list()
pred_list5 = list()
pred_list6 = list()
pred_list7 = list()
pred_list8 = list()

for(i in 1:k){

  cv_dat = do.call(rbind, cv_list[-i])
  cvX = model.matrix(Promoted ~., data = cv_dat)[,-c(1)]
  rf_cv1 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 5)
  rf_cv2 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 20)
  rf_cv3 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 100)
  rf_cv4 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 125)
  rf_cv5 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 135)
  rf_cv6 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 150)
  rf_cv7 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 200)
  rf_cv8 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 250)
  test_dat = cv_list[[i]]
  test_datX = model.matrix(Promoted ~., data=test_dat)[,-c(1)]
  pred_list1[[i]] = predict(rf_cv1, data.frame(test_datX), type = "response")
  pred_list2[[i]] = predict(rf_cv2, data.frame(test_datX), type = "response")
  pred_list3[[i]] = predict(rf_cv3, data.frame(test_datX), type = "response")
  pred_list4[[i]] = predict(rf_cv4, data.frame(test_datX), type = "response")
  pred_list5[[i]] = predict(rf_cv5, data.frame(test_datX), type = "response")
  pred_list6[[i]] = predict(rf_cv6, data.frame(test_datX), type = "response")
  pred_list7[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  pred_list8[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  print(i)

}

cv_preds <- do.call(c, pred_list1)
actual <- do.call(rbind, cv_list)

val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("5 trees")

cv_preds = do.call(c, pred_list2)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("20 trees")

cv_preds = do.call(c, pred_list3)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("100 trees")

cv_preds = do.call(c, pred_list4)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("125 trees")

cv_preds = do.call(c, pred_list5)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("135 trees")

cv_preds = do.call(c, pred_list6)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")

cv_preds = do.call(c, pred_list7)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("200 trees")

cv_preds = do.call(c, pred_list8)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("250 trees")

# Best is 100 Trees
# Validation Prediction
cvallX = model.matrix(Promoted ~ .,data=AA_batters_nonval)[,-c(1)]
rf_cvall = randomForest::randomForest(AA_batters_nonval$Promoted ~ ., data=data.frame(cvallX),ntree=100)

valX =model.matrix(Promoted ~ .,data=AA_batters_validation)[,-c(1)]
val_preds = predict(rf_cvall,data.frame(valX),type="response")


# MSE is not that accurate with the data
val_preds_mock <- roc(AA_batters_validation$Promoted, val_preds, plot = TRUE, print.auc = TRUE)
title("100 trees")

AA_pitchersX <- model.matrix(Promoted ~ ., data=AA_pitchers)[,-c(1)]


rf_mod <- randomForest(AA_pitchers$Promoted ~ ., data=data.frame(AA_pitchersX))

# library(rpart) for missing vals

varImpPlot(rf_mod)

plot(rf_mod)

gbm_mod = gbm(AA_pitchers$Promoted ~ ., data=data.frame(AA_pitchersX))

summary(gbm_mod)

plot(gbm_mod,i.var='ERA')
plot(gbm_mod,i.var='WHIP')
plot(gbm_mod,i.var='Age')


# Training-Test Set
train_index <- sample(nrow(AA_pitchers), 0.90*nrow(AA_pitchers))
AA_pitchers_train <- AA_pitchers[train_index,]
AA_pitchers_test <- AA_pitchers[-train_index,]

trainX <- model.matrix(Promoted ~., data = AA_pitchers_train)[,-c(1)]
testX <- model.matrix(Promoted ~., data = AA_pitchers_test)[,-c(1)]

rf_train <- randomForest::randomForest(AA_pitchers_train$Promoted ~., data = data.frame(trainX), ntree = 100, na.action = na.omit)

test_preds = predict(rf_train, data.frame(testX), type = "response")
print(test_preds)


# 80% Training
#15% Test
#5% Validation

set.seed(21)
validation_sample = sample(nrow(AA_pitchers), .05*nrow(AA_pitchers))
AA_pitchers_validation = AA_pitchers[validation_sample,]
AA_pitchers_nonval = AA_pitchers[-validation_sample,]
#K-fold cross validation
# Usually k is somewhere between 2 and 20
k = 10
nonval_sample = sample(nrow(AA_pitchers_nonval))
nonval_deciles = quantile(1:nrow(AA_pitchers_nonval),
                          seq(0,1, by=1/k))
cv_list = list()

for(i in 1:k){
  randomized_dec = nonval_sample[ceiling(nonval_deciles[i]):floor(nonval_deciles[i+1])]
  cv_list[[i]] = AA_pitchers_nonval[randomized_dec,]
}

pred_list1 = list()
pred_list2 = list()
pred_list3 = list()
pred_list4 = list()
pred_list5 = list()
pred_list6 = list()
pred_list7 = list()
pred_list8 = list()

for(i in 1:k){

  cv_dat = do.call(rbind, cv_list[-i])
  cvX = model.matrix(Promoted ~., data = cv_dat)[,-c(1)]
  rf_cv1 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 5)
  rf_cv2 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 20)
  rf_cv3 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 100)
  rf_cv4 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 125)
  rf_cv5 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 135)
  rf_cv6 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 150)
  rf_cv7 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 200)
  rf_cv8 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 250)
  test_dat = cv_list[[i]]
  test_datX = model.matrix(Promoted ~., data=test_dat)[,-c(1)]
  pred_list1[[i]] = predict(rf_cv1, data.frame(test_datX), type = "response")
  pred_list2[[i]] = predict(rf_cv2, data.frame(test_datX), type = "response")
  pred_list3[[i]] = predict(rf_cv3, data.frame(test_datX), type = "response")
  pred_list4[[i]] = predict(rf_cv4, data.frame(test_datX), type = "response")
  pred_list5[[i]] = predict(rf_cv5, data.frame(test_datX), type = "response")
  pred_list6[[i]] = predict(rf_cv6, data.frame(test_datX), type = "response")
  pred_list7[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  pred_list8[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  print(i)

}

cv_preds <- do.call(c, pred_list1)
actual <- do.call(rbind, cv_list)

val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("5 trees")

cv_preds = do.call(c, pred_list2)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("20 trees")

cv_preds = do.call(c, pred_list3)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("100 trees")

cv_preds = do.call(c, pred_list4)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("125 trees")

cv_preds = do.call(c, pred_list5)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("135 trees")

cv_preds = do.call(c, pred_list6)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")

cv_preds = do.call(c, pred_list7)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("200 trees")

cv_preds = do.call(c, pred_list8)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("250 trees")

# Validation Prediction
cvallX = model.matrix(Promoted ~ .,data=AA_pitchers_nonval)[,-c(1)]
rf_cvall = randomForest::randomForest(AA_pitchers_nonval$Promoted ~ ., data=data.frame(cvallX),ntree=250)

valX =model.matrix(Promoted ~ .,data=AA_pitchers_validation)[,-c(1)]
val_preds = predict(rf_cvall,data.frame(valX),type="response")


# MSE is not that accurate with the data
val_preds_mock <- roc(AA_pitchers_validation$Promoted, val_preds, plot = TRUE, print.auc = TRUE)
title("250 trees")



AAA_battersX <- model.matrix(Promoted ~ ., data=AAA_batters)[,-c(1)]


rf_mod <- randomForest(AAA_batters$Promoted ~ ., data=data.frame(AAA_battersX))

# library(rpart) for missing vals

varImpPlot(rf_mod)

plot(rf_mod)

gbm_mod = gbm(AAA_batters$Promoted ~ ., data=data.frame(AAA_battersX))

summary(gbm_mod)

plot(gbm_mod,i.var='BA')
plot(gbm_mod,i.var='OPS')
plot(gbm_mod,i.var='OBP')
plot(gbm_mod,i.var='Age')

# Training-Test Set
train_index <- sample(nrow(AAA_batters), 0.90*nrow(AAA_batters))
AAA_batters_train <- AAA_batters[train_index,]
AAA_batters_test <- AAA_batters[-train_index,]

trainX <- model.matrix(Promoted ~., data = AAA_batters_train)[,-c(1)]
testX <- model.matrix(Promoted ~., data = AAA_batters_test)[,-c(1)]

rf_train <- randomForest::randomForest(AAA_batters_train$Promoted ~., data = data.frame(trainX), ntree = 100, na.action = na.omit)

test_preds = predict(rf_train, data.frame(testX), type = "response")
print(test_preds)


# 80% Training
#15% Test
#5% Validation

set.seed(21)
validation_sample = sample(nrow(AAA_batters), .05*nrow(AAA_batters))
AAA_batters_validation = AAA_batters[validation_sample,]
AAA_batters_nonval = AAA_batters[-validation_sample,]
#K-fold cross validation
# Usually k is somewhere between 2 and 20
k = 10
nonval_sample = sample(nrow(AAA_batters_nonval))
nonval_deciles = quantile(1:nrow(AAA_batters_nonval),
                          seq(0,1, by=1/k))
cv_list = list()

for(i in 1:k){
  randomized_dec = nonval_sample[ceiling(nonval_deciles[i]):floor(nonval_deciles[i+1])]
  cv_list[[i]] = AAA_batters_nonval[randomized_dec,]
}

pred_list1 = list()
pred_list2 = list()
pred_list3 = list()
pred_list4 = list()
pred_list5 = list()
pred_list6 = list()
pred_list7 = list()
pred_list8 = list()

for(i in 1:k){

  cv_dat = do.call(rbind, cv_list[-i])
  cvX = model.matrix(Promoted ~., data = cv_dat)[,-c(1)]
  rf_cv1 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 5)
  rf_cv2 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 20)
  rf_cv3 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 100)
  rf_cv4 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 125)
  rf_cv5 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 135)
  rf_cv6 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 150)
  rf_cv7 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 200)
  rf_cv8 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 250)
  test_dat = cv_list[[i]]
  test_datX = model.matrix(Promoted ~., data=test_dat)[,-c(1)]
  pred_list1[[i]] = predict(rf_cv1, data.frame(test_datX), type = "response")
  pred_list2[[i]] = predict(rf_cv2, data.frame(test_datX), type = "response")
  pred_list3[[i]] = predict(rf_cv3, data.frame(test_datX), type = "response")
  pred_list4[[i]] = predict(rf_cv4, data.frame(test_datX), type = "response")
  pred_list5[[i]] = predict(rf_cv5, data.frame(test_datX), type = "response")
  pred_list6[[i]] = predict(rf_cv6, data.frame(test_datX), type = "response")
  pred_list7[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  pred_list8[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  print(i)

}

cv_preds <- do.call(c, pred_list1)
actual <- do.call(rbind, cv_list)

val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("5 trees")

cv_preds = do.call(c, pred_list2)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("20 trees")

cv_preds = do.call(c, pred_list3)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("100 trees")

cv_preds = do.call(c, pred_list4)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("125 trees")

cv_preds = do.call(c, pred_list5)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("135 trees")

cv_preds = do.call(c, pred_list6)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")

cv_preds = do.call(c, pred_list7)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("200 trees")

cv_preds = do.call(c, pred_list8)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("250 trees")

# Validation Prediction
cvallX = model.matrix(Promoted ~ .,data=AAA_batters_nonval)[,-c(1)]
rf_cvall = randomForest::randomForest(AAA_batters_nonval$Promoted ~ ., data=data.frame(cvallX),ntree=150)

valX =model.matrix(Promoted ~ .,data=AAA_batters_validation)[,-c(1)]
val_preds = predict(rf_cvall,data.frame(valX),type="response")


# MSE is not that accurate with the data
val_preds_mock <- roc(AAA_batters_validation$Promoted, val_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")



AAA_pitchersX <- model.matrix(Promoted ~ ., data=AAA_pitchers)[,-c(1)]


rf_mod <- randomForest(AAA_pitchers$Promoted ~ ., data=data.frame(AAA_pitchersX))

# library(rpart) for missing vals

varImpPlot(rf_mod)

plot(rf_mod)

gbm_mod = gbm(AAA_pitchers$Promoted ~ ., data=data.frame(AAA_pitchersX))

summary(gbm_mod)

plot(gbm_mod,i.var='ERA')
plot(gbm_mod,i.var='WHIP')
plot(gbm_mod,i.var='Age')


# Training-Test Set
train_index <- sample(nrow(AAA_pitchers), 0.90*nrow(AAA_pitchers))
AAA_pitchers_train <- AAA_pitchers[train_index,]
AAA_pitchers_test <- AAA_pitchers[-train_index,]

trainX <- model.matrix(Promoted ~., data = AAA_pitchers_train)[,-c(1)]
testX <- model.matrix(Promoted ~., data = AAA_pitchers_test)[,-c(1)]

rf_train <- randomForest::randomForest(AAA_pitchers_train$Promoted ~., data = data.frame(trainX), ntree = 100, na.action = na.omit)

test_preds = predict(rf_train, data.frame(testX), type = "response")
print(test_preds)


# 80% Training
#15% Test
#5% Validation

set.seed(21)
validation_sample = sample(nrow(AAA_pitchers), .05*nrow(AAA_pitchers))
AAA_pitchers_validation = AAA_pitchers[validation_sample,]
AAA_pitchers_nonval = AAA_pitchers[-validation_sample,]
#K-fold cross validation
# Usually k is somewhere between 2 and 20
k = 10
nonval_sample = sample(nrow(AAA_pitchers_nonval))
nonval_deciles = quantile(1:nrow(AAA_pitchers_nonval),
                          seq(0,1, by=1/k))
cv_list = list()

for(i in 1:k){
  randomized_dec = nonval_sample[ceiling(nonval_deciles[i]):floor(nonval_deciles[i+1])]
  cv_list[[i]] = AAA_pitchers_nonval[randomized_dec,]
}

pred_list1 = list()
pred_list2 = list()
pred_list3 = list()
pred_list4 = list()
pred_list5 = list()
pred_list6 = list()
pred_list7 = list()
pred_list8 = list()

for(i in 1:k){

  cv_dat = do.call(rbind, cv_list[-i])
  cvX = model.matrix(Promoted ~., data = cv_dat)[,-c(1)]
  rf_cv1 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 5)
  rf_cv2 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 20)
  rf_cv3 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 100)
  rf_cv4 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 125)
  rf_cv5 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 135)
  rf_cv6 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 150)
  rf_cv7 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 200)
  rf_cv8 = randomForest::randomForest(cv_dat$Promoted ~., data = data.frame(cvX), ntree = 250)
  test_dat = cv_list[[i]]
  test_datX = model.matrix(Promoted ~., data=test_dat)[,-c(1)]
  pred_list1[[i]] = predict(rf_cv1, data.frame(test_datX), type = "response")
  pred_list2[[i]] = predict(rf_cv2, data.frame(test_datX), type = "response")
  pred_list3[[i]] = predict(rf_cv3, data.frame(test_datX), type = "response")
  pred_list4[[i]] = predict(rf_cv4, data.frame(test_datX), type = "response")
  pred_list5[[i]] = predict(rf_cv5, data.frame(test_datX), type = "response")
  pred_list6[[i]] = predict(rf_cv6, data.frame(test_datX), type = "response")
  pred_list7[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  pred_list8[[i]] = predict(rf_cv7, data.frame(test_datX), type = "response")
  print(i)

}

cv_preds <- do.call(c, pred_list1)
actual <- do.call(rbind, cv_list)

val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("5 trees")

cv_preds = do.call(c, pred_list2)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("20 trees")

cv_preds = do.call(c, pred_list3)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("100 trees")

cv_preds = do.call(c, pred_list4)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("125 trees")

cv_preds = do.call(c, pred_list5)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("135 trees")

cv_preds = do.call(c, pred_list6)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")

cv_preds = do.call(c, pred_list7)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("200 trees")

cv_preds = do.call(c, pred_list8)
val_preds_mock <- roc(actual$Promoted, cv_preds, plot = TRUE, print.auc = TRUE)
title("250 trees")

# Validation Prediction
cvallX = model.matrix(Promoted ~ .,data=AAA_pitchers_nonval)[,-c(1)]
rf_cvall = randomForest::randomForest(AAA_pitchers_nonval$Promoted ~ ., data=data.frame(cvallX),ntree=150)

valX =model.matrix(Promoted ~ .,data=AAA_pitchers_validation)[,-c(1)]
val_preds = predict(rf_cvall,data.frame(valX),type="response")


# MSE is not that accurate with the data
val_preds_mock <- roc(AAA_pitchers_validation$Promoted, val_preds, plot = TRUE, print.auc = TRUE)
title("150 trees")
