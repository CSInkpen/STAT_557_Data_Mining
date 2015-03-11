## STAT 557 Assignment 4 Analysis ##
setwd("/Users/Chris/Desktop/PSU_Class_Documents/2014_Fall_Semester_Classes/STAT_557_Data_Mining/assignment_2/assignment_4")
#setwd("/Users/cinkpen/Desktop/Classes/Fall_2014_classes/STAT_557/assignment_2")

misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,3)
}
## Trying my hand at making the sensitivity rate: the equation is TP / (TP + FN)
sensitivity.rate = function(tab){
  sens <- tab[2,2]/(tab[2,2]+tab[2,1])
  signif(sens, 3)
}
# This works, now to do the specificity rate: the equation is TN / (TN + FP)
specificity.rate <- function(tab){
  spec <- tab[1,1]/(tab[1,1] + tab[1,2])
  signif(spec, 3)
}


### Now installing the packages for D-trees ###

#install.packages("rpart")
install.packages("rpart.plot")
install.packages("tree")
install.packages("rattle")
library(tree)
library(ISLR)
library(rattle)
library(rpart)
library(rpart.plot)

wholesale_pca2 <- read.csv("After_PCA_2_data2.csv", header=F)
wholesale <- read.csv("wholesale_data.csv", header=T)
wholesale$channel <- as.factor(wholesale$channel)

depvar <- as.matrix(wholesale[,1])
colnames(depvar)[1] <- "channel"
colnames(wholesale_pca2)[1] <- "PC1"
colnames(wholesale_pca2)[2] <- "PC2"
summary(depvar)
wholesale_pca2 <- cbind(depvar, wholesale_pca2)

summary(wholesale_pca2$channel)
## Splitting UP the data frame ##
wholesale_smp_size <- floor(0.75 * nrow(wholesale))

## set the seed to make your partition reproductible
set.seed(123)
wholesale_train_ind <- sample(seq_len(nrow(wholesale)), size = wholesale_smp_size)

wholesale_train <- wholesale[wholesale_train_ind, ]
wholesale_test <- wholesale[-wholesale_train_ind, ]
wholesale <- data.frame(wholesale)
wholesale_train <- data.frame(wholesale_train)
wholesale_test <- data.frame(wholesale_test)
train.Y <- wholesale_train[,1]
test.Y <- wholesale_test[,1]

######## SPLITTING UP PCA2 DATA ##################
set.seed(123)
wholesale_pca2_train_ind <- sample(seq_len(nrow(wholesale_pca2)), size = wholesale_smp_size)

wholesale_pca2_train <- wholesale_pca2[wholesale_pca2_train_ind, ]
wholesale_pca2_test <- wholesale_pca2[-wholesale_pca2_train_ind, ]
wholesale_pca2 <- data.frame(wholesale_pca2)
wholesale_pca2_train <- data.frame(wholesale_pca2_train)
wholesale_pca2_test <- data.frame(wholesale_pca2_test)
pca2_train.Y <- wholesale_pca2_train[,1]
pca2_test.Y <- wholesale_pca2_test[,1]

summary(pca2_test.Y)




#####################################################
########### Random Forest Attempts ##################
#####################################################
require(useful)
#install.packages("randomForest")
require(randomForest)

# fit the random forest
vars <- c(2:9)
for(i in vars){
  set.seed(1)
  forest <- randomForest(form, data=wholesale_train, mtry=i, ntree=1000)
  pred <- predict(forest, wholesale_test, type="response",
                   norm.votes=TRUE, predict.all=FALSE, proximity=FALSE,)
  tab <- table(pred, wholesale_test$channel)
  print(misclassification.rate(tab))
  #print(sensitivity.rate(rf_tab))
  #print(specificity.rate(rf_tab))
}


### Best error rate was with 9 variables - gave 10% error rate.
x <- wholesale_train[,-1]
y <- wholesale_train[,1]
set.seed(123)
wf_forest <- randomForest(x=x, y=y,
                          ntree=1000, importance=TRUE)
# show the summary
wf_forest
# Training accuracy
pred <- predict(wf_forest, wholesale_train, type="response")
view <- as.matrix(pred, row.names=FALSE)
view1 <- as.matrix(wholesale_train$channel)
visual_check <- cbind(view, view1)
tab1 <- table(view, wholesale_train$channel)
print(misclassification.rate(tab1))
print(sensitivity.rate(tab1))
print(specificity.rate(tab1))
tab1
## Impressive, 0% error rate!
### Time for a little 10-fold cross validation
library(cvTools)
p <- 10 #the number of folds
folds <- cvFolds(NROW(wholesale_train), K=p)
for(i in 1:p){
  train <- wholesale_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- wholesale_train[folds$subsets[folds$which == i], ] #Set the validation set
  forest <- randomForest(x=train[,-1], y=train[,1],ntree=1000, importance=TRUE)
  # Testing tree error, specificity, sensitivity
  pred <- predict(forest, validation, type="class")
  tab <- table(pred, validation$channel)
  #print(misclassification.rate(tab))
  #print(sensitivity.rate(tab))
  print(specificity.rate(tab))  
}





pred1 <- predict(wf_forest, wholesale_test, type="response")
tab2 <- table(pred1, test.Y)
tab2
print(misclassification.rate(tab2))
print(sensitivity.rate(tab2))
print(specificity.rate(tab2))
## Not so good, still got the 11.8% error rate. 

# Checking out variable importance
varImp <- importance(wf_forest)
varImp
varImpPlot(wf_forest, type=1)
##################################################
####### Random Forest with the PCA2 Data #########
##################################################
x2 <- wholesale_pca2_train[,-1]
y2 <- wholesale_pca2_train[,1]
set.seed(123)
pca_forest <- randomForest(x=x2, y=y2,
                          ntree=1000, importance=TRUE)
# show the summary
pca_forest
# Training accuracy
pred <- predict(pca_forest, wholesale_pca2_train, type="response")
tab_pca <- table(pred, wholesale_pca2_train$channel)
print(misclassification.rate(tab_pca))
print(sensitivity.rate(tab_pca))
print(specificity.rate(tab_pca))
tab_pca

## Impressive, 0% error rate!
### Time for a little 10-fold cross validation
library(cvTools)
p <- 10 #the number of folds
folds <- cvFolds(NROW(wholesale_pca2_train), K=p)
for(i in 1:p){
  train <- wholesale_pca2_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- wholesale_pca2_train[folds$subsets[folds$which == i], ] #Set the validation set
  forest <- randomForest(x=train[,-1], y=train[,1],ntree=1000, importance=TRUE)
  # Testing tree error, specificity, sensitivity
  pred <- predict(forest, validation, type="class")
  tab <- table(pred, validation$channel)
  #print(misclassification.rate(tab))
  #print(sensitivity.rate(tab))
  print(specificity.rate(tab))  
}




pca_forest
# Training accuracy
pred <- predict(pca_forest, wholesale_pca2_train, type="response")
tab_pca <- table(pred, wholesale_pca2_train$channel)
print(misclassification.rate(tab_pca))
print(sensitivity.rate(tab_pca))
print(specificity.rate(tab_pca))
tab_pca


pred1 <- predict(pca_forest, wholesale_pca2_test, type="response")
tab2 <- table(pred1, pca2_test.Y)
tab2
print(misclassification.rate(tab2))
print(sensitivity.rate(tab2))
print(specificity.rate(tab2))


## Plotting Variable Importance for PCA Data ##
# just making sure I didn't do something wrong here.
varImppc <- importance(pca_forest)
varImppc
varImpPlot(pca_forest, type=1)


#####################################################
######### Support Vector Machines ###################
#####################################################
# install.packages("kernlab")
# library('kernlab')
# svm_train <- ksvm(form, data=wholesale_train, kernel='rbfdot', 
#                   C=10, prob.model=T, cross=5)
# test.Y <- as.matrix(test.Y)
# svm_train
# test.Y$svm_pred <- predict(svm_train, data=wholesale_test, type='response')
# print(with(test.Y, table(y=test.Y, svmPred=svm_pred)))

install.packages('e1071')
library('e1071')
svm_try <- svm(form, data=wholesale_train)
svm_try
print(svm_try)
summary(svm_try)
plot(svm_try, data=wholesale_train, formula = detergents_paper~fresh)


## Predicting the training set 

svm_train_pred <- predict(svm_try, wholesale_train)
tab <- table(svm_train_pred, train.Y)
tab
print(misclassification.rate(tab))
print(sensitivity.rate(tab))
print(specificity.rate(tab))

## 10-fold cross validation ##
p <- 10 #the number of folds
folds <- cvFolds(NROW(wholesale_train), K=p)
for(i in 1:p){
  train <- wholesale_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- wholesale_train[folds$subsets[folds$which == i], ] #Set the validation set
  svm_cv <- svm(form, data=train)
  # Testing tree error, specificity, sensitivity
  pred <- predict(svm_cv, validation, type="class")
  tab <- table(pred, validation$channel)
  #print(misclassification.rate(tab))
  #print(sensitivity.rate(tab))
  print(specificity.rate(tab))  
}

## Predicting the test set 

svm_test_pred <- predict(svm_try, wholesale_test)
tab <- table(svm_test_pred, test.Y)
tab
print(misclassification.rate(tab))
print(sensitivity.rate(tab))
print(specificity.rate(tab))

################################################
############### SVM with PCA2 Data #############
################################################

svm_pca2_try <- svm(form, data=wholesale_pca2_train)
svm_pca2_try
#print(svm_try)
#summary(svm_try)
plot(svm_pca2_try, data=wholesale_pca2_train, formula = PC1~PC2)




## Predicting the training set 

svm_pca2_train_pred <- predict(svm_pca2_try, wholesale_pca2_train)
tab <- table(svm_pca2_train_pred, pca2_train.Y)
tab
print(misclassification.rate(tab))
print(sensitivity.rate(tab))
print(specificity.rate(tab))

## 10-fold cross validation ##
p <- 10 #the number of folds
folds <- cvFolds(NROW(wholesale_pca2_train), K=p)
for(i in 1:p){
  train <- wholesale_pca2_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- wholesale_pca2_train[folds$subsets[folds$which == i], ] #Set the validation set
  svm_cv <- svm(form, data=train)
  # Testing tree error, specificity, sensitivity
  pred <- predict(svm_cv, validation, type="class")
  tab <- table(pred, validation$channel)
  #print(misclassification.rate(tab))
  #print(sensitivity.rate(tab))
  print(specificity.rate(tab))  
}

## Predicting the test set 

svm_test_pred <- predict(svm_pca2_try, wholesale_pca2_test)
tab <- table(svm_test_pred, pca2_test.Y)
tab
print(misclassification.rate(tab))
print(sensitivity.rate(tab))
print(specificity.rate(tab))




