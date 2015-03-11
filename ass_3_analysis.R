# assignment_3: Decision Tree analysis

# Including the functions for misclassification rate, specificity, sensitivity.
## 10 Fold CV Item ##

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

setwd("/Users/Chris/Desktop/PSU_Class_Documents/2014_Fall_Semester_Classes/STAT_557_Data_Mining/assignment_3")
#setwd("/Users/cinkpen/Desktop/Classes/Fall_2014_classes/STAT_557/assignment_2")
wholesale <- read.csv("wholesale_data.csv", header=T)
summary(wholesale)
class(wholesale)
class(wholesale$channel)
class(wholesale$fresh)

wholesale$channel <- as.factor(wholesale$channel)
class(wholesale$channel)
summary(wholesale$channel)

## Rpart part ##

tree1 <- rpart(channel ~ fresh+milk+grocery+frozen+detergents_paper+delicassen+lisbon+oporto+other,
      data=wholesale, method="class")

# display cp table
printcp(tree1)
# plot cross-validation results
plotcp(tree1)
# rsq.rpart(tree1) plots approximate R-squared and relative error for different splits (2 plots)
# labels are only appropriate for "anova" method
rsq.rpart(tree1) # says it may not be applicable for this method.

#prints results
print(tree1)
# summary of results
summary(tree1)
# plot decision tree
plot(tree1)
text(tree1)


### classification tree example from Quick R ###

# grow tree

# library(rpart)
# data(kyphosis)
# summary(kyphosis)
# summary(kyphosis$Kyphosis)
# class(kyphosis$Kyphosis)
# 
# fit <- rpart(Kyphosis ~ Age + Number + Start,
#              method="class", data=kyphosis)
# # display the results
# printcp(fit)
# plotcp(fit) # visualize cross-validation results
# summary(fit) # detailed summary of splits
# #plot tree
# plot(fit, uniform=TRUE, main="Classification Tree for Kyphosis")
# text(fit, use.n=TRUE, all=TRUE, cex=.8)
# 
# install.packages("rattle")
# library(rattle)
# prp(fit) # a fast plot.
# fancyRpartPlot(fit)
# 
# prp(tree1)
# fancyRpartPlot(tree1)

###### Splitting Up The Data into Training and Testing Samples ######

# now I'll split this up into training and test
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

tree.wholesale <- tree(channel~.,wholesale_train)
tree.wholesale_pred <- predict(tree.wholesale, wholesale_test, type="class")
tree.wholesale
wholesale_tree_table <- table(tree.wholesale_pred, test.Y)
misclassification.rate(wholesale_tree_table)
sensitivity.rate(wholesale_tree_table)
specificity.rate(wholesale_tree_table)

plot(tree.wholesale)
text(tree.wholesale, pretty = 0)

### Trying some Rpart pruning tutorial ###

# fit <- rpart(Kyphosis ~ Age+Number+Start, method="class", data=kyphosis)
# printcp(fit)
# plotcp(fit)
# # prune the tree 
# pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# 
# # plot the pruned tree 
# plot(pfit, uniform=TRUE, 
#      main="Pruned Classification Tree for Kyphosis")
# text(pfit, use.n=TRUE, all=TRUE, cex=.8)

####################################################################
############ R-part for class assignment cross validation ##########
####################################################################

set.seed(123)
wholesale_train_ind <- sample(seq_len(nrow(wholesale)), size = wholesale_smp_size)

wholesale_train <- wholesale[wholesale_train_ind, ]
wholesale_test <- wholesale[-wholesale_train_ind, ]
wholesale <- data.frame(wholesale)
wholesale_train <- data.frame(wholesale_train)
wholesale_test <- data.frame(wholesale_test)
train.Y <- wholesale_train[,1]
test.Y <- wholesale_test[,1]
#### Running the first testing tree #####

# First I'm just going to run rpart with the training data and then test it
# to predict the test data. No pre-pruning or post-pruning. This is just to 
# get an estimate. The stopping criterion is default I guess and the method
# for splitting is Gini index.
form <- as.formula(channel~.)

ws_tree1 <- rpart(form, wholesale_train, method="class")
# Testing tree error, specificity, sensitivity
ws_tree1_base_pred <- predict(ws_tree1, wholesale_train, type="class")
ws_tree1_base_tab <- table(ws_tree1_base_pred, wholesale_train$channel)
ws_tree1_base_tab
misclassification.rate(ws_tree1_base_tab)
sensitivity.rate(ws_tree1_base_tab)
specificity.rate(ws_tree1_base_tab)

## Predicting with Test Data from Base Model ##
ws_tree1_pred <- predict(ws_tree1, wholesale_test, type="class")
ws_tree1_tab <- table(ws_tree1_pred, test.Y)
ws_tree1_tab
misclassification.rate(ws_tree1_tab)
sensitivity.rate(ws_tree1_tab)
specificity.rate(ws_tree1_tab)
fancyRpartPlot(ws_tree1)
##### Pre-Pruning ######

# First I'm going to pre-prune based on the minimum observations in a split group
# this is the stopping criterion I am choosing. I will run this through for 3 to 15 in 
# multiples of 3.
threes <- c(3, 6, 9, 12, 15)
threes
# First I will get the error rate for the training data.
for(i in threes){
  set.seed(1)
  tree_i <- rpart(form, wholesale_train, method="class", minsplit=i)
  # Testing tree error, specificity, sensitivity
  tree_i_base_pred <- predict(tree_i, wholesale_train, type="class")
  tree_i_base_tab <- table(tree_i_base_pred, wholesale_train$channel)
  #print(misclassification.rate(tree_i_base_tab))
  #print(sensitivity.rate(tree_i_base_tab))
  print(specificity.rate(tree_i_base_tab))
}

## Now I will print out the error rates for the test data using the minimum splitting
# pre-pruning stopping criterion.
for(i in threes){
  set.seed(1)
  tree_i_test <- rpart(form, wholesale_train, method="class", minsplit=i)
  # Testing tree error, specificity, sensitivity
  tree_i_test_pred <- predict(tree_i_test, wholesale_test, type="class")
  tree_i_test_tab <- table(tree_i_test_pred, test.Y)
  #print(misclassification.rate(tree_i_test_tab))
  print(sensitivity.rate(tree_i_test_tab))
  #print(specificity.rate(tree_i_test_tab))
}
# We can see that the error rates are higher for the test data (all around 10%,
# at 15 for minimum split, the error rate was 10.9). The sensitivity and specificity
# were all lower for the test data.

######## Post-Pruning ###########
# here we're going to use the prune.rpart function and prune by cp.
# re-grow the tree to start with the base tree.
tree_post <- rpart(form, wholesale_train, method="class")
tree_post
fancyRpartPlot(tree_post) # just plotting it to take a look at it.
printcp(tree_post)  # displaying the results
plotcp(tree_post)   # visualizing the cross-validation results.
# # prune the tree 
prune_tree_post <- prune(tree_post, cp=0.01)
summary(prune_tree_post)
fancyRpartPlot(prune_tree_post)
# 
# # plot the pruned tree 
plot(prune_tree_post, uniform=TRUE, 
      main="Pruned Classification Tree for Wholesale Data") 
text(prune_tree_post, use.n=TRUE, all=TRUE, cex=.8)
fancyRpartPlot(prune_tree_post)
# And it appears to give me the exact same tree as if I didn't do any pruning.

pruned_pred <- predict(prune_tree_post, wholesale_test, type="class")
pruned_table <- table(pruned_pred, test.Y)
misclassification.rate(pruned_table)
sensitivity.rate(pruned_table)
specificity.rate(pruned_table)

####### This bit of pruning doesn't really seem to be doing much of 
# anything, so I'll try it with the "tree()" function to see if that helps.

# Trying to fix the terrible plot thing that seems to be 
# afflicting my plot screen
par(mfrow = c(1, 1))


Rtree <- tree(form, wholesale_train)
Rtree
summary(Rtree)
plot(Rtree)
text(Rtree, splits=TRUE, pretty = 0, cex=0.6, adj=0.7)
#tile.tree(Rtree, wholesale_train$channel)
# Ok so it made a larger tree, but the tree kind of sucks to look at.
# predicting the training set values.
training_tree <- predict(Rtree, wholesale_train, type="class")
training_table <- table(training_tree, wholesale_train$channel)
training_table
misclassification.rate(training_table)
sensitivity.rate(training_table)
specificity.rate(training_table)

# 10-fold cross validation
for(i in 1:p){
  train <- wholesale_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- wholesale[folds$subsets[folds$which == i], ] #Set the validation set
  loop_tree <- tree(form, train)
  # Testing tree error, specificity, sensitivity
  tree_pred <- predict(loop_tree, validation, type="class")
  tree_tab <- table(tree_pred, validation$channel)
  #print(misclassification.rate(tree_tab))
  #print(sensitivity.rate(tree_tab))
  print(specificity.rate(tree_tab))  
}

# Predicting the test channel values
Rtree.wholesale_pred <- predict(Rtree, wholesale_test, type="class")
Rtree_pred_table <- table(Rtree.wholesale_pred, test.Y)
Rtree_pred_table
misclassification.rate(Rtree_pred_table)
sensitivity.rate(Rtree_pred_table)
specificity.rate(Rtree_pred_table)

###################################################
#### Pre-Pruning based on minimum size of node ####
###################################################

# Regular training based on changing minimum size for nodes

for(i in threes){
  nobs <- nrow(wholesale_train)
  pre_tree <- tree(form, wholesale_train, control=tree.control(nobs, minsize=i))
  pred_pre_tree <- predict(pre_tree, wholesale_train, type="class")
  pre_table <- table(pred_pre_tree, wholesale_train$channel)
  #print(misclassification.rate(pre_table))
  #print(sensitivity.rate(pre_table))
  print(specificity.rate(pre_table))
}



##############################################
######## 10-fold CV with Training ############
##############################################

for(i in 1:p){
  train <- wholesale_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- wholesale_train[folds$subsets[folds$which == i], ] #Set the validation set
  for(j in threes){
    nobs <- nrow(train)
    pre_tree <- tree(form, train, control=tree.control(nobs, minsize=j))
    pred_pre_tree <- predict(pre_tree, validation, type="class")
    pre_table <- table(pred_pre_tree, validation$channel)
    #print(misclassification.rate(pre_table))
    #print(sensitivity.rate(pre_table))
    print(specificity.rate(pre_table))
  } 
}

########################################
#### Predicting the Testing Data #######
########################################

for(i in threes){
  nobs <- nrow(wholesale_train)
  pre_tree <- tree(form, wholesale_train, control=tree.control(nobs, minsize=i))
  pred_pre_tree <- predict(pre_tree, wholesale_test, type="class")
  pre_table <- table(pred_pre_tree, wholesale_test$channel)
  #print(misclassification.rate(pre_table))
  #print(sensitivity.rate(pre_table))
  print(specificity.rate(pre_table))
}






# Now let's prune this mother.
set.seed(3)
cv.Rtree <- cv.tree(Rtree, FUN=prune.misclass)
cv.Rtree
####
# shows us the results.Despite the name, "dev" corresponds with cross-validation
# error rate in this instance. The tree with 9 termianl nodes results in
# the lowest cross validation error rate (with 50 cross-validation errors).
# we plot the error rate as a function of both size and k.

par(mfrow=c(1,1))
plot(cv.Rtree$size, cv.Rtree$dev, type="b")
#plot(cv.Rtree$k, cv.Rtree$dev, type="b")

# we now apply the prune.misclass() function in order to prune the tree to 
# obtain the nine-node tree.
prune.Rtree <- prune.misclass(Rtree, best=7)
plot(prune.Rtree)
text(prune.Rtree, pretty=0)
# Ok so even though the plot looks like it would be 7, let's
# just iterate through all of them to see what the lowest error
# rate would be.
size <- c(2:11)
size
# Predicting for Training Data
for(i in size){
  set.seed(1)
  prune <- prune.misclass(Rtree, best=i)
  # Testing tree error, specificity, sensitivity
  tree_pred <- predict(prune, wholesale_train, type="class")
  tree_tab <- table(tree_pred, wholesale_train$channel)
  #print(misclassification.rate(tree_tab))
  #print(sensitivity.rate(tree_tab))
  print(specificity.rate(tree_tab))
}





########################################

library(cvTools)
p <- 10 #the number of folds

folds <- cvFolds(NROW(wholesale_train), K=p)

for(i in 1:p){
  train <- wholesale_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- wholesale[folds$subsets[folds$which == i], ] #Set the validation set
  loop_tree <- tree(form, train)
  prune <- prune.misclass(loop_tree, best=11)
  # Testing tree error, specificity, sensitivity
  tree_pred <- predict(prune, validation, type="class")
  tree_tab <- table(tree_pred, validation$channel)
  print(misclassification.rate(tree_tab))
  #print(sensitivity.rate(tree_tab))
  #print(specificity.rate(tree_tab))  
}

### Now that 10 fold CV is done, I will 

train <- wholesale_train[folds$subsets[folds$which != i], ] #Set the training set
loop_tree <- tree(form, train)
plot(loop_tree)
text(loop_tree, pretty=0)

# Predicting for Test Data
for(i in size){
  set.seed(1)
  prune <- prune.Rtree <- prune.misclass(Rtree, best=i)
  # Testing tree error, specificity, sensitivity
  tree_pred <- predict(prune, wholesale_test, type="class")
  tree_tab <- table(tree_pred, test.Y)
  #print(misclassification.rate(tree_tab))
  #print(sensitivity.rate(tree_tab))
  print(specificity.rate(tree_tab))
}

library(tree)
## Plotting the Best Tree - 6 nodes
prune.Rtree <- prune.misclass(Rtree, best=6)
tree_6_pred <- predict(prune.Rtree, wholesale_test, type="class")
tree_6_tab <- table(tree_6_pred, test.Y)
#print(misclassification.rate(tree_tab))
#print(sensitivity.rate(tree_tab))
#print(specificity.rate(tree_tab))
tree_6_tab


plot(prune.Rtree)
text(prune.Rtree, pretty=0, cex=0.6, adj=0.8)
prune.Rtree
summary(prune.Rtree)



# Anything else maybe? Random forest stuff?

#####################################################
########### Random Forest Attempts ##################
#####################################################
require(useful)
install.packages("randomForest")
require(randomForest)

w_x <- build.x(form, data=wholesale_train)
w_y <- build.y(form, data=wholesale_train)

# fit the random forest

w_forest <- randomForest(x=w_x, y=w_y)
w_forest
summary(w_forest)
w_forest$confusion
w_forest$err.rate

###################################################################
####### Machine Learning in R - using C50 package #################
###################################################################

install.packages("C50")
library(C50)
c50_base_model <- C5.0(wholesale_train[-1], wholesale_train$channel)
c50_base_model
summary(c50_base_model)
c50_base_pred <- predict(c50_base_model, wholesale_train, type="class")
c50_base_tab <- table(c50_base_pred, wholesale_train$channel)
c50_base_tab
misclassification.rate(c50_base_tab)
sensitivity.rate(c50_base_tab)
specificity.rate(c50_base_tab)


# Predicting using C50
c50_pred <- predict(c50_base_model, wholesale_test, type="class")
c50_tab <- table(c50_pred, test.Y)
c50_tab
misclassification.rate(c50_tab)
sensitivity.rate(c50_tab)
specificity.rate(c50_tab)

## Ok, so we got an error rate of 8.18 based on the base model from C5.0
# sensitivity rate of 86.5 and specificity rate of 94.5
# This error rate is lower than the error rates for K-nearest neighbors at 20-25
# nearest neighbors.

library(MASS)
# Quick Comparison of LDA, QDA, and Logit
lda.fit = lda(form,data=na.omit(wholesale_train))
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, wholesale_test)
lda.class = lda.pred$class
tab1 <- table(lda.class, test.Y)
misclassification.rate(tab1)
## Now to try with QDA
form
qda.fit <- qda(channel~fresh+milk+grocery+frozen+detergents_paper+delicassen+lisbon+oporto, data=wholesale_train)
qda.pred <- predict(qda.fit, wholesale_test)
qda.class <- qda.pred$class
tab2 <- table(qda.class, test.Y)
misclassification.rate(tab2)

## Now to try with Logit

w_log <- glm(channel~fresh+milk+grocery+frozen+detergents_paper+delicassen+lisbon+oporto, 
             data=wholesale_train, family=binomial(link="logit"))

summary(w_log)
log_pred <- predict(w_log, wholesale_test, type="response")
ctab_test <- table(pred=log_pred>0.05, test.Y)
misclassification.rate(ctab_test)
