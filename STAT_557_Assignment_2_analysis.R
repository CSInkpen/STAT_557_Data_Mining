## STAT 557 Assignment 2 Analysis ##
setwd("/Users/Chris/Desktop/PSU_Class_Documents/2014_Fall_Semester_Classes/STAT_557_Data_Mining/assignment_2")
#setwd("/Users/cinkpen/Desktop/Classes/Fall_2014_classes/STAT_557/assignment_2")
wholesale <- read.csv("wholesale_data.csv", header=T)
summary(wholesale)
class(wholesale)
class(wholesale$channel)
class(wholesale$fresh)
descrip_table <- data.frame(summary(wholesale))
class(descrip_table)
## Writing this to a table via csv

write.csv(descrip_table, file = "descrip_table.csv")
summary(wholesale$channel)

## Descriptive Statistics for Wholesale Dataset ##

summary(wholesale)
# getting just the HoReCa and the Retail stores to look at descriptives.

horeca <- subset(wholesale, channel == 0)
retail <- subset(wholesale, channel == 1)
summary(horeca)
horeca_descrip_table <- data.frame(summary(horeca))
write.csv(horeca_descrip_table, file = "horeca_descrip_table.csv")
summary(retail$channel)
summary(horeca$channel)
retail_descrip_table <- data.frame(summary(retail))
write.csv(retail_descrip_table, file = "retail_descrip_table.csv")

##### Plotting #####
library(RColorBrewer)
library(ggplot2)
plotto <- ggplot(wholesale, aes(x = fresh, y = milk, colour = channel)) + geom_point(size = 1.5, pex=19)
plotto + ggtitle("Scatterplot by Class")

## Plotting using PC1 and PC2
# first, rename the channel variable as such
colnames(wholesale_pc3)[1] <- "channel"
plottopca3 <- ggplot(wholesale_pc3, aes(x = PC1, y = PC2, colour = channel)) + geom_point(size = 1.5, pex=19)
plottopca3 + ggtitle("Scatterplot by Class for Reduced Data")
# we can see it's well reduced now.

## Splitting UP the data frame ##

wholesale_smp_size <- floor(0.75 * nrow(wholesale))

## set the seed to make your partition reproductible
set.seed(123)
wholesale_train_ind <- sample(seq_len(nrow(wholesale)), size = wholesale_smp_size)

wholesale_train <- wholesale[wholesale_train_ind, ]
wholesale_test <- wholesale[-wholesale_train_ind, ]

### first trying to do PCA by removing a variable from the wholesale dataset - one
# of the categorical regional variables to see if that keeps the matrix from being
# singular

omit <- names(wholesale) %in% c("other") 
new_wholesale <- wholesale[!omit]
fa.parallel(new_wholesale[, -1], fa = "pc", n.iter = 100, 
            show.legend = FALSE, main = "Scree plot with parallel analysis")
# scree plots recommends 3 components
install.packages("GPArotation")
library(GPArotation)
wholesale_pc <- principal(new_wholesale[, -1], nfactors = 4, score = TRUE)
wholesale_pc

# Just trying out exploratory PCA - In factor.scores, the correlation matrix is singular (because of
# the dummy variables I think) - it says an approximation was used.
fa.parallel(wholesale[, -1], fa = "pc", n.iter = 100, 
            show.legend = FALSE, main = "Scree plot with parallel analysis")

# First I'm going to remove one of the dummy variables because otherwise it keeps telling me
# the matrix is singular.


# plot appears to show that 4 principal components should be used (they have eigen values over 1)
install.packages("GPArotation")
library(GPArotation)
pctry_std <- principal(centered_wholesale[,-1], nfactors = 3, score = TRUE, rotate = "none")
pctry_std
head(pctry_std$scores)
wholesale_pc3 <- data.frame(wholesale[,1])
pcs <- data.frame(pctry$scores)
wholesale_pc3 <- cbind(wholesale_pc3, pcs)
# Now we have the pc3 dataset, we'll need to split that up into training and test
# but we've got the full, and the 3 PCs data. 
# now writing it to a csv to send to the other kiddos.

write.csv(wholesale_pc3, file = "wholesale_pc3.csv")

summary(wholesale)
cor(wholesale$channel, pctry$scores)
pca_wholesale <- data.frame(pctry$scores)
head(pca_wholesale)

# centering the data.
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}
centered_data <- center_colmeans(new_wholesale[, -1])

## another way to try to get principal components using the princomp() function #
pca_data <- centered_wholesale[, -1]
fit <- princomp(pca_data[,1:8], cor=FALSE, scores = TRUE)
summary(fit)
loadings(fit)
summary(fit)
head(fit$scores)

# Ok, I can't get it to match Yongjing's so I'll just use his.
# bringing in Yongjing's PCA2 dataset
wholesale_pca2 <- read.csv("After_PCA_2_data2.csv", header=FALSE)
wholesale_pca2 <- cbind(wholesale$channel, wholesale_pca2)
class(wholesale$channel)
# good, it's a factor
summary(wholesale$channel)
# it's the same. So that one is good to go.

# bringing in Yongjing's PCA3 dataset
wholesale_pca3 <- read.csv("After_PCA_3_data2.csv", header=FALSE)
wholesale_pca3 <- cbind(wholesale$channel, wholesale_pca3)

## Plotting using PC1 and PC2 - quick change the colnames for the pca2 and pca3 datasets.
colnames(wholesale_pca2)[2] <- "PC1"
colnames(wholesale_pca2)[3] <- "PC2"
colnames(wholesale_pca3)[2] <- "PC1"
colnames(wholesale_pca3)[3] <- "PC2"
colnames(wholesale_pca3)[4] <- "PC3"
plottopca2 <- ggplot(wholesale_pca2, aes(x = PC1, y = PC2, colour = channel)) + geom_point(size = 1.5, pex=19)
plottopca3 + ggtitle("Scatterplot by Class for Reduced Data")
# we can see it's well reduced now.

#########################################################################
########## K-Nearest Neighbor Analysis for full and pca datasets ########
#########################################################################

library(class)
## 10-Fold Cross Validation Scripts ##

## 10 Fold CV Item ##
library(caret)
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

# testing the misclassification rate function #
#misclassification.rate(tab1)
# Gives an error rate of 16.7


## We may or may not even need this but I thought it might be useful to have.

install.packages("RWeka", dependencies = TRUE)
library(RWeka)
names(wholesale[,2:9])
IVs <- c(names(wholesale[,2:9]))
IVs
classifier <- IBk(channel ~ fresh+milk+grocery+frozen+detergents_paper+delicassen+lisbon+oporto, data = wholesale)
summary(classifier)
# checking why this isn't working, it might be an issue because channel is an "integer"
data(iris)
class(iris$Species)
# just to check in iris as to what their class was, species was a factor
wholesale$channel_f <- factor(wholesale$channel)
class(wholesale$channel_f)
# doing the same, but for the pca data
wholesale_pc3$channel <- factor(wholesale_pc3$channel)
class(wholesale_pc3$channel)
# they're both factors in their respective datasets, now let's give it a whirl.
classifier <- IBk(channel_f ~ fresh+milk+grocery+frozen+detergents_paper+delicassen+lisbon+oporto, data = wholesale)
summary(classifier)
# Ok, now it worked, now i've got a classifier that 100% got right the class.
# Now we'll try it specifying the K and such and doing crossfold on training
# and test data. This will just be a run through since I think I'll want to 
# actually see that it's splitting up the training and test data using
# the "class" library from ISLR.

classifier <- IBk(channel_f ~ fresh+milk+grocery+frozen+detergents_paper+delicassen+lisbon+oporto,
                  data = wholesale, control = Weka_control(K = 20, X = TRUE))
evaluate_Weka_classifier(classifier, numFolds = 10)
classifier

# Now just taking a look at this with the PCA dataset
classifier_pca3 <- IBk(channel ~ +PC1+PC2+PC3,
                  data = wholesale_pc3, control = Weka_control(K = 20, X = TRUE))
evaluate_Weka_classifier(classifier_pca3, numFolds = 10)
classifier_pca3
# Ok, it gives me a 10% error rate (worse than non-PCA) and it uses 9 neighbors, more
# than non-pca (which used 6). This makes sense since we have less variance captured (69%)
# so we need more cases to classify our dependent variable correctly.


###### Using the Class Library from ISLR ####
summary(wholesale$channel_f)
total <- 298 + 142
pct_retail <- 142/total
pct_retail
# 32.27 cases are retail stores. So we have to beat 32.27 error rate, because
# if we just guessed hotel, we would be wrong 32.27 times.

# So first thing I will do is center the data so we have standardized data for
# the full set. I'll remove the channel variable, obviously and also omit one of
# the reference groups (other)

# first, I'm just going to convert the channel variable as factor and get rid of
# the original channel_f from the dataset

wholesale$channel <- factor(wholesale$channel)
class(wholesale$channel)
wholesale <- wholesale[,1:10]
centered_wholesale <- scale(wholesale[,-1])
#testing if variance is 1
var(wholesale[,2])
var(wholesale[,3])
# testing if variance is 1 on centered data
var(centered_wholesale[,2])
var(centered_wholesale[,3])
# it is.
# now I'll re-cbind the outcome variable to the data.
centered_wholesale <- cbind(wholesale$channel, centered_wholesale)
colnames(centered_wholesale)[1] <- "channel"
class(centered_wholesale)
# now I'll split this up into training and test
## Splitting UP the data frame ##
cent_wholesale_smp_size <- floor(0.75 * nrow(centered_wholesale))

## set the seed to make your partition reproductible
set.seed(123)
cent_wholesale_train_ind <- sample(seq_len(nrow(centered_wholesale)), size = cent_wholesale_smp_size)

centered_wholesale_train <- centered_wholesale[cent_wholesale_train_ind, ]
centered_wholesale_test <- centered_wholesale[-cent_wholesale_train_ind, ]
centered_wholesale <- data.frame(centered_wholesale)
centered_wholesale_train <- data.frame(centered_wholesale_train)
centered_wholesale_test <- data.frame(centered_wholesale_test)

  
  
  
train.X <- centered_wholesale_train[,-1]
test.X <- centered_wholesale_test[,-1]
train.Y <- centered_wholesale_train[,1]
test.Y <- centered_wholesale_test[,1]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
tab1 <- table(knn.pred, test.Y)
tab1
misclassification.rate(tab1)
sensitivity.rate(tab1)
specificity.rate(tab1)
# This gives us an error rate of 13.6%. This is not bad for K = 1, it's better
# than just guessing HoReCa, which would give us an error of 32.76% or something.

# now I'm going to iterate through 1 to 20 for k
k = 30
for(i in 1:k){
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=i)
tab <- table(knn.pred, test.Y)
#print(misclassification.rate(tab))
#print(sensitivity.rate(tab))
print(specificity.rate(tab))
}

## Now, this is silly, but I'm generating the training error rate without
# 10-fold just to confirm that k=1 should have an error rate of 0.

k = 30
for(i in 1:k){
  set.seed(1)
  knn.pred <- knn(train.X, train.X, train.Y, k=i)
  tab <- table(knn.pred, train.Y)
  print(misclassification.rate(tab))
}
# and it does, that's good.




########################################

library(cvTools)
p <- 10 #the number of folds

folds <- cvFolds(NROW(train.X), K=p)
train.Y <- as.matrix(train.Y)
dim(train.Y)

for(i in 1:p){
  train <- train.X[folds$subsets[folds$which != i], ] #Set the training set
  validation <- train.X[folds$subsets[folds$which == i], ] #Set the validation set
  Y.train <- train.Y[folds$subsets[folds$which != i], ] # set the training Y
  Y.validation <- train.Y[folds$subsets[folds$which == i], ] # set the validation Y
  knn.pred <- knn(train, validation, Y.train, k=30)
  tab <- table(knn.pred, Y.validation)
  print(misclassification.rate(tab))
}

#######################################################
### PCA 2 KNN stuff ###################################
#######################################################

## Splitting UP the data frame ##
wholesale_pca2_smp_size <- floor(0.75 * nrow(wholesale_pca2))

## set the seed to make your partition reproductible
set.seed(123)
wholesale_pca2_train_ind <- sample(seq_len(nrow(wholesale_pca2)), size = wholesale_pca2_smp_size)

wholesale_pca2_train <- wholesale_pca2[wholesale_pca2_train_ind, ]
wholesale_pca2_test <- wholesale_pca2[-wholesale_pca2_train_ind, ]
wholesale_pca2 <- data.frame(wholesale_pca2)
wholesale_pca2_train <- data.frame(wholesale_pca2_train)
wholesale_pca2_test <- data.frame(wholesale_pca2_test)



pca2_train.X <- wholesale_pca2_train[,-1]
pca2_test.X <- wholesale_pca2_test[,-1]
pca2_train.Y <- wholesale_pca2_train[,1]
pca2_test.Y <- wholesale_pca2_test[,1]
pca2_train.Y <- as.matrix(pca2_train.Y)
dim(pca2_train.Y)
pca2_test.Y <- as.matrix(pca2_test.Y)
dim(pca2_test.Y)
# ok, when running the knn pred thing, it's saying dims of test and train differ. Going to double check this.
dim(pca2_train.X)
dim(pca2_test.X) # Oh I see, for some reason, I must've kept the actual wholesale for test, mistake.
# all fixed, 2 columns for both.

# now I'm going to iterate through 1 to 30 for k
k = 30
for(i in 1:k){
  set.seed(1)
  knn.pred <- knn(pca2_train.X, pca2_test.X, pca2_train.Y, k=i)
  tab <- table(knn.pred, pca2_test.Y)
  #print(misclassification.rate(tab))
  #print(sensitivity.rate(tab))
  print(specificity.rate(tab))
}



#######################################################
### PCA 3 KNN stuff ###################################
#######################################################
## Splitting UP the data frame ##
wholesale_pca3_smp_size <- floor(0.75 * nrow(wholesale_pca3))

## set the seed to make your partition reproductible
set.seed(123)
wholesale_pca3_train_ind <- sample(seq_len(nrow(wholesale_pca3)), size = wholesale_pca3_smp_size)

wholesale_pca3_train <- wholesale_pca3[wholesale_pca3_train_ind, ]
wholesale_pca3_test <- wholesale_pca3[-wholesale_pca3_train_ind, ]
wholesale_pca3 <- data.frame(wholesale_pca3)
wholesale_pca3_train <- data.frame(wholesale_pca3_train)
wholesale_pca3_test <- data.frame(wholesale_pca3_test)



pca3_train.X <- wholesale_pca3_train[,-1]
pca3_test.X <- wholesale_pca3_test[,-1]
pca3_train.Y <- wholesale_pca3_train[,1]
pca3_test.Y <- wholesale_pca3_test[,1]
pca3_train.Y <- as.matrix(pca3_train.Y)
dim(pca3_train.Y)
pca3_test.Y <- as.matrix(pca3_test.Y)
dim(pca3_test.Y)
# ok, when running the knn pred thing, it's saying dims of test and train differ. Going to double check this.
dim(pca3_train.X)
dim(pca3_test.X) # Oh I see, for some reason, I must've kept the actual wholesale for test, mistake.
# all fixed, 2 columns for both.
# now I'm going to iterate through 1 to 30 for k
k = 30
for(i in 1:k){
  set.seed(1)
  knn.pred <- knn(pca3_train.X, pca3_test.X, pca3_train.Y, k=i)
  tab <- table(knn.pred, pca3_test.Y)
  #print(misclassification.rate(tab))
  #print(sensitivity.rate(tab))
  print(specificity.rate(tab))
}

##### Attempt to plot the two distributions

set.seed(1)
knn.pred <- knn(pca3_train.X, pca3_test.X, pca3_train.Y, k=20)
tab <- table(knn.pred, pca3_test.Y)
#print(misclassification.rate(tab))
#print(sensitivity.rate(tab))
#print(specificity.rate(tab))

set.seed(1)
knnPred <- function(df){
  knnDecision <- knn(train.X, text.X, train.Y, k=20,prob=TRUE)
  if
}


