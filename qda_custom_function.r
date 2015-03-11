setwd("/Users/Chris/Desktop/PSU_Class_Documents/2014_Fall_Semester_Classes/STAT_557_Data_Mining/assignment_1")
library(ggplot2)
library(MASS)
library(ISLR)



full_data <- read.csv(file = "adult_afterProc_edited.csv", header=FALSE, sep = ",")
slim_data <- read.csv(file = "afterpca.csv", header=FALSE, sep = ",")
head(slim_data)
summary(slim_data$V1)

library(RColorBrewer)
gg <- ggplot(diamonds, aes(x=carat, y=price, colour=cut)) + geom_point(size=1.5)
gg + scale_fill_brewer(palette="Oranges")
plotto <- ggplot(slim_data, aes(x = V2, y = V3, colour = V1)) + geom_point(size = 1.5, pex=19)
plotto + ggtitle("Scatterplot by Class")


plotto + geom_point()
# Testing LDA in general #

fit1 <- lda(V1 ~ V2 + V3, data=slim_data,
            na.action="na.omit", CV=TRUE)
fit1
item <- table(slim_data$V1, fit1$class)
diag(prop.table(item, 1))
sum(diag(prop.table(item)))
# Using the R package for LDA gives us a success rate of 83.4%
# doing it from lab book
library(MASS)
lda.fit = lda(y~age+pre4,data=na.omit(train_thor))
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, test_thor)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, test_thor$y)
mean(lda.class==test_thor$y)
# Now to try with QDA
qda.fit <- qda(V1~V2+V3,data=na.omit(train))
qda.fit

qda.class <- predict(qda.fit, test)$class
table(qda.class, test$V1)
## QDA test 2
qda.fit2 <- qda(V1~V2+V3,data=na.omit(train), prior=c(0.5,0.5))
plot(qda2.class, test$V1)
# testing priors at 0.7 and 0.3
qda.fit3 <- qda(V1~V2+V3,data=na.omit(train), prior=c(0.55,0.45))
qda.class3 <- predict(qda.fit3, test)$class
table(qda.class3, test$V1)

# Testing covariance matrix using Bartlett's test #
bartlett.test(V2+V3~V1, slim_train)
length(train)
length(train$V1)

# Setting up a prediction just to try out scatting qda classifier
qda_predict <- data.frame(test, y = as.numeric(predict(qda.fit, test)$class))


plotto <- ggplot(train, aes(x = V2, y = V3, color = V1)) + geom_point()
plotto + stat_contour(aes(x = V2, y = V3, z = V1), data = qda_predict) + ggtitle("QDA Decision Boundaries")


qda2.class <- predict(qda.fit2, test)$class
table(qda2.class, test$V1)
summary(test$V1)
# trying to plot the results of the prediction.
library(adegenet)
data(test)
x <- test$V1
grp <- find.clusters(x, max.n.clust = 2)
clust1 <- 
plotto <- ggplot(slim_data, aes(V1, V1), class=V1, colour=V1)
plotto + geom_point()


## Splitting UP the data frame ##

full_smp_size <- floor(0.75 * nrow(full_data))

## set the seed to make your partition reproductible
set.seed(123)
full_train_ind <- sample(seq_len(nrow(full_data)), size = full_smp_size)

full_train <- full_data[full_train_ind, ]
full_test <- full_data[-full_train_ind, ]




fitqda1 <- qda(V1 ~ V2 + V3, data=na.omit(slim_data), prior=c(1, 1)/2)
fitqda1


## Ok, did that, still not sure why I can't get a fitqda statistics thing.

# doing the stock logit to check it.

income1 <- glm(V1 ~ V2 + V3, data=na.omit(testdata), family=binomial(link="logit"))
summary(income1)







## QDA Function for STAT 557 ##
qda_byhand <- function(train, test){
  # Function for quadratic discriminant analysis
  # function receives dataframe as input, assumes y is in first column.
  # 
  p = ncol(train)
  n = nrow(train)
  k = 2
  # In this function, I'm only writing it for K = 2, but it could be customized
  # to have K have more classes.
  data1 <- data.train[data.train$y==0,2:p]
  data2 <- data.train[data.train$y==1,2:p]
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  pi1 <- n1/n
  pi2 <- n2/n
  ## Means of data sets
  means1 <- apply(data1,2,mean)
  means2 <- apply(data2,2,mean)
  ## Calculate sigma
  var1 <- apply(data1,2,function(x) (x-mean(x)))
  var1 <- (t(var1))%*%(var1)
  var2 <- apply(data2,2,function(x) (x-mean(x)))
  var2 <- (t(var2))%*%(var2)
  sigma <- (var1+var2)/(n-k)
  sigma.inv <- solve(sigma)
  a0 <- as.vector(-(log(pi2/pi1)) + t(.5*(means1+means2))%*%sigma.inv%*%(means2-means1))
  a <- sigma.inv%*%(means2-means1)
  y.test <- data.test[,1]
  xvals <- as.matrix(data.t2[,2:p])%*%a
  predictions <- ifelse(xvals> a0,1,0)
  confusion.matrix <- table(predictions,y.test)
  error.rate <- (sum(confusion.matrix[2,1] + confusion.matrix[1,2]))/(sum(confusion.matrix))
  sensitivity <- (sum(confusion.matrix[2,2]))/(sum(confusion.matrix[,2]))
  specificity <- (sum(confusion.matrix[1,1]))/(sum(confusion.matrix[,1]))
  performance <- list(error=error.rate, sens=sensitivity, spec=specificity)
  return(performance)
}


lda_out <- lda(V1~.,data=train)
qda_out <- qda(V1~.,data=train)
require(ggplot2)
lda_predict <- data.frame(test, y=as.numeric(predict(lda_out, test)$class))
qda_predict <- data.frame(test, y= as.numeric(predict(qda_out, test)$class))


p <-ggplot(train, aes(x=V2, y=V1, color=y)) + geom_point()
p + stat_contour(aes(x=V2, y=V1,z=y), data=lda_predict) + ggtitle("LDA Decision Boundaries")

p + stat_contour(aes(x=V2, y=V1, z=y),data=qda_predict) + ggtitle("QDA Decision Boundaries")

#####################################################
##### QDA with Full Data - 81 dimensions - 1 DV #####
#####################################################

# Now to try with QDA
qda.fit_full <- qda(V1~V2+V3+V4+V5+V6+V8+V9+V10+V11+V12+V13+V14+V15+V16+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V30+V32+V33+V34+V35+V36+V38+V39+V40+V41
                    ,data=na.omit(full_train))
qda.fit_full
# variables causing issues: V7, V17, V29, V31, V37, V42
qda.fit_full.class <- predict(qda.fit_full, full_test)$class
table(qda.fit_full.class, full_test$V1)
# V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12

qda.class <- predict(qda.fit, test)$class
table(qda.class, test$V1)
## Calculating error, specificity, sensitivity.

dim(full_test)
full_model_QDA_ER <- ((225+4084)/11306)*100
full_model_QDA_ER # with test error - 38.1
# Make a table with the inter-training error
qda.fit_full_train.class <- predict(qda.fit_full, full_train)$class
table(qda.fit_full_train.class, full_train$V1)
dim(full_train) # 33916
# training error from qda in full.
((657+12163)/33916)*100
# training error 37.8
# says all arguments must have same length.
length(qda.fit_full.class) # says 11306
length(full_train$V1) # says 33916 - I get it.


####################################################
###### 10 fold cross-validation attempt for qda ####
####################################################

## Splitting UP the data frame ##

slim_smp_size <- floor(0.75 * nrow(slim_data))

## set the seed to make your partition reproductible
set.seed(123)
slim_train_ind <- sample(seq_len(nrow(slim_data)), size = slim_smp_size)

slim_train <- slim_data[slim_train_ind, ]
slim_test <- slim_data[-slim_train_ind, ]

#################################################


qda.fit_slim <- qda(V1~V2+V3,data=na.omit(slim_train))
qda.fit_slim
## Getting error rate for slim_train

qda.fit_full_train.class <- predict(qda.fit_full, full_train)$class
table(qda.fit_full_train.class, full_train$V1)

qda.fit_slim_train.class <- predict(qda.fit_slim, slim_train)$class
table(qda.fit_slim_train.class, slim_train$V1)
##
qda.class_slim <- predict(qda.fit_slim, slim_test)$class
table(qda.class_slim, slim_test$V1)


set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  qda.fit=qda(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

## attempt ##

qda1=qda(V1~V2+V3,data=slim_train,CV=TRUE)
tab1=table(qda1$class,slim_train$V1)
tab1
## Leave one out - just classified everything as 0 again ###

## 10 Fold CV Item ##
library(caret)
misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,3)
}
# testing the misclassification rate function #
misclassification.rate(tab1)
# Gives an error rate of 16.7

#Then each element of flds is a list of indexes for each dataset. 
#If your dataset is called dat, then dat[flds$train,] gets 
#you the training set, dat[ flds[[2]], ] gets you the 
#second fold set, etc.
library(cvTools)
k <- 10 #the number of folds

folds <- cvFolds(NROW(slim_train), K=k)

for(i in 1:k){
  train <- slim_train[folds$subsets[folds$which != i], ] #Set the training set
  validation <- slim_train[folds$subsets[folds$which == i], ] #Set the validation set
  
  newqda <- qda(V1~V2+V3,data=train) #Get your new linear model (just fit on the train data)
  newpred <- predict(newqda, validation)$class #Get the predicitons for the validation set (from the model just fit on the train data)
  tab <- table(newpred, validation$V1)
  print(misclassification.rate(tab)) #Put the hold out prediction in the data set for later use
}





qda.fit_slim1 <- qda(V1~V2+V3,data=slim_data[flds$train,])
qda.fit_slim1
qda.fit_slim_train.class <- predict(qda.fit_slim, slim_train)$class
table(qda.fit_slim_train.class, slim_train$V1)


