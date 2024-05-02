# What is the best classification method for heart disease risk data?
## The data was downloaded from Kaggle. To create a categorical variable for classification,
# "highRisk" variable was created. "highRisk" = 1 if "Risk" variable is greater than 20, otherwise 0. The value 20 was 
# selected based on information from https://intermountainhealthcare.org/-/media/files/services/heart-care/ascvd-risk-score_062719_kro.pdf.
# Additionally, including highRisk, all of dummy variables are coerced to factors by using as.factor function to carry out classification tree.
# Finally "Risk" variable was removed as 'highRisk" is a categorical variable based on it.
# Note: all dummy variables = 1 means yes.

# In this analysis, the point of interest to evaluate performances of models is test accuracy rate.
# Hence, only the test accuracy rate is considered. This allows us to relax some assumptions that
# model requires. On the other hand, any confidence intervals or p-values shouldn't be interpreted.

data$highRisk <- ifelse(data$Risk > 20, 1,0 ) # creating highRisk variable
data$highRisk <- as.factor(data$highRisk)

## A) Loading packages and data

library("tidyverse")
library("ROCR")
library("MASS")
library("klaR")
library("ICS")

setwd("C:/Users/jhkjhk/Desktop/New folder/Fall 21/STAT 4630")

data <- read_csv("heartrisk.csv")%>%as.data.frame()


##  B) Cleaning data

### i) Checking NA variables
colSums(is.na(data)) # no empty variables

### ii) Assessment of Numerical variables
# The purpose of this is to examine outliers that can impact the model.

sapply(data,is.numeric) # all variables are in numeric 
str(data) # first 5 variables are factors(replications of 1s and 0s)

data[,1:5] <- lapply(data[,1:5],function(x) as.factor(x)) 
str(data) # the first 5 columns are now converted.

par(mfrow=c(1,6))
for (i in 6:ncol(data)){
  boxplot(data[,i], main= colnames(data)[i])
}

# Based on boxplots, only the variable "Risk" has few outliers. This can be examined later if problems
# arise.


## C) EDA for variable selection

# Because "highRisk" is associatd with "Risk", for mathematical visulations, "Risk" is used.

fact.var <- which(sapply(data,is.factor) == "TRUE") #finding factorial variables

dev.off()
for(i in fact.var){
  boxplot(data$Risk ~ data[,i], main= colnames(data)[i])
}

# By just evaluating boxplot, there is some difference in the mean between groups of each categorical 
# variables. So, all variables are used initially.

num.var <- which(sapply(data,is.numeric) == "TRUE")[-5] # numerical variables except for "Risk" b/c it's the response var.

for(i in num.var){
  plot(data$Risk ~ data[,i], main= colnames(data)[i])
}

# Based on the plot, "HDL", and "Cholestrol" look like they don't have any relationship with the response variable.
# However, they both reflect health condition related to the heart, so it's kept for now.
# Because there are only 9 variables, we kept all despite undesirable plots. 

## D) Modelling

# For modeling, simple split of 7-3 is used, 70% training and 30% test. Other advanced like cross validation
# may be implemented to find the best parameters for a model. 


set.seed(12345)
index <- sample(1:nrow(data), nrow(data) *0.7) #sampling
data <- data%>%select(-10) #dropping "Risk" b/c "highRisk" is based on it.
test <- data[-index,] # test
train <- data[index,] # train


### i) Logistic Regression

log.train <- glm(highRisk ~., data=train, family = "binomial")
summary(log.train) # individual parameters are okay based on p-values
like <- log.train$null - log.train$dev
1- pchisq(like,9) # H0: B1...= B9 = 0 Ha: at least 1 != 0
# The p-value is 0, hence the null hypothesis is rejected. At least 1 predictor is useful in the model.

# Performance test
log.pred <- predict(log.train,newdata=test, type="response")
log.rates <- prediction(log.pred,test$highRisk)
roc_res<-performance(log.rates,measure="tpr", x.measure="fpr") # true positive vs. false positive
plot(roc_res, main="ROC Curve")
lines(x=c(0,1), y=c(0,1), col="red")
performance(log.rates, measure="auc")@y.values # auc of 0.97 is very good. 1 = perfect.
table(test$highRisk,log.pred >0.5) # accuracy = 91% 


# The test accuracy is 91% which is desirable. One thing to note is that, often in health data where time 
# is significant for treatment, the confusion matrix may be mimiced by adjusting the threshold value
# to decrease the false negative ratio at the cost of increasing false positive ratio.
# The purpose of this is to avoid situations where patients are not being treating due to false negative.
# This wasn't considered for this analysis b/c the purpose is to find the best working classification method,
# not creating a model to classify patients for treatments.


### ii) Linear Discriminant Analysis
# Linear Discriminant Analysis requires predictors to follow normal distribution. 
lda.train <- lda(highRisk~., data=train[,c(num.var,which(colnames(data)=="highRisk"))])
plot(lda.train, dimen=1, tybe="b")
# The plot generated discrimiant score based on the model. Some overlaps between -1 and 0 could be early
# sign for a poor performance.

lda.pred <- predict(lda.train, test)
table(test$highRisk,lda.pred$class)
mean(test$highRisk == lda.pred$class) # accuracy is about 81.67%

# Despite a worrisome sign in the plot, the lda classification generated pretty good test accuracy.

lda.rates <- prediction(lda.pred$posterior[,2],test$highRisk)
roc_res <- performance(lda.rates, measure="tpr", x.measure="fpr")
plot(roc_res)
lines(x=c(0,1), y=c(0,1), col="red")
performance(lda.rates, measure="auc")@y.values # auc 0.914 # good result

### iii) Quadratic Discriminant Analysis
qda.train <- qda(highRisk~., data=train[,c(num.var,which(colnames(data)=="highRisk"))])
qda.pred <- predict(qda.train,test) 
mean(qda.pred$class == test$highRisk) # accuracy = 82.33%


qda.rates <- prediction(qda.pred$posterior[,2], test$highRisk)
roc_res <- performance(qda.rates, measure= "tpr", x.measure="fpr")
plot(roc_res)
lines(x=c(0,1), y=c(0,1), col="red")
performance(qda.rates,measure="auc")@y.values #auc = 0.9220

### iv) Support Vector Machine

# a) selecting the best performing kernel method on the test data


svm.linear <- svm(highRisk~., data=train[,c(num.var,which(colnames(data)=="highRisk"))], kernel = "linear")
mean(predict(svm.linear,newdata=test[,num.var]) == test$highRisk) # 82.33% test accuracy

svm.rad <- svm(highRisk~., data=train[,c(num.var,which(colnames(data)=="highRisk"))], kernel = "radial")
mean(predict(svm.rad,newdata=test[,num.var]) == test$highRisk) # 85% test accuracy



svm.sig <- svm(highRisk~., data=train[,c(num.var,which(colnames(data)=="highRisk"))], kernel = "sigmoid")
mean(predict(svm.sig,newdata=test[num.var]) == test$highRisk) # 73.33% test accuracy


# For kernel selection, radial kernel performed the best so is chosen.

# b) parameter selection
# Inside radial kernel, the model needs to choose the best parameters which are gamma and cost. # Gamma is 
# the scalier used in the kernel function. Kernel functions determine weights based on the distance
# between observations. Cost determines how much room the model allows for soft-classfications, where
# misclassifications are allowed to prevent overfitting. Ideal approach is cross validation with putting in random values.



tuning <- tune.svm(highRisk~., data=train[,c(num.var,which(colnames(data)=="highRisk"))], kernel = "radial",
         gamma=seq(0,1,by=0.001), cost=seq(0.1,10,by=length(seq(0,1,by=0.001))))

tuning # result gamma = 0.307 and cost = 0.1

#refitting
svm.rad2 <- svm(highRisk~., data=train[,c(num.var,which(colnames(data)=="highRisk"))], kernel = "radial",
                gamma=0.307, cost = 0.1)
mean(predict(svm.rad2,newdata=test[,num.var]) == test$highRisk) # 82.333% test accuracy

# Ironically, on the test data, default value, gamma = 0.25 and cost =1, outperformed the value derived
# by the cross validation. One reason for this is that that there are only 700 training data, and svm generally
# performs better as the data gets bigger.

 # E) Conclusion

# In this analysis, logistic regression performed the best when the performance was measured with the test
# accuracy. I think the biggest reason for this is that discriminant analyses assume multivariate normality
# assumption, which was violated, plus, the assumption only allows for the numeric variables.
# Same condition is applied to support vector machine. However, for the support vector machine, one can overcome
# categorical variables by creating dummy variables for each levels of factors. This wasn't performed in this analsys
# mainly because the data of 700 obs. may not be enough to maximize the effectiveness of high-dimensional approach
# of svm. 

