setwd("C:/Users/jhkjhk/Desktop/New folder/R Project/House_Price")

library("tidyverse")

house <- read_csv("House_Price.csv") %>%as.data.frame()


#  A) Data cleaning
dim(house)

colSums(is.na(house)) # checking for missing values

# Is it okay to remove "n_hos_beds"?

plot(x= house[-which(is.na(house$n_hos_beds)), "n_hos_beds"], y=house$price[-which(is.na(house$n_hos_beds))])

# based on the plot, there seems to be no relationship between "price" and "n_hos_beds" thus removed.

house <- house%>%select(-n_hos_beds)

str(house) # exploring variables

# The vast majority of predictors are numeric.

# i) Let's check the levels of non-numeric variables.
# Since there are only about 500 obs., the dataset has limitation on the number of levels a categorical predictor can take.

factors <- names(sapply(house,is.character)[sapply(house,is.character)]) #identifying chr factors

house[,factors] <- lapply(house[,factors],as.factor) # chr columns are now factor columns

lapply(house[,factors],table) # "bus_ter" just has "yes" thus not needed.

house <- house %>% select(-bus_ter)

# ii) numeric columns

numeric <- names(sapply(house,is.numeric)[sapply(house,is.numeric)])



for(i in 2:length(numeric)){
  plot(x=house[,numeric[i]], y=house[,"price"], main= numeric[i])
}

# possible relationship between predictors and the price

# crime:  negative exp
# resid_area: polynomial
# air_qual: negative exp
# room_num: linear
# age: almost no relationship
# dist1: log
# dist2: log
# dist3: log
# dist4: log
# teachers: higher order
# poor_pro: neg exp
# n_hot_rooms: undecided
# rainfall: almost no relationship
# parks: polynomial

# multicollinearity
pairs(house[,numeric[-1]], upper.panel = NULL)

# Based on the plot,
# dist1,2,3, and 4 have high correlations between each other.
# Thus, use only 1 of them to prevent multicollinearity issue.
# Additionally, parks and air_qual have high correlation as well.

cor(house[,c("dist1","dist2","dist3","dist4")]) # indeed extremely high cor, only dist1 is used.

house <- house %>%select(-c("dist2", "dist3", "dist4"))
cor(house$air_qual, house$parks) # high as well

hist(house$air_qual)
hist(house$parks)

# based on the histogram, parks look to be more ideally distributed because air_qual has more skewness
# and random peaks that requires additional analysis.

house2 <- house%>%select(-"air_qual")

# Exploring the response variable
hist(house$price) # Although there are only 500 obs., I think the data contains the typical characteristics
# of house data, where there are fewer observations as the price goes up.

# B) Modelling

# Because there are only 500 obs. 9-1 proportion is used for training and testing data.

set.seed(45454)
index <- sample(1:500,450) #random sampling
train <- house2[index,] # train data
test <- house2[-index,] # test data


# i) Multiple Linear Regression
linear.mod <- lm(price ~., data=train)
summary(linear.mod)
par(mfrow=c(2,2))
plot(linear.mod)

# based on residual plots,
# one critical problem is that residuals are in a "U" shaped pattern. This indicates that the model
# hasn't identified all of the major relationships that can make residuals in random, which are variations
# that are impossible to get rid of. Furthermore, the residuals are far from normal distribution
# based on the qqplot. This is a normal outcome when dealing with house price data because of huge skewness
# with the price.

linear.mod2 <- lm(log(price) ~., data=train)
summary(linear.mod2)
plot(linear.mod2)

dev.off()
plot(x="", y="", ylim=c(min(log(house$price)),max(log(house$price))), xlim=c(1,500))
lines(log(house$price),col="blue")
lines(linear.mod2$fitted.values, col="red")
legend(20,2, legend=c("observed","fitted"), col=c("blue","red"), lty=1)

# the linear model tends to vary much greater than the actual model.
# I think the major issue with this is that the model cannot identify latent variables like location
# that often impacts the price much greater than observed predictors like the size of a house.

summary(linear.mod2)

# Before moving to more advanced modelling, does removing nonimportant variables improve the model?

linear.mod3 <- lm(log(price) ~ crime_rate +room_num + dist1 + teachers+poor_prop + airport + parks, data=train)
summary(linear.mod3)

#Nested F test
#H0: predictors not in linear.mod3 are = 0
#Ha: at least 1 of those are != 0
anova(linear.mod3,linear.mod2)
# The null is failed to be rejected. Thus, linear.mod3 is better.

plot(x="", y="", ylim=c(min(log(house$price)),max(log(house$price))), xlim=c(1,500))
lines(log(house$price),col="blue")
lines(linear.mod3$fitted.values, col="red")
lines(linear.mod2$fitted.values, col="yellow")
legend(20,2, legend=c("observed","nested", "full"), col=c("blue","red", "green"), lty=1)

summary(linear.mod2)
summary(linear.mod3)
summary(linear.mod)

# The major reason why there isn't much difference between model2 and model3 is the magnitude of coefficients.
# The magnitude of coefficients, in the model2, or even in the first model, isn't big enough to make huge impact
# on the outcome.

# ii) playing around with polynomials

# From the part i model3, the most impractical coefficients, in terms of the magnitude of coefficients, are
# "room_num" and "parks". 

poly.mod <- lm(price ~ poly(room_num,2)+poly(parks,2), data=train)
summary(poly.mod) # polynomial of park isn't significant. But, p-value is not that big to remove so keeping it for now.
par(mfrow=c(2,2))
plot(poly.mod)

# The pattern existed in residual plots of linear model is mitigated.
# However, normality assumption of residuals is still not met.
# Additionally, not mentioned before, the residual plot is stil signalizing potential outliers/influential points.
# These are few observations that are strongly varied from other existing observations with enough power
# to impact the model. Again, quick solution is logging the response variable.


dev.off()
poly.mod <- lm(log(price) ~ poly(room_num,2)+poly(parks,2), data=train)

plot(x="", y="", ylim=c(min(log(house$price)),max(log(house$price))), xlim=c(1,500))
lines(log(house$price),col="blue")
lines(poly.mod$fitted.values, col="red")
legend(20,12, legend=c("observed","polynomial"), col=c("blue","red"), lty=1)

# The model is doing pretty well between log(price) 3.0 and 3.5. To values greater than or less than
# the interval, the model is not perfoming as well.

fit.vals <- matrix(NA,nrow=nrow(train),ncol=10)
colnames(fit.vals) <- paste("poly",1:10)

for(i in 1:10){
  poly.fit <- lm(log(price) ~ poly(room_num,i)+poly(parks,i), data=train)
  fit.vals[,i] <- poly.fit$fitted.values
}

plot(x="", y="", ylim=c(min(log(house$price)),max(log(house$price))), xlim=c(1,500))
lines(log(house$price),col="blue")
lines(fit.vals[,9], col="red")
lines(fit.vals[,10], col="black")
legend(20,2, legend=c("observed","poly, 9", "poly, 10"), col=c("blue","red","black"), lty=1)

# Visually, there isn't dramatic improvement on the training data.
# For more accurate decision, let's compare training RMSE.

training.rmse <- matrix(NA,nrow=10)
rownames(training.rmse) <- paste("poly",1:10)





for(i in 1:10){
  rmse <- sqrt(mean((fit.vals[,i]-log(train$price))^2))
  training.rmse[i,] <- exp(rmse)
}

plot(training.rmse)

# RMSE is converted back to the original value. 
# Based on the RMSE plot, the ideal amount of polynomial degree is 6 because
# there isn't much improvement after 6.

# linear vs polynomial on the test data.
poly.mod6 <- lm(log(price) ~ poly(room_num,6)+poly(parks,6), data=train)

linear.test <- predict(linear.mod3,newdata=test)
poly.test <- predict(poly.mod6,newdata=test)

exp(sqrt(mean((linear.test-log(test$price))^2)))
exp(sqrt(mean((poly.test-log(test$price))^2)))

# On the test data, the polynomial model with degrees of 6 performed worse than the training data.
# Just for verfication, what about on the training data?

exp(sqrt(mean((linear.mod3$residuals)^2)))
exp(sqrt(mean((poly.mod6$residuals)^2)))


# linear.mod3 performed better on the training data, which doesn't make sense. Polynomial is more-fitting
# than the linear term. One possible reason for this is poly() generates orthogonal polynomials.
# This is more preferred because it reduces multicollinearity issue. Another reason is that
# omittied variables in the polynomial model plays some role in predicting the response variable.

test.1 <- lm(log(price)~room_num+parks,data=train)
test.2 <- lm(log(price) ~ poly(room_num,6)+poly(parks,6), data=train)
test.3 <- lm(log(price) ~ poly(room_num,6,raw=T)+poly(parks,6,raw=T), data=train)

exp(sqrt(mean((test.1$residuals^2))))
exp(sqrt(mean((test.2$residuals^2))))
exp(sqrt(mean((test.3$residuals^2))))
# poly's raw option doesn't have any impact. Thus, the underperformance of polynomial is from the 
# omitted variables.

# Updating linear.mod3 with poly

training.rmse <- matrix(NA,nrow=10)
rownames(training.rmse) <- paste("poly",1:10)

for(i in 1:10){
  fit <- lm(log(price) ~ crime_rate + poly(room_num,i) + dist1 + 
              teachers+poor_prop + airport + poly(parks,i), data=train)
  training.rmse[i,] <- exp(sqrt(mean(fit$residuals^2)))
}

plot(training.rmse) # polynomial with degrees=4 is the ideal value.

linear_poly <- lm(log(price) ~ crime_rate +poly(room_num,4) + 
                    dist1 + teachers+poor_prop + airport + poly(parks,4), data=train)
summary(linear_poly) #park poly=4 isn't needed

linear_poly <- lm(log(price) ~ crime_rate +poly(room_num,4) + 
                    dist1 + teachers+poor_prop + airport + poly(parks,3), data=train)
summary(linear_poly) # good.

exp(sqrt(mean((predict(linear_poly,newdata=test) - log(test$price))^2))) #improved than before but still big
median(house$price) #the rmse is roughly around 5% of the median houseprice

# iii) nls: nonlinear least sqaures regression

nls.mod <- nls(price~a*exp(room_num*b), data=train, start=list(a=1,b=1), trace=T)
summary(nls.mod)
plot(predict(nls.mod,data=train)-train$price)# residuals are random across 0


plot(x="",y="",xlim=c(0,450),ylim=c(0,60))
lines(train$price, col="blue")
lines(predict(nls.mod,data=train), col="red") 
lines(exp(linear.mod3$fitted.values), col="green")

(sqrt(mean(predict(nls.mod,newdata=test)-test$price)^2))
(sqrt(mean(exp(predict(linear.mod3,newdata=test))-test$price)^2))

#nls performed much better than the linear fit.

# iv) svr: support vector regression
library("e1071")

svm.fit <- svm(price~crime_rate+room_num+dist1+teachers+poor_prop+parks, data=train, kernel="radial")
# radial assumes to be the best fit because we saw some nonlinearity in the houseprice.
# ideally, we can verify all three to figure out which one is the best. But, to save time, continue with radial

                   
tune.svm.fit <- tune.svm(price~crime_rate+room_num+dist1+teachers+poor_prop+parks, data=train, kernel="radial",
                         gamma=seq(0,1,by=0.01), cost=seq(0.1,5,by=length(seq(0,1,by=0.01))))
tune.svm.fit

svm.fit <- svm(price~crime_rate+room_num+dist1+teachers+poor_prop+parks, data=train, kernel="radial",
               gamma=0.09, cost=0.1)

plot(x="",y="",xlim=c(0,450),ylim=c(0,60))
lines(train$price, col="blue")
lines(predict(svm.fit,data=house), col="red") 
legend(20,12, legend=c("observed","svr"), col=c("blue","red"), lty=1)


sqrt(mean((predict(svm.fit,newdata=test)-test$price)^2))

#SVR performed worse than NLS.

# PCA regression


pca <- prcomp(train[,colnames(train)[colnames(train)%in% names(linear.mod3$coefficients)[-c(1,7)]]],scale=T,center=T)

plot(cumsum(pca$sdev/sum(pca$sdev)*100))

#Based on the pca, ideal component numbers are 4 or 5.

# with 4 components
library("pls")
pca.reg <- pcr(price~ crime_rate + room_num + dist1 + teachers + poor_prop + parks, data=train, 
    scale =T, validation="CV")
summary(pca.reg)
validationplot(pca.reg)
# idea components are either 3 or 4.

sqrt(mean(predict(pca.reg,newdata=test,ncomp = 3) - test$price)^2)
sqrt(mean(predict(pca.reg,newdata=test,ncomp = 4) - test$price)^2)

# pca regression with components =3 performed the best, however it is not as good as nls regression).



#### Can our models be improved  if we can separate the house by levels of owners' economic status?

# Parameters of interest is now price/resid_area, which, in the real world, can be interpreted
# as price per square feet. In theory, this measure may reflect latent variables that can affect the price.
# For example, for a house size of 2,000 sqft. The one in Manhattan should many folds more than the one in
# the upper state New York despite the fact that they are in the same state.

house$per_resid <- house$price / house$resid_area
hist(house$per_resid) 

# The quick histogram already reflects the typical distribution of houses, skewness with longer tails on one side.

#EM algorithm

pi = 0.5
mu1 = mean(house$per_resid) -1 
mu2 = mean(house$per_resid) +1
sigma1 = sd(house$per_resid)^2
sigma2 = sd(house$per_resid)^2

diff = 1

while(diff >0.001){
  
  #estimation 
  d1 <- pi * dnorm(house$per_resid, mean=mu1,sd=sqrt(sigma1))
  d2 <- (1-pi)*dnorm(house$per_resid, mean=mu2, sd=sqrt(sigma2))
  d11 <- d1 / (d1+d2)
  d22 <- d2/ (d1+d2)
  
  pi.2 <- pi
  mu1.2 <- mu1
  mu2.2 <- mu2
  sigma1.2 <- sigma1
  sigma2.2 <- sigma2
  
  #maximization
  pi <- mean(d11)
  mu1 <- sum(house$per_resid * d11) / sum(d11)
  mu2 <- sum(house$per_resid *d22) / sum(d22)
  sigma1 = sum((house$per_resid-mu1)^2*d11) / sum(d11)
  sigma2 = sum((house$per_resid-mu2)^2*d22) / sum(d22)
  
  parameter.2 <- c(pi.2,mu1.2,mu2.2,sigma1.2,sigma2.2)
  parameter <- c(pi,mu1,mu2,sigma1,sigma2)
  
  diff <- sum((parameter.2 - parameter)^2)
  
}
parameter


# Means of two groups are 0.41068785 and 0.82471150

dist <- data.frame(dist1=(house$per_resid - parameter[2])^2, dist2=(house$per_resid - parameter[3])^2)

house$cluster <- ifelse(dist$dist1 <dist$dist2, "1", "2")

plot(house$price, col=house$cluster)

# With EM algorithm, the data is clustered pretty well.

# Comparison with kmeans

k.means <- kmeans(house$per_resid, centers=2)
k.means$cluster <- ifelse(k.means$cluster==2,k.means$cluster-1,k.means$cluster+1) #modifying the cluster order for visualization

plot(house$price, col=k.means$cluster)

par(mfrow=c(2,1))
plot(house$price, col=house$cluster, main="EM Algorithm")
plot(house$price, col=k.means$cluster, main="K-Means")

# The biggest difference is the way each algorithm handled observations near the boundary of the two groups.
# Probability based vs distance based

table(house$cluster)
table(k.means$cluster)
