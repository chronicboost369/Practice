library(tidyverse)


n = 5000
y1 <- rnorm(n, mean = 4, sd = 2)
y2 <- rnorm(n, mean = 14, sd = 3)
z <- rbinom(n, 1, prob = 0.7)
y <- z * y1 + (1 - z) * y2
hist(y)

# Based on the histogram, one can expect that there are two major groups because of the two peaks.


# Parameters to estimate
# 1. omega, the probability of data from which group?
# 2. mu and sigma of the two groups

# Initial values
mu1 <- mean(y) -1
mu2<- mean(y) + 1
omega <- 0.5
sigma1 <- sd(y)^2
sigma2 <- sd(y)^2

diff = 1
while(diff > 0.001){
  
  # estimation
  d1 <- omega * dnorm(y,mean = mu1, sd= sqrt(sigma1)) # density based on distribution a
  d2 <- (1-omega) * dnorm(y,mean = mu2, sd= sqrt(sigma2)) # density based on distribution b
  d1a <- d1 / (d1 + d2) # proportion of d1 
  d2a <- d2 / (d1 + d2) # proprotion of d2
  
  omega.v2 = omega
  mu1.v2 = mu1
  mu2.v2 = mu2
  sigma1.v2 = sigma1
  sigma2.v2 = sigma2
  
  # maximization
  omega = mean(d1a)
  mu1 = sum(y*d1a)/sum(d1a)
  mu2 = sum(y*d2a) / sum(d2a)
  sigma1 = sum(d1a*(y-mu1)^2) / sum(d1a)
  sigma2 = sum(d2a*(y-mu2)^2) / sum(d2a)
  
  parameters.v2 <- c(omega.v2, mu1.v2, mu2.v2, sigma1.v2, sigma2.v2)
  parameters <- c(omega, mu1, mu2, sigma1, sigma2)
  diff <- sum((parameters.v2- parameters)^2)
}
parameters

# With GMM based on EM algo, the parameters of the data, y, are mu1 = 4, mu2= 14, sd1=2, sd=3, and omega = 0.7
# The group 1 with mu=4 and sd=2 represents the 70% of the data.
# The group 2 with mu=14, and sd=3 represents the 30% of the data.

# Can clustering can identify the same result?
# K-means should be a good start. Hclust is not receommended because there are too many observations that
# dendrogram is not interpretable due to its size.

prop.exp <- matrix(c(1:10,rep(NA,10)),ncol=2)
for( i in 1:10){
  k.cluster <- kmeans(y,centers= i)
  prop.exp[i,2] <- k.cluster$betweenss / k.cluster$totss
}


plot(x=prop.exp[,1], y=prop.exp[,2], type="b", main="K-means Elbow Chart")

# Based on the kmeans elbow chart, cluster of 2 or 3 is the best as increase in the 
# proportion of explanation by clusters decrease after the cluster number of 2.
# Here, we decided to move on with the cluster # of 2.

# Testing parameters 
k.res <- kmeans(y,2)
table(k.res$cluster) / length(y) * 100 # omega is almost 70

y2 <- data.frame(y=y,cluster=k.res$cluster)
y2 %>% group_by(cluster) %>% summarise(mean=mean(y), sd= sd(y), omega= length(cluster)/nrow(y2) *100 )

# K-means did somewhat acceptable job of distinguishing two distributions. However, its performance 
# is not as good as EM algorithm.

# Does this change, if the dataset is scaled before applying k-means?
# Why scale? Scale is often used to limit the influence of numbers that are relatively bigger in magnitude.
prop.exp2 <- matrix(c(1:10,rep(NA,10)),ncol=2)
for( i in 1:10){
  k.cluster <- kmeans(scale(y),centers= i)
  prop.exp2[i,2] <- k.cluster$betweenss / k.cluster$totss
}


plot(x=prop.exp2[,1], y=prop.exp2[,2], type="b", main="K-means Elbow Chart")

# Still, cluster of 2 or 3 is the best. Proceed with 2

# Testing parameters 
k.res2 <- kmeans(scale(y),2)
table(k.res2$cluster) / length(y) * 100 # omega is almost 70

y3 <- data.frame(y=y,cluster=k.res2$cluster)
y3 %>% group_by(cluster) %>% summarise(mean=mean(y), sd= sd(y), omega= length(cluster)/nrow(y2) *100 )

# Scaling didn't affect the performance.
