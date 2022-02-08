library("tidyverse")
## KDE practice


data <- iris[,c(1:2)]


# boostrapping data to increase the sample size
data <- data.frame( se = sample(data$Sepal.Length,5000,replace=T) , sw = sample(data$Sepal.Width,5000,replace=T))

#random points is generated

randompt <- seq(mean(data$se)-2, mean(data$se)+2, by=0.0001)
randomptb <- seq(mean(data$sw)-2, mean(data$sw)+2, by=0.0001)
bwidth <- 1.06 * sd(data$se) * nrow(data)^-0.2
bwidthb <- 1.06 * sd(data$sw) * nrow(data)^-0.2
kernel.d <- NULL




#fitting the density
for ( i in 1:length(randompt)){
  term1 <- 1/(sqrt(2*pi))
  term2 <- exp(-0.5 * ( ((randompt[i]-data$se)/bwidth)^2))
  combine <- sum(term1*term2)
  kernel.d[i] <- combine * (1/(nrow(data)*bwidth))
              
}


den.df <- data.frame(random = randompt, den = kernel.d)

ggplot()+geom_histogram(data=data,aes(x=se,y=..density..), binwidth = 3.49 * sd(data$se) * (1/(nrow(data)^0.3333333333)))+
  geom_line(data=den.df, aes(x=random,y=den))


#isntead of randompoints, let's try on the dataset itself
data.k.den <- NULL
for ( i in 1:nrow(data)){
  term1 <- 1/(sqrt(2*pi))
  term2 <- exp(-0.5 * ( ((data$se[i]-data$se)/bwidth)^2))
  combine <- sum(term1*term2)
  data.k.den[i] <- combine * (1/(nrow(data)*bwidth))
  
}

den.df <- data.frame(points = data$se, den = data.k.den)
ggplot()+geom_histogram(data=data,aes(x=se,y=..density..), binwidth = 3.49 * sd(data$se) * (1/(nrow(data)^0.3333333333)))+
  geom_line(data=den.df, aes(x=points,y=den))




#multiplicative kernel
m.kernel.d <- NULL
for ( i in 1:length(randompt)){
  term1 <- 1/(sqrt(2*pi))
  term2.a <- exp(-0.5 * ( ((randompt[i]-data$se)/bwidth)^2))
  term2.b <- exp(-0.5 * ( ((randomptb[i]-data$sw)/bwidthb)^2))
  combine <- sum((term1*term2.a) * (term1*term2.b))
  m.kernel.d[i] <- combine * (1/(nrow(data)*bwidth))
  
}
m.den.df <- data.frame(random = randompt, den = m.kernel.d, random2 = randomptb)
ggplot()+geom_histogram(data=data,aes(x=se,y=..density..), binwidth = 3.49 * sd(data$se) * (1/(nrow(data)^0.3333333333)))+
  geom_line(data=m.den.df, aes(x=random2,y=den))

plot(m.den.df$random2,m.den.df$den)


kmeans.mden <- kmeans(m.den.df$den, centers=4)

m.den.df$cluster <- kmeans.mden$cluster
plot(m.den.df$random2,m.den.df$den, col=m.den.df$cluster)

plot(data$se,data$sw, col=m.den.df$cluster)

# multiplicative kde on the dataset
data <-iris[,c(1,2)]
## sepal length and sepal width
m.kernel.d.act <- NULL
for ( i in 1:nrow(data)){
  term1 <- 1/(sqrt(2*pi))
  term2.a <- exp(-0.5 * ( ((data$Sepal.Length[i]-data$Sepal.Length)/bwidth)^2))
  term2.b <- exp(-0.5 * ( ((data$Sepal.Width[i]-data$Sepal.Width)/bwidthb)^2))
  combine <- sum((term1*term2.a) * (term1*term2.b))
  m.kernel.d.act[i] <- combine * (1/(nrow(data)*bwidth))
  
}
m.kernel.act <- data.frame(se = data$Sepal.Length, sw = data$Sepal.Width, den = m.kernel.d.act)

ggplot()+geom_histogram(data=data,aes(x=Sepal.Length,y=..density..), binwidth = 3.49 * sd(data$Sepal.Length) * (1/(nrow(data)^0.3333333333)))+
  geom_line(data=m.kernel.act, aes(x=se,y=den))

m.kernel.act$cluster <- kmeans(m.kernel.act$den, 3)$cluster

plot(data$Sepal.Length,data$Sepal.Width,col=m.kernel.act$cluster)

data$cluster <- kmeans(data,3)$cluster
plot(data$Sepal.Length,data$Sepal.Width,col=data$cluster)

colnames(m.kernel.act)

# Result dosn't look good
m.kernel.act%>%group_by(cluster)%>%summarize(mean.se = mean(se), mean.sw = mean(sw))
iris%>% group_by(Species)%>%summarize(mean.se = mean(Sepal.Length), mean.sw = mean(Sepal.Width))

## Sepal Length and Petal Lenth
bwidth.pl <- 1.06 * sd(iris$Petal.Length) * nrow(data)^-0.2
m.kernel.d.act <- NULL
for ( i in 1:nrow(data)){
  term1 <- 1/(sqrt(2*pi))
  term2.a <- exp(-0.5 * ( ((data$Sepal.Length[i]-data$Sepal.Length)/bwidth)^2))
  term2.b <- exp(-0.5 * ( ((iris$Petal.Length[i]-iris$Petal.Length)/bwidth.pl)^2))
  combine <- sum((term1*term2.a) * (term1*term2.b))
  m.kernel.d.act[i] <- combine * (1/(nrow(data)*bwidth*bwidth.pl))
  
}
dim(m.kernel.act)
m.kernel.act <- data.frame(se = data$Sepal.Length, pl = iris$Petal.Length, den = m.kernel.d.act)

ggplot()+geom_histogram(data=data,aes(x=Sepal.Length,y=..density..), binwidth = 3.49 * sd(data$Sepal.Length) * (1/(nrow(data)^0.3333333333)))+
  geom_line(data=m.kernel.act, aes(x=se,y=den))

m.kernel.act$cluster <- kmeans(m.kernel.act$den, 3)$cluster

plot(data$Sepal.Length,data$Sepal.Width,col=m.kernel.act$cluster)

data$cluster <- kmeans(data,3)$cluster
plot(data$Sepal.Length,data$Sepal.Width,col=data$cluster)

colnames(m.kernel.act)

# Result dosn't look good
m.kernel.act%>%group_by(cluster)%>%summarize(mean.se = mean(se), mean.pl = mean(pl))
iris%>% group_by(Species)%>%summarize(mean.se = mean(Sepal.Length), mean.pl = mean(Petal.Length))
