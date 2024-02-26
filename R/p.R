ralab1 = function(){


  code = cat('data = read.csv("C:\\Users\\HP\\Desktop\\VIT\\SEM-2\\Rregression_Analysis_Lab\\Health_data.csv")
data
total_col = ncol(data)
total_col
total_row = nrow(data)
total_row
summary_ = summary(data)
summary_

library(tidyverse)                 ## For pre-processing data-set

## Handling missing values

age = data$AGE
boxplot(age)
is.na(age)
data2 = data %>% mutate(AGE=replace(AGE,is.na(AGE),median(AGE,na.rm = TRUE)))
data2

height = data2$HEIGHT
boxplot(height)
data3 = data2 %>% mutate(HEIGHT=replace(HEIGHT,is.na(HEIGHT),mean(HEIGHT,na.rm = TRUE)))
data3

weight = data3$WEIGHT
boxplot(weight)
data4 = data3 %>% mutate(WEIGHT=replace(WEIGHT,is.na(WEIGHT),median(WEIGHT,na.rm = TRUE)))
data4

bmi = data4$BMI
boxplot(bmi)
data5 = data4 %>% mutate(BMI=replace(BMI,is.na(BMI),mean(BMI,na.rm = TRUE)))
data5

bmr = data5$BMR
boxplot(bmr)
data6 = data5 %>% mutate(BMR=replace(BMR,is.na(BMR),median(BMR,na.rm = TRUE)))
data6

db = data6
db

## Segregating male and female data

male_data = subset(db,GENDER == "M")
male_data

female_data = subset(db,GENDER == "F")
female_data

## Contengency table for GENDER and excercise

contengency_tabel = table(db$GENDER,db$Exercise)
chisq.test(contengency_tabel)

## X-squared = 1.28, df = 1, p-value = 0.2579
## Since p-value is greater than 0.05 we fail to reject null hypothesis.
## Therefore the categorical variables "GENDER" and "Excercise" are indepedent of each other.

## Calculating correlation between variable
correlation = cor(db$HEIGHT,db$WEIGHT,method = "pearson")
correlation

scatter.smooth(db$HEIGHT,db$WEIGHT)
plot(db$HEIGHT,db$WEIGHT)
abline(lm(db$WEIGHT~db$HEIGHT))
summary(lm(db$WEIGHT~db$HEIGHT))

mlm = lm(db$BMI~db$HEIGHT+db$WEIGHT)
mlm
summary(mlm)')
  return(cat(code))

}


ralab2 = function(){
  code = cat("## Experiment-1
# Correlation Analysis using scatter diagram, Karl Pearson's correlation coefficient and drawing inferrence for given data-set.

data1 = iris
library(tidyverse)
ggplot(data1,aes(x = data1$Sepal.Width,y = data1$Sepal.Length)) +
  facet_wrap(~Species,scales='free_x') +
  geom_point() +
  geom_smooth(formula=y~x,method='lm',se=FALSE)+
  labs(x = 'Sepal Width' , y = 'Sepal Length' ,
       title = 'Sepal Length vs. Sepal Width in iris',
       subtitle = 'Grouped by Species')

data2 =mtcars
cor1 = cor.test(data2$wt,data2$mpg,method='pearson')
cor1

data3 = data2[,c(1,3,4,5)]
data3
cor2 = round(cor(data3,method='pearson'),4)
cor2
library(corrplot)
cor3 = corrplot(cor2,type='lower',order='hclust',tl.col='blue',tl.srt=45)
cor3

library(PerformanceAnalytics)
data4 = data2[,c(1,3,4,5)]
cor4 = chart.Correlation(data4,histogram=TRUE,method='pearson')
cor4

library(ggcorrplot)
ggcorrplot(cor6)
ggcorrplot(cor6,hc.order=TRUE,type='lower',lab=TRUE)
cor5 = cor(data4)
cor5
symnum(cor5,abbr.colnames = F)

cor6 = round(cor(data4),4)
cor6

## Excercise

data5 = trees
data5

ggplot(data5,aes(x = data5$Girth,y = data5$Height)) +
  geom_point() +
  geom_smooth(formula=y~x,method='lm',se=FALSE)+
  labs(x = 'Girth' , y = 'Height' ,
       title = 'Girth vs Height in trees dataset')

# Interpretation: There is positive correlation between 'Girth' and 'Height' i.e with increase in 'Girth' there is increase in 'Height'

data6 = data5[,c(2,3)]
cor7 = chart.Correlation(data6,histogram=TRUE,method='pearson')
cor7

# Interpretation: The pearson's correlation coefficient between 'Height' and 'Volume' is 0.60 i.e intermediate positive correlation.

data7 = head(data5,5)
cor8 = round(cor(data7,method='pearson'),4)
cor8

cor9 = corrplot(cor8,type='lower',order='hclust',tl.col='blue',tl.srt=45)
cor9

cor10 = round(cor(data7),4)
cor10
ggcorrplot(cor10)
ggcorrplot(cor10,hc.order=TRUE,type='lower',lab=TRUE)")
  return(cat(code))

}



ralab3 = function(){

  code = cat("#Experiment-2;
#Simple linear regression model fitting, estimation of parameters, computing R^2 and adjusted R^2 and model interpretation

cars
summary(cars)

speed=cars$speed
speed_bar=mean(speed)
speed_bar #xbar

distance=cars$dist
dist_bar=mean(distance)
dist_bar#ybar

#b1=cov(xy)/var(x)
b1=sum((speed-speed_bar)*(distance-dist_bar))/sum((speed-speed_bar)**2)
#b0=ybar-b1*xbar
bo=dist_bar-(b1*speed_bar)

# Scatterplot, Estimation of Regression Coefficients using inbuilt function & Adding the line of best fit
plot(speed,distance,ylab='Car Stopping Distance (ft)',xlab='Speed(mph)',
     main='Car Speed vs. Stopping Distance',col='blue')

SLR = lm(distance ~ speed)
SLR
abline(SLR,col='red')

#Error analysis
#We assume the errors are normally distributed and are independent with constant variance
residual=SLR$residuals
hist(residual)

#The above histogram is bell shaped and skewed
#Homoscedasticity - variance is constant
#We plot the residuals against the independent variable
plot(residual~speed)
abline(0,0)

#For slower speed, there is very little variability, while for higher speeds, there is more variability

summary(SLR)
#High R squared value represents smaller differences between observed and fitted values
#note: It is not he only measure to test the goodness of fit for the model

#Hypothesis testing
#H0: beta1=0 vs H1: beta1!=0

anova(SLR)

#Using table values:
Ft=qf(0.95,df1=1,df2=48)
Ft
#Calculated F value is 89.567
#Hence, we reject the null hypothesis

#P-value approach
pv=1-pf(89.567,1,48)
pv
#as pv<0.05, we reject Ho

#Confidence Interval
confint(SLR,level=0.95)

#Prediction and Confidence interval for the stopping distance when the speed is 15 mph
newdata=data.frame(speed=15)
newdata
conf=predict(SLR,newdata,interval='confidence')
conf
pred=predict(SLR,newdata,interval='predict')
pred


#Exercise
pressure
summary(pressure)

x=pressure$temperature
y=pressure$pressure
xbar=mean(x)
ybar=mean(y)
#b1=cov(xy)/var(x)
b1=sum((x-xbar)*(y-ybar))/sum((x-xbar)**2)
#b0=ybar-b1*xbar
bo=ybar-(b1*xbar)

plot(x,y,main='Temperature vs Pressure')
SLR = lm(y ~ x)
SLR
abline(SLR,col='red')

residual=SLR$residuals
hist(residual)

plot(residual~x)
abline(0,0)

#Modification of the model'
data=pressure
pressure1=data
pressure1$temperature2=pressure1$temperature^2
pressure1$temperature3=pressure1$temperature^3
pressure1$temperature4=pressure1$temperature^4
poly_reg=lm(pressure~.,data=pressure1)

#Visualizing
library(ggplot2)
ggplot()+ geom_point(aes(x=pressure1$temperature,y=pressure1$pressure),color='red')+
  geom_line(aes(x=pressure1$temperature,y=predict(poly_reg,newdata = pressure1)),col='blue')+ggtitle('Polynomial Regression Model')+xlab('Temperature')+ylab('Pressure')
summary(poly_reg)
")
  return(cat(code))

}




silab1 = function(){

  code = cat('dataset = iris
head(dataset)

ss_setosa = subset(dataset,Species=="setosa")
ss_versicolor = subset(dataset,Species=="versicolor")
ss_virginica = subset(dataset,Species=="virginica")

plot(density(ss_setosa$Sepal.Length),col="red",xlab = "setosa sepal length",main = "Density plot") ## Almost Normally distributed
plot(density(ss_versicolor$Sepal.Length),col="red",xlab = "versicolor sepal length",main = "Density plot")
plot(density(ss_virginica$Sepal.Length),col="red",xlab = "virginica sepal length",main = "Density plot")

plot(density(ss_setosa$Sepal.Width),col="green",xlab = "setosa sepal width",main = "Density plot")
plot(density(ss_versicolor$Sepal.Width),col="green",xlab = "versicolor sepal width",main = "Density plot")
plot(density(ss_virginica$Sepal.Width),col="green",xlab = "virginica sepal width",main = "Density plot")

plot(density(ss_setosa$Petal.Length),col="blue",xlab = "setosa petal length",main = "Density plot")
plot(density(ss_versicolor$Petal.Length),col="blue",xlab = "versicolor petal length",main = "Density plot")
plot(density(ss_virginica$Petal.Length),col="blue",xlab = "virginica petal length",main = "Density plot")

plot(density(ss_setosa$Petal.Width),col="orange",xlab = "setosa petal width",main = "Density plot")
plot(density(ss_versicolor$Petal.Width),col="orange",xlab = "versicolor petal width",main = "Density plot")
plot(density(ss_virginica$Petal.Width),col="orange",xlab = "virginica petal width",main = "Density plot")

population = ss_setosa$Sepal.Length
mu = mean(population)
sigma =sd(population)

library(ggplot2)

mean_and_sd = function(data){

  result = c(mean(data),sd(data))
  names(result) = c("mean","sd")
  result

}

samp = as.data.frame(t(replicate(50, (mean_and_sd(sample(population, size = 5))))))
samp$lower = samp$mean - 1.96 * samp$sd/sqrt(5)
samp$upper = samp$mean + 1.96 * samp$sd/sqrt(5)

plot.ci = function(dat, mu){
  dat$index = 1:length(dat[,1])
  dat = transform(dat, containMU = upper > mu & lower < mu)
  ggplot(dat, aes(x = lower, y = index, xend = upper, yend = index, color = containMU)) +
    geom_segment(arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
    xlab("mean") +
    geom_vline(xintercept = mu)
}
plot.ci(samp, mu = mu)')

  return(cat(code))
}


silab2 = function(){


  code = cat("## Confidence Interval for a Proportion
# Confidence Interval = p +/- z*(√p(1-p) / n)
# where: p: sample proportion
# z: the chosen z-value
#n: sample size

'A random sample of 500 apples was taken from, a large consignment and 60
were found to be bad. Obtain the 95%, 98% confidence limits for the percentage
number of bad apple in the consignment'

library(glue)
n<-500;
p<-(60/500)
SE <- sqrt(p*(1-p)/n)
SE
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')

n<-500;
p<-(60/500)
SE <- sqrt(p*(1-p)/n)
SE
z_star<- qnorm(1-(1 - 0.98)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')


'A sample of 900 members has a mean 3·4 cms, and s.d. 2·61 cms. If the
population is normal and its mean is unknown, find the 95% and 98% fiucial
limits of true mean.'

# Xbar +/- z * sigma/sqrt(n)

n<-900;
sigma<-2.61
Xbar<-3.4
SE <- sigma/sqrt(n)
SE
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
ME<-z_star*SE
ME
glue('({Xbar - ME}, { Xbar + ME})')

n<-900;
sigma<-2.61
Xbar<-3.4
SE <- sigma/sqrt(n)
SE
z_star<- qnorm(1-(1 - 0.98)/2)
z_star
ME<-z_star*SE
ME
glue('({Xbar - ME}, { Xbar + ME})')


'solve
The mean muscular endurance score of a random sample of 60 subjects was found to be 145 with a s.d.
of 40. Construct a 95% confidence interval for the true mean. Assume the sample size to be large
enough for normal approximation.'

n<-60;
Xbar<-145
sigma = 40
SE <- sigma/sqrt(n)
SE
z_star<- qnorm(1-(1 - 0.98)/2)
z_star
ME<-z_star*SE
ME
glue('({Xbar - ME}, { Xbar + ME})')


#Confidence Interval for a Difference in Proportions
#Confidence interval = (p1–p2) +/- z*√(p1(1-p1)/n1 + p2(1-p2)/n2)

#p1, p2: sample 1 proportion, sample 2 proportion
#z: the z-critical value based on the confidence level
#n1, n2: sample 1 size, sample 2 size
#Write R code for the above formula and solve the following problem

#Write R code for the above formula and solve the following problem

'A medical researcher conjectures that smoking can result in the wrinkled skin around the eyes. The
researcher recruited 150 smokers and 250 nonsmokers to take part in an observational study and found
that 95 of the smokers and 105 of the nonsmokers were seen to have prominent wrinkles around the eyes
(based on a standardized wrinkle score administered by a person who did not know if the subject smoked
 or not). Find CI for the true difference that would exist between these two groups in the population.'

'Let
p1: proportion for smokers
p2: proportion for non-smokers
'
p1 = 95/150
p2 = 105/250
n1 = 150
n2 = 250
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
SE = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
ME<-z_star*SE
ME
glue('({(p1-p2) - ME}, {(p1-p2) + ME})')


'In a certain factory there are two independent processes manufacturing the same item. The average weight
in a sample of 250 items produced from one process is found to be 120 ozs. with a standard deviation of
12 ozs. while the corresponding figures in a sample of 400 items from ihe other process are 124 and 14.
Obtain the standard error of difference between the two. sample means. Find the 99% confidence limits
for the difference in the average weights of items produced by the two processes respectively.'

n1 = 250
n2 = 400
Xbar1 = 120
Xbar2 = 124
sigma1 = 12
sigma2 = 14
SE <- sqrt((((sigma1)^2)/n1)+(((sigma2)^2)/n2))
SE
z_star<- qnorm(1-(1 - 0.99)/2)
z_star
ME<-z_star*SE
ME
glue('({(Xbar2-Xbar1) - ME}, {(Xbar2-Xbar1) + ME})')

# P-value from z score

# Right-tailed test
# Suppose we want to find the p-value associated with a z-score of 2.02 in a right-tailed hypothesis test.

#P = P(Z > 2.02) = 0.0217
pnorm(q=2.02, lower.tail=FALSE)

# Left-tailed test
# Suppose we want to find the p-value associated with a z-score of -0.77 in a left-tailed hypothesis test.
pnorm(q=-0.77, lower.tail=TRUE)

#Two-tailed test
#Suppose we want to find the p-value associated with a z-score of 2.83 in a two-tailed hypothesis test.
#P = P(|Z| > 2.83) = 2P(Z < −2.83) = 0.0046
2*pnorm(q=2.83, lower.tail=FALSE)

# T test (two tiled)
2*pt(q=2.06, 14,lower.tail=FALSE)

# here 14 is degrees of freedom
#Similarly you can do for one tailed test (t)")
  return(cat(code))

}



silab3 = function(){


  code = cat("## Confidence interval for sample size less than 30.
## Using t-distribution
## {x_bar +/- t(n-1),(1-alpha/2)*[s/(root(n))]}

'Ques 1: A random sample of 10 boys had the following IQ: 70,120,110,101,88,83,95,98,107,100
         Find a reasonable range in which the most of the mean IQ values of samples of 10 boys lies(95%)'

library(glue)
x = c(70,120,110,101,88,83,95,98,107,100)
n = length(x)
x_bar = sum(x)/n
mean_diff = (x - x_bar)^2
x_bar
s = sqrt((sum(mean_diff))/(n-1))
s
CI = t.test(x,conf.level = 0.95)
CI
t_star<- qt(1-(1 - 0.95)/2,9)
t_star
t = 21.535
ME = t_star*(s/sqrt(n))
ME
glue('({x_bar - ME}, { x_bar + ME})')



'Ques2: A random sample of 16 values from a normal population showed a mean of 41.5 and a sum of
        squared deviation is 135.  Find the confidence interval for 95% & 99%'

n <- 16
dev <- 135
sigma <- sqrt(dev/(n-1))
sigma
xbar <- 41.5
SE <- sigma/sqrt(n)
#95%
t_value <- qt(1-(1-0.95)/2,n-1)
ME <- t_value*SE
glue('({xbar-ME},{xbar+ME})')
#99%
t_value <- qt(1-(1-0.99)/2,n-1)
ME <- t_value*SE
glue('({xbar-ME},{xbar+ME})')

'Ques 3: The fedding habits of two specices of net-casting spoders are studies . The species ,
        the deinopis and menneus, coexist in easter Australia. The following data were obtained on
        the size, in millimiters, of the prey of random samples of stwo species:

             Size of random pray samples of Dienopis Spider in mm
             12.9,10.2,7.4,7.0,10.5,11.9,7.1,9.9,14.4,11.3

             size of random pray samples of meeneus spider in mm
             10.2,6.9,10.9,11.0,10.1,5.3,7.5,10.3,9.2,8.8'

x = c(12.9,10.2,7.4,7.0,10.5,11.9,7.1,9.9,14.4,11.3)
y = c(10.2,6.9,10.9,11.0,10.1,5.3,7.5,10.3,9.2,8.8)

t.test(x,y,conf.level = 0.95,paired=FALSE)

## Exercise

## Ques:1
pnorm(q=2.23, lower.tail=FALSE)

## Ques:2
pnorm(q=-0.795, lower.tail=TRUE)

## Ques:3
2*pnorm(q=2.92, lower.tail=FALSE)

## Ques:4
2*pt(q=2.06, 25,lower.tail=FALSE)

## Ques:5
n = 81
Xbar= 74.6
sigma = 11.3
SE <- sigma/sqrt(n)
SE
z_star<- qnorm(1-(1 - 0.90)/2)
z_star
ME<-z_star*SE
ME
glue('({Xbar - ME}, { Xbar + ME})')

## Ques:6
#a)
n = 20
sigma <- 15.4
xbar <- 330.2
SE <- sigma/sqrt(n)
t_value <- qt(1-(1-0.95)/2,n-1)
ME <- t_value*SE
glue('({xbar-ME},{xbar+ME})')

#b)
t_value <- qt(1-(1-0.99)/2,n-1)
ME <- t_value*SE
glue('({xbar-ME},{xbar+ME})')

## Ques:7
n<-400;
p<-(13/400)
SE <- sqrt(p*(1-p)/n)
SE
# 90%
z_star<- qnorm(1-(1 - 0.90)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')

# 95%
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')

## Ques:8
x1 = c(3250,3268,4302,3184,3266,3297,3332,3502,3064,3116)
x2 = c(3094,3106,3004,3066,2984,3124,3316,3212,3380,3018)
n1 = length(x1)
n2 = length(x2)
xbar1 = sum(x1)/n1
xbar2 = sum(x2)/n2
sigma1 = sqrt((sum((x1 - xbar1)^2))/(n1-1))
sigma2 = sqrt((sum((x2 - xbar2)^2))/(n2-1))
SE <- sqrt((((sigma1)^2)/n1)+(((sigma2)^2)/n2))
SE
z_star<- qnorm(1-(1 - 0.99)/2)
z_star
ME<-z_star*SE
ME
glue('({(xbar2-xbar1) - ME}, {(xbar2-xbar1) + ME})')

## Ques:9
p1 = 12/50
p2 = 12/60
n1 = 50
n2 = 60
z_star<- qnorm(1-(1 - 0.98)/2)
z_star
SE = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
ME<-z_star*SE
ME
glue('({(p1-p2) - ME}, {(p1-p2) + ME})')")
  return(cat(code))

}


silab4 = function(){


  code = cat("#Use the simulation to show that sample mean is unbiased estimator for the population mean mew, when X1,....,X10 is a random sample from an exponential random variable with rate 2

sample = rexp(10,rate = 2)
sample
mean(sample)

simu_data = replicate(1000,{
  sample = rexp(10,rate = 2)
  mean(sample)
})

mean(simu_data)
plot(density(simu_data),main = 'SAMPLING DISTRIBUTION OF SAMPLE MEAN',col = 'red')


sample = rpois(10, 4)
sample
mean(sample)

simu_data = replicate(2000,{
  sample = rexp(10,rate = 4)
  mean(sample)
})

mean(simu_data)
plot(density(simu_data),main = 'SAMPLING DISTRIBUTION OF SAMPLE MEAN',col = 'red')

")
  return(cat(code))

}


silab5 = function(){


  code = cat("library(maxLik)
set.seed(123)
x=rnorm(100,mean=1,sd=2)
logLikfun=function(param){
  mu=(param[1])
  sigma=(param[2])
  sum(dnorm(x,mean=mu,sd=sigma,log=T))
}
mle=maxLik(logLik=logLikfun,start=c(mu=0,sigma=1))
summary(mle)

set.seed(123)
x=rpois(1000,lambda = 2)
logLikfun=function(param){
  mu=(param[1])
  sigma=(param[2])
  sum(dpois(x,lambda =mu,log=T))
}
mle=maxLik(logLik=logLikfun,start=1)
summary(mle)

set.seed(123)
x=rexp(1000,rate=1)
logLikfun=function(param){
  mu=(param[1])
  sigma=(param[2])
  sum(dexp(x,rate=2,log=T))
}
mle=maxLik(logLik=logLikfun,start=1)
summary(mle)

## ques:1
x = c(rep(0,144),rep(1,91),rep(2,32),rep(3,11),rep(4,2),rep(5,0))
logLikfun=function(param){
  lam=param[1]
  sum(dexp(x,lam,log=T))
}
mle=maxLik(logLik=logLikfun,start=c(lam=1),method='NR')
summary(mle)

## ques:2
x = c(rpois(0,1),rpois(0,14),rpois(1,30),rpois(2,36),rpois(3,68),rpois(5,43),rpois(6,30),rpois(7,14),rpois(8,10),rpois(9,6),rpois(10,4),rpois(11,1),rpois(12,1),rpois(13,0))
logLikfun=function(param){
  lam=param[1]
  sum(dpois(x,lam,log=T))
}
mle=maxLik(logLik=logLikfun,start=c(lam=1),method='NR')
summary(mle)")
  return(cat(code))

}




sipracticelab1 = function(){


  code = cat("## Ques: 1
n<-500;
p<-(40/500)
SE <- sqrt(p*(1-p)/n)
SE
# 95%
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')
# 98%
z_star<- qnorm(1-(1 - 0.98)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')

## Ques: 2
n<-32;
sigma<-8.4
Xbar<-66.3
SE <- sigma/sqrt(n)
SE
# 90%
z_star<- qnorm(1-(1 - 0.90)/2)
z_star
ME<-z_star*SE
ME
glue('({Xbar - ME}, { Xbar + ME})')
# 99%
z_star<- qnorm(1-(1 - 0.99)/2)
z_star
ME<-z_star*SE
ME
glue('({Xbar - ME}, { Xbar + ME})')

## Ques: 3
x = c(17,13,18,19,17,21,29,22,16,28,21,15,26)
n = length(x)
x_bar = sum(x)/n
x_bar
mean_diff = (x - x_bar)^2
mean_diff
s = sqrt((sum(mean_diff))/(n-1))
s
CI = t.test(x,conf.level = 0.95)
CI
# 95%
t_star<- qt(1-(1 - 0.95)/2,n-1)
t_star
t = 21.535
ME = t_star*(s/sqrt(n))
ME
glue('({x_bar - ME}, { x_bar + ME})')
# 98%
t_star<- qt(1-(1 - 0.98)/2,n-1)
t_star
t = 21.535
ME = t_star*(s/sqrt(n))
ME
glue('({x_bar - ME}, { x_bar + ME})')
# 99%
t_star<- qt(1-(1 - 0.99)/2,n-1)
t_star
t = 21.535
ME = t_star*(s/sqrt(n))
ME
glue('({x_bar - ME}, { x_bar + ME})')

## Ques: 4

x1 = c(133.5,137.2,136.3,133.3,137.5,135.4,138.4,137.1,136.5,139.4,137.9,136.8)
x2 = c(134.0,134.7,136.0,132.7,134.6,135.2,135.9,135.8,134,135.6)
n1 = length(x1)
n2 = length(x2)
xbar1 = sum(x1)/n1
xbar2 = sum(x2)/n2
sigma1 = sqrt((sum((x1 - xbar1)^2))/(n1-1))
sigma2 = sqrt((sum((x2 - xbar2)^2))/(n2-1))
SE <- sqrt((((sigma1)^2)/n1)+(((sigma2)^2)/n2))
SE
# 95%
t_star<- qt(1-(1 - 0.95)/2,n1+n2-2)
t_star
ME<-t_star*SE
ME
glue('({(xbar1-xbar2) - ME}, {(xbar1-xbar2) + ME})')
# 98%
t_star<- qt(1-(1 - 0.98)/2,n1+n2-2)
t_star
ME<-t_star*SE
ME
glue('({(xbar1-xbar2) - ME}, {(xbar1-xbar2) + ME})')
# 99%
t_star<- qt(1-(1 - 0.99)/2,n1+n2-2)
t_star
ME<-t_star*SE
ME
glue('({(xbar1-xbar2) - ME}, {(xbar1-xbar2) + ME})')

## Ques: 5
pt(q=2.83, 14,lower.tail=FALSE)

## Ques: 6
pt(q=2.02, 14,lower.tail=FALSE)

## Ques: 7
n<-642
p<-(24/n)
SE <- sqrt(p*(1-p)/n)
SE

# a)
p

# b)

# 95%
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')
# 99%
z_star<- qnorm(1-(1 - 0.99)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')

## Ques: 8
2*pnorm(q=2.39, lower.tail=FALSE)

## Ques: 9
n1 = 400
n2 = 250
xbar1 = 124
xbar2 = 120
sigma1 = 14
sigma2 = 12
SE <- sqrt((((sigma1)^2)/n1)+(((sigma2)^2)/n2))
SE
# 95%
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
ME<-z_star*SE
ME
glue('({(xbar1-xbar2) - ME}, {(xbar1-xbar2) + ME})')
)

")
  return(cat(code))

}



sipracticelab2 = function(){


  code = cat("## Ques:1
pnorm(q=2.23, lower.tail=FALSE)

## Ques:2
pnorm(q=-0.795, lower.tail=TRUE)

## Ques:3
2*pnorm(q=2.92, lower.tail=FALSE)

## Ques:4
2*pt(q=2.06, 25,lower.tail=FALSE)

## Ques:5
n = 81
Xbar= 74.6
sigma = 11.3
SE <- sigma/sqrt(n)
SE
z_star<- qnorm(1-(1 - 0.90)/2)
z_star
ME<-z_star*SE
ME
glue('({Xbar - ME}, { Xbar + ME})')

## Ques:6
#a)
n = 20
sigma <- 15.4
xbar <- 330.2
SE <- sigma/sqrt(n)
t_value <- qt(1-(1-0.95)/2,n-1)
ME <- t_value*SE
glue('({xbar-ME},{xbar+ME})')

#b)
t_value <- qt(1-(1-0.99)/2,n-1)
ME <- t_value*SE
glue('({xbar-ME},{xbar+ME})')

## Ques:7
n<-400;
p<-(13/400)
SE <- sqrt(p*(1-p)/n)
SE
# 90%
z_star<- qnorm(1-(1 - 0.90)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')

# 95%
z_star<- qnorm(1-(1 - 0.95)/2)
z_star
ME<-z_star*SE
ME
glue('({p - ME}, {p + ME})')

## Ques:8
x1 = c(3250,3268,4302,3184,3266,3297,3332,3502,3064,3116)
x2 = c(3094,3106,3004,3066,2984,3124,3316,3212,3380,3018)
n1 = length(x1)
n2 = length(x2)
xbar1 = sum(x1)/n1
xbar2 = sum(x2)/n2
sigma1 = sqrt((sum((x1 - xbar1)^2))/(n1-1))
sigma2 = sqrt((sum((x2 - xbar2)^2))/(n2-1))
SE <- sqrt((((sigma1)^2)/n1)+(((sigma2)^2)/n2))
SE
z_star<- qnorm(1-(1 - 0.99)/2)
z_star
ME<-z_star*SE
ME
glue('({(Xbar2-Xbar1) - ME}, {(Xbar2-Xbar1) + ME})')

## Ques:9
p1 = 12/50
p2 = 12/60
n1 = 50
n2 = 60
z_star<- qnorm(1-(1 - 0.98)/2)
z_star
SE = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
ME<-z_star*SE
ME
glue('({(p1-p2) - ME}, {(p1-p2) + ME})')")
return(cat(code))

}




mllab1 = function(){


  code = cat("# Data Types
## Vectors

a = c(1,2,3,-7,6,-6,4,6,7)
a

b = c('Aman','Vedant','Raman','Bhuvan','Bharat','Roma','Nidhi')
b

c = c(TRUE,FALSE)
c

## Matrix

mat1 = matrix(10:30,nrow = 7,ncol = 3)
mat1

mat2= matrix(10:30,nrow = 7,ncol = 3,byrow = T)
mat2

nums = c(2,3,4,5,6,7,7,8,9)
rname = c('r1','r2','r3')
cname = c('c1','c2','c3')
mat3 = matrix(nums,nrow = 3,ncol = 3,byrow = T,dimnames = list(rname,cname))
mat3

mat3[2,]
mat3[,3]

## Data Frames

movie_id = c(1,2,3,4,5,6)
movie_category = c('Sci-Fi','Comedy','Sci-Fi','Haunted','Haunted','Sci-Fi')
movie_name = c('Intestellar','Deadpool','Robot','Anabelle','Conjuring','Matrix')
df = data.frame(movie_id,movie_category,movie_name)
df

df$movie_name

w = list(name='Aman',mynumbers=2,records = movie_id,age = 1,6)
w

## Factor

gender = c(rep('Male',10),rep('Female',10))
gender = factor(gender)
gender

## Importing Excel file
library(readxl)
dir()
data1 = read_excel('FSI-2023-DOWNLOAD.xlsx')
data1
excel_sheets('FSI-2023-DOWNLOAD.xlsx')         ## To get sheet details in excel
data2 = read_excel('FSI-2023-DOWNLOAD.xlsx',sheet='Sheet2')
data2

## Creating custom data in R and exporting it to system.
age = c(21,22,23,24)
name = c('A','V','N','R')
ID = c(68,62,28,73)
data2 = data.frame(age,name,ID)
data2

write.table(data2,'roughdata.txt',sep = '\t')
write.csv(data2,'roughtdata.csv')

dir()")
  return(cat(code))

}


mllab2 = function(){


  code = cat("## PCA

d = iris; d
X = d[,1:4]; X
X_scaled = scale(X,center = TRUE, scale = TRUE); X_scaled
cov_X = cor(X_scaled); cov_X
eigvl = eigen(cov_X)$values; eigvl
eigvc = eigen(cov_X)$vectors; eigvc
X_PCAs = X_scaled%*%eigvc; X_PCAs

var1 = eigvl[1]/sum(eigvl);var1
var2 = eigvl[2]/sum(eigvl);var2
var3 = eigvl[3]/sum(eigvl);var3
var4 = eigvl[4]/sum(eigvl);var4

reduced_X = X_scaled%*%eigvc[,1:2];reduced_X

# Built-in library

iris.pca = prcomp(iris[,c(1:4)],center = TRUE,scale = TRUE,rank = 2);iris.pca
iris.PCAs = iris.pca$x; iris.PCAs

## SVD

matA = cbind(c(3,4,5,6),c(7,5,3,7),c(0,7,4,2));matA
svdA = svd(matA);svdA
# verify
svdA$u %*% diag(svdA$d) %*% t(svdA$v)
A=cbind(c(2,1,3),c(1,3,2))
Asvd=svd(A)
Asvd
Asvd$u%*%diag(Asvd$d)%*%t(Asvd$v)

Au=Asvd$u
Bu=as.matrix(Au[,1]);Bu
Ad=diag(Asvd$d)
Bd=as.matrix(Ad[1,1]);Bd
Avt=t(Asvd$v)
Bvt=as.matrix(Avt[1,])
Bvt
B=Bu%*%Bd%*%t(Bvt)
B
f=sqrt(sum((A-B)^2))
f")
  return(cat(code))

}


mllab3 = function(){


  code = cat("## Multi-Dimension Scaling (MDS)

# Brute-Force code

P = rbind(c(0,93,82,133),c(93,0,52,60),c(82,52,0,111),c(133,60,111,0))
P2 = P*P ; P2
I = c(1,1,1,1) ; I
IIt = I%*%t(I) ; IIt
J = diag(4) - (1/4)*(IIt) ; J
B = (-1/2)*J%*%P2%*%J ; B
eigB = eigen(B) ; eigB
svalues = sqrt(eigB$values) ; svalues
E = eigB$vectors[,1:2] ; E
sigma_matrix = diag(c(svalues[1],svalues[2]),nrow=2) ; sigma_matrix
X = E%*%sigma_matrix ; X
x = X[,1]
y = X[,2]
plot(x,y,col= 'orange',pch = 20, main = 'MDS_MAP')
city.names = c('cph','aar','ode','aal')
text(x,y,pos=4,labels=city.names)
abline(v=0,h=0)")
  return(cat(code))

}


mllab4 = function(){


  code = cat("#Title: Logistic Regression

library(mlbench)
library(MASS)
library(pROC)
data('PimaIndiansDiabetes2')
head(PimaIndiansDiabetes2)
#Descriptive statistics
summary(PimaIndiansDiabetes2)
#Removing missing values
newdata=na.omit(PimaIndiansDiabetes2)
summary(newdata)

set.seed(12)
data1=sort(sample(nrow(newdata),nrow(newdata)*0.7))
data1
#data1 consists of row numbers only and not the actual data

#Splitting into train and test data set
train=newdata[data1,]
test= newdata[-data1,]

#Dimensions of the train and test sets
dim(train)
dim(test)

#To fit a logistic regression model with the train set
log_model=glm(diabetes~.,data=train, family=binomial(link='logit'))
summary(log_model)

#to predict using logistic regression model, probabilities obtained
log_predictions=predict(log_model,test,type='response')
head(log_predictions,10)

log_predictions_rd=ifelse(log_predictions>0.5,1,0)
head(log_predictions_rd,10)")
  return(cat(code))

}



tslab1 = function(){


  code = cat("## To define time-series data

# need to install time-series package

library(timeSeries)
library(TSstudio)

Rainfall = c(799,1774.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall.timeseries = ts(Rainfall,start=c(2012,1),frequency=12)
rainfall.timeseries

plot(rainfall.timeseries,col='red')

## Multiple time series
Rainfall1 = c(799,1774.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
Rainfall2 = c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)

combined.rainfall = matrix(c(Rainfall1,Rainfall2),nrow=12)
rainfall.combined.timeseries = ts(combined.rainfall,start=c(2012,1),frequency = 12)
rainfall.combined.timeseries

plot(rainfall.combined.timeseries,col= 'blue')

data('AirPassengers')
class(AirPassengers)

start(AirPassengers)

end(AirPassengers)

sum(is.na((AirPassengers)))

summary(AirPassengers)

plot(AirPassengers)

## Decomposing data into four components
tsdata = ts(AirPassengers,frequency = 12)
ddata = decompose(tsdata,'multiplicative')
plot(ddata)

plot(ddata$trend)

plot(ddata$seasonal)

plot(ddata$random)

## To plot trend-line on original data-set
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

## To create box-plot by cycle

boxplot(AirPassengers~cycle(AirPassengers,xlab='Date',ylab='Passenger Number (1000s)' , main = 'Monthly air passengers boxplot from 1949-1960'))

library(forecast)

# Seasonal plot
ggseasonplot(AirPassengers)

## Nile-data-set

print(Nile)

length(Nile)

plot(Nile)

plot(Nile,xlab='Year',ylab='River Volume(1e9m^{3})')

## Stationarity and Random time-series (stochastic process)

eps = rnorm(100,mean=0,sd=1)
mu=2
X_t = mu + eps
ts.plot(X_t,main='Example of (random) stationary time seiries',ylab = expression(X[t]))

## Auto-covariance function of a simulated stationary random time-series

acf(X_t,main='Auto-covariance function of X')

## Purely random process with mean 0.5 and standard deviation 1.5

Z = rnorm(100,mean=0.5,sd=1-.5)

X = 0
for(i in 2:length(Z)){

  X[i] = X[i-1] + Z[i]

}

ts.plot(X,main = 'Random walk process')")
  return(cat(code))

}



tslab2 = function(){


  code = cat("# Moving Average

library(forecast)
data(Nile)
opar <- par(no.readonly = T)
par(mfrow = c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main = 'Raw Time Series')
plot(ma(Nile,3), main = 'Simple Moving Average (k=3)', ylim = ylim)
plot(ma(Nile,7), main = 'Simple Moving Average (k=7)', ylim = ylim)
plot(ma(Nile,15), main = 'Simple Moving Average (k=15)', ylim = ylim)
par(opar)
")
  return(cat(code))

}



tslab3 = function(){


  code = cat("## Exponential smoothing techniques (Single, Double and Tripple)

# Single exponential smoothing
library(forecast)
y = c(71,70,69,68,64,65,72,78,75,75,75,70)
yt= ts(y,start=c(1,1),frequency=1)
fit1=ets(yt,model='ANN',alpha=0.1)
A1=fit1$fitted
fit2=ets(yt,model='ANN',alpha=0.3)
A2=fit2$fitted
fit3=ets(yt,model='ANN',alpha=0.5)
A3=fit3$fitted
fit4=ets(yt,model='ANN',alpha=0.7)
A4=fit4$fitted
fit5=ets(yt,model='ANN',alpha=0.9)
A5=fit5$fitted
plot(yt,col='red')
lines(A1,lty=1)
lines(A2,lty=2)
lines(A3,lty=3)
lines(A4,lty=4)
lines(A5,lty=5)
legend('topleft',c('Raw Data','alpha=0.1','alpha=0.3','alpha=0.5','alpha=0.7','alpha=0.9'),lty=c(1,1,2,3,4,5),col=c('red','black','black','black','black','black'))

# Double exponential smoothing
s = c(7,6,5,4,8,9,10,11,10,7)
sl = ts(s,start=c(1,1),frequency=1)
fit = ets(sl,model='AAN')
pred = forecast(fit,4)
pred
plot(fit)

# Triple exponential smoothing
fit = ets(AirPassengers,model='AAA')
fit
pred = forecast(fit,5)
pred
plot(fit)

")
  return(cat(code))

}



tslab4 = function(){


  code = cat("# Auto Regressive Model for Stationary Time Series

set.seed(0)

'yt = 0.5(yt-1) + et'
y = rnorm(250,0,2); y
y1 = numeric(250); y1
y1[1] = y[1]
for (i in 2:250) {
  y1[i] = 0.5*y1[i-1]+y[i]
  }
plot.ts(y1)
acf(y1)

'yt = 0.7(yt-1) + et'
y = rnorm(250,0,2); y
y1 = numeric(250); y1
y1[1] = y[1]
for (i in 2:250) {
  y1[i] = 0.7*y1[i-1]+y[i]
}
plot.ts(y1)
acf(y1)

'yt = 0.7(yt-1) 0.3(yt-2)+ et'
y = rnorm(250,0,2); y
y1 = numeric(250); y1
y1[1] = y[1]
y1[2] = y[2]
for (i in 3:250) {
  y1[i] = 0.7*y1[i-1]+0.3*y1[i-2]+y[i]
}
plot.ts(y1)
acf(y1)

")
  return(cat(code))

}

mllab4()
