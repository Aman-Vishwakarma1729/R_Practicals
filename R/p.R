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
ggcorrplot(cor10,hc.order=TRUE,type='lower',lab=TRUE)



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

## Exporting excel file

library(readxl)
dir()
excel_df = read.excel('Path to excel file')
excel_sheets('Path of excel file')
excel_df1 = read.excel('Path to excel file',sheet='sheet name'))")
  return(cat(code))

}

