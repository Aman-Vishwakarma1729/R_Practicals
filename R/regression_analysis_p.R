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

