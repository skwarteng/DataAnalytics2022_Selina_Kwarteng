#In class work 3/3/2022

#Example #1: Cook's Distance' example using mtcars

mtcars
head(mtcars)
str(mtcars)
model1 = lm(mpg ~ cyl + wt, data = mtcars)
model1
help("cooks.distance")
plot(model1, pch = 18, col = 'red', which = c(4))
dim(mtcars)


cooks.distance(model1)
CooksDistance = cooks.distance(model1)

round(CooksDistance, 5)

sort(round(CooksDistance, 5))

#Example #2

install.packages("dplyr")
library(ISLR)
library(dplyr)

head(Hitters)

dim(Hitters)

is.na(Hitters)

HittersData = na.omit(Hitters)

dim(HittersData)

glimpse(HittersData)

head(HittersData)

SalaryPredictModel1 = lm(Salary ~., data = HittersData)

summary(SalaryPredictModel1)

#Cooks Distance
cooksD = cooks.distance(SalaryPredictModel1)
cooksD
influential = cooksD[(cooksD > (3*mean(cooksD, na.rm=TRUE)))]
influential

names_of_influential = names(influential)
names_of_influential

outliers = HittersData[names_of_influential,]
Hitters_Without_Outliers = HittersData %>% anti_join(outliers)

#Model 2: without the outliers
SalaryPredictModel1 = lm(Salary~., data = Hitters_Without_Outliers)
summary(SalaryPredictModel1)
