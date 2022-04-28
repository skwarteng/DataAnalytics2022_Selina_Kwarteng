#Selina Kwarteng
#Assignment 3

#first dataset nyt20
nyt20_data <- read.csv(file.choose(), header = TRUE)
nyt20_data
View(nyt20_data)

#boxplot
boxplot(nyt20_data$Age, nyt20_data$Impressions, names = c("Age",'Impressions'))


#dataset nyt21
nyt21_data <- read.csv(file.choose(), header = TRUE)
nyt21_data
#View(nyt21_data)

#boxplot
boxplot(nyt21_data$Age, nyt21_data$Impressions, names = c("Age",'Impressions'))

#dataset nyt22
nyt22_data <- read.csv(file.choose(), header = TRUE)
nyt22_data
#View(nyt22_data)

#boxplot
boxplot(nyt22_data$Age, nyt22_data$Impressions, names = c("Age",'Impressions'))

#dataset nyt23
nyt23_data <- read.csv(file.choose(), header = TRUE)
nyt23_data
#View(nyt23_data)

#boxplot
boxplot(nyt23_data$Age, nyt23_data$Impressions, names = c("Age",'Impressions'))

#dataset nyt24
nyt24_data <- read.csv(file.choose(), header = TRUE)
nyt24_data
#View(nyt24_data)

#boxplot
boxplot(nyt24_data$Age, nyt24_data$Impressions, names = c("Age",'Impressions'))

#dataset nyt25
nyt25_data <- read.csv(file.choose(), header = TRUE)
nyt25_data
#View(nyt25_data)

#boxplot
boxplot(nyt25_data$Age, nyt25_data$Impressions, names = c("Age",'Impressions'))


#dataset nyt26
nyt26_data <- read.csv(file.choose(), header = TRUE)
nyt26_data
#View(nyt26_data)

#boxplot
boxplot(nyt26_data$Age, nyt26_data$Impressions, names = c("Age",'Impressions'))

#-------------------------------------------------------------------------------

#histograms

#dataset 1
summary(nyt20_data)
hist(nyt20_data$Age, breaks = seq(from = 0, to = 120, by = 10))
hist(nyt20_data$Impressions, seq(0,20,1))

#dataset 2
summary(nyt21_data)
hist(nyt21_data$Age, breaks = seq(from = 0, to = 120, by = 10))
hist(nyt21_data$Impressions, seq(0,20,1))

#dataset 3
summary(nyt22_data)
hist(nyt22_data$Age, breaks = seq(from = 0, to = 120, by = 10))
hist(nyt22_data$Impressions, seq(0,25,1))

#dataset 4
summary(nyt23_data)
hist(nyt23_data$Age, breaks = seq(from = 0, to = 120, by = 10))
hist(nyt23_data$Impressions, seq(0,20,1))

#dataset 5
summary(nyt24_data)
hist(nyt24_data$Age, breaks = seq(from = 0, to = 110, by = 10))
hist(nyt24_data$Impressions, seq(0,20,1))

#dataset 6
summary(nyt25_data)
hist(nyt25_data$Age, breaks = seq(from = 0, to = 120, by = 10))
hist(nyt25_data$Impressions, seq(0,20,1))

#dataset 7
summary(nyt26_data)
hist(nyt26_data$Age, breaks = seq(from = 0, to = 120, by = 10))
hist(nyt26_data$Impressions, seq(0,20,1))

#------------------------------------------------------------------------------

install.packages("ggplot2")
library(ggplot2)

ggplot(nyt24_data, aes(Age,Impressions)) + stat_ecdf(geom = "step")

#------------------------------------------------------------------------------

#performing the wilcox test
x = nyt24_data$Age
y = nyt24_data$Impressions

wilcox.test(x, y, alternative = "g")

#t test
t.test(x, y, paired = TRUE)

#-------------------------------------------------------------------------------

#dataset 1
summary(nyt20_data)
hist(nyt20_data$Clicks, breaks = seq(from = 0, to = 5, by = 1))
hist(nyt20_data$Signed_In, seq(0,1,0.5))

#dataset 2
summary(nyt21_data)
hist(nyt21_data$Clicks, breaks = seq(from = 0, to = 5, by = 1))
hist(nyt21_data$Signed_In, seq(0,1,0.5))

#dataset 3
summary(nyt22_data)
hist(nyt22_data$Clicks, breaks = seq(from = 0, to = 5, by = 1))
hist(nyt22_data$Signed_In, seq(0,1,0.5))

#dataset 4
summary(nyt23_data)
hist(nyt23_data$Clicks, breaks = seq(from = 0, to = 5, by = 1))
hist(nyt23_data$Signed_In, seq(0,1,0.5))

#------------------------------------------------------------------------------

install.packages("ggplot2")
library(ggplot2)

ggplot(nyt23_data, aes(Clicks,Signed_In)) + stat_ecdf(geom = "step")

#------------------------------------------------------------------------------

#performing the wilcox test
x = nyt23_data$Clicks
y = nyt23_data$Signed_In

wilcox.test(x, y, alternative = "g")

#t test
t.test(x, y, paired = TRUE)
