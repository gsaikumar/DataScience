# Insurance model 
# reading the data 
dataset = read.csv("insurance.csv")
#EDA 
#understanding the structure of the data
summary(dataset)
str(dataset)
# restructuring the data
dataset$children = as.factor(dataset$children) # as no.of children is more sensible as factor
str(dataset)

# Univariate anaylysis(dependent variable)
summary(dataset$charges)
hist(dataset$charges)
#Skewness And kurtosis
#install.packages('e1071')
library(e1071)
skewness(dataset$charges)
#1.5 mean highly skewed data
kurtosis(dataset$charges)

# the charges variable is right skewed

# Bivariate Analysis ( relation b/w dependent and each of the independent variables)
# charges vs age
library(ggplot2)
ggplot(aes(dataset$age , dataset$charges), data = dataset) + geom_point() + 
  geom_smooth(method = "lm", color ='red', se =F)
#
# charges vs sex
ggplot(aes(dataset$sex , dataset$charges), data = dataset) + geom_boxplot()
# many outliers are present 
# charges vs bmi
ggplot(aes(dataset$bmi , dataset$charges), data = dataset) + geom_point() +
  geom_smooth(method = 'lm', color ='red', se =F)
# there is a fanning effect 
# chanrges vs children
ggplot(aes(dataset$children , dataset$charges), data = dataset) + geom_boxplot()
# many outlier are present
# charges vs smoker
ggplot(aes(dataset$smoker , dataset$charges), data = dataset) + geom_boxplot()
# outlier are present for smoker=no
# charges vs region
ggplot(aes(dataset$region , dataset$charges), data = dataset) + geom_boxplot()
#potential outliers are present 

## Objective Bivariate Analysis

str(dataset)
names(dataset)
# identify non-numeric variables => sex,childern,smoker,region
# identify numeric variables and extract them to new data set
sub_dataset = dataset[,c('age','bmi','charges')]

# pair plot 
library(GGally)
ggpairs(sub_dataset)
# or correleation plot
corrmat = cor(sub_dataset)
library(corrplot)
corrplot.mixed(corrmat)
# there a weak correlation present b/w the variables

## DATA Preprocessing
sum(is.na(dataset$charges))
# there are no na values hence no further action required

## Spliting the Data in to training and test sets
library(caTools)
set.seed(123)
split = sample.split(dataset$charges, SplitRatio = 0.7)
train_data = subset(dataset, split==T)
test_data = subset(dataset, split==F)

## FIrst vesrsion of the model on training data 
regressor1 = lm(charges~., data=train_data)
y_pred = predict(regressor1, newdata = test_data)
# testing the assumption 
# Assumption 1 - linearity (residuals vs fits)

residuals = resid(lm(charges~., data = test_data))
#or
library(MASS)
stanres = stdres(lm(charges~., data = test_data))

#residualvsfits plot
ggplot( aes(y_pred, stanres), data = test_data) + geom_point()
# non-linear, hetroscedacity, potential influential points

# data transformation 
# lets transform the mpg 
hist(dataset$charges)
skewness(dataset$charges)
kurtosis(dataset$charges)

#applying log transformation 

dataset$charges = log(dataset$charges)
hist(dataset$charges)
skewness(dataset$charges)
kurtosis(dataset$charges)


## BiVariate Analysis ( post transfromation #)

# create second version of the model
# splitting the data set again

library(caTools)
set.seed(123)
split = sample.split(dataset$charges, SplitRatio = 0.7)
train_data = subset(dataset, split==T)
test_data = subset(dataset, split==F)

# creating regressor 2 after transforming charges variable 
regressor2 = lm(charges~., data=train_data)
y_pred2 = predict(regressor2, newdata = test_data)

# testing assumption1 again linearity
stanres2 = stdres(lm(charges~., data = test_data))

#assumption 1 linearity ~residualvsfits plot
ggplot( aes(y_pred2, stanres2), data = test_data) + geom_point()
# curvy linear and some outliers 

# assumption 2 ~ independence of the residuals 
library(car)
durbinWatsonTest(regressor2)
#the value is b/w 1.5 and 2.5 hence it is independent