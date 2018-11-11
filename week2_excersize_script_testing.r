# Week 2: Regression model excersize
# Anna Syreeni
# This document has all the codes. They are also in chapter2.Rmd file

#First, set working directory to where data is
setwd("~/Tyojuttuja/opinnot/GitHub/IODS-project/data")

#Read in data
data <- read.table(file = "learning2014.txt", sep="\t")

#Look that variables are OK
str(data) #looks good!
head(data)

#Summaries of all variables in the data are found below:

summary(data$gender)
summary(data$age)
summary(data$attitude)
summary(data$deep)
summary(data$stra)
summary(data$surf)
summary(data$points)

Show a graphical overview of the data 
and show summaries of the variables in the data.
Describe and interpret the outputs,
commenting on the distributions of the variables 
and the relationships between them. (0-3 points)

#Summary of all data

#Access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)

# Variable dispersions and correlations can be studied with ggpairs-function

data_correlations <- ggpairs(data, 
                            mapping = aes(col= gender, alpha = 0.3),
                            lower = list(combo = wrap("facethist", bins = 20)))

data_correlations

##Next 
#Choose three variables as explanatory
#variables and fit a regression model 
#where exam points is the target (dependent) variable. 
#Show a summary of the fitted model and comment and 
#interpret the results.
#Explain and interpret the statistical test 
#related to the model parameters. 
#If an explanatory variable in your model does
#not have a statistically significant relationship 
#with the target variable, 
##remove the variable from the model
#and fit the model again without it. (0-4 points

model <- lm(points ~ attitude + stra + surf, data = data)
summary(model)

model2 <- lm(points ~ attitude, data = data)
summary(model