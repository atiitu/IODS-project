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
        
#Using a summary of your fitted model, explain the 
#relationship between the chosen explanatory variables 
#and the target variable (interpret the model parameters).
#Explain and interpret the multiple R squared of the model. 
#(0-3 points)

# WRITTEN!


#Produce the following diagnostic plots:
#Residuals vs Fitted values, Normal QQ-plot
#and Residuals vs Leverage. Explain the assumptions
#of the model and interpret the validity of those 
#assumptions based on the diagnostic plots. (0-3 points)

#We will focus on plots 1, 2 and 5: Residuals vs Fitted values, 
#Normal QQ-plot and Residuals vs Leverage.

# draw diagnostic plots using the plot() function. Choose the plots 1, 2 and 5
#Residuals vs Fitted values
#This plot shows if residuals have non-linear patterns
plot(model2, which = c(1))

#Normal QQ-plot
#This plot shows if residuals are normally distributed. 
#Do residuals follow a straight line well or do they 
#deviate severely? It's good if residuals are lined well 
#on the straight dashed line.

plot(model2, which = c(2))


#Residuals vs Leverage.
plot(model2, which = c(5))
