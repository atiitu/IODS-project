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
# access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)




