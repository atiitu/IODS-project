# Open Data Science -course
# Anna Syreeni
# 6.11.2018

###################################################
# This is a script to create data-table used later.

# 2. STEP in excersize ###

# read table from internet to R
learning_data <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", header=T, sep="\t", as.is=T)

#check dimensions
dim(learning_data)

#Data includes 183 rows and 60 columns

#check structure
str(learning_data)

#str shows what are the variables, mostly integers, gender is in characters

#3. STEP in excersize ###

#scale Attitude variable (because it is a sum of 10 questions)
learning_data$attitude <- learning_data$Attitude / 10

# Next steps are for combining right questions to form columns deep, stra and surf

#select for deep
deep_questions <- c("D03", "D11", "D19", "D27", "D07", 
                    "D14", "D22", "D30","D06",  "D15", "D23", "D31")
#select for stra
strategic_questions <- c("ST01","ST09","ST17","ST25",
                         "ST04","ST12","ST20","ST28")
#select for surf
surface_questions <- c("SU02","SU10","SU18","SU26",
                       "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")

#Call library you need
library(dplyr) 

#I had to install that package, and it took some time, and R stopped working...

#Select right columns
deep_columns <- select(learning_data, one_of(deep_questions))
stra_columns <- select(learning_data, one_of(strategic_questions))
surf_columns <- select(learning_data, one_of(surface_questions))

#check dimension of some
dim(deep_columns) #look OK! 12 columns 

#make columns deep, stra and surf that are averages of the values in columns
#in deep_columns, stra_columns and surf_columns

learning_data$deep <- rowMeans(deep_columns)
learning_data$stra <- rowMeans(stra_columns)
learning_data$surf <- rowMeans(surf_columns)


learn_variables <- c("gender", "Age", "Attitude", "")

Create an analysis dataset with the variables gender,
age, attitude, deep, stra, surf and points by combining 
questions in the learning2014 data, as defined in the datacamp 
exercises and also on the bottom part of the following page 
(only the top part of the page is in Finnish).
http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt. 
Scale all combination variables to the original scales (by taking the mean).
Exclude observations where the exam points variable is zero.
(The data should then have 166 observations and 7 variables) (1 point)