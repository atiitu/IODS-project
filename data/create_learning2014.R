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
learning_data$Attitude #nämä jaetaan kymmenella
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

#look how these new columns are
learning_data$deep
learning_data$stra
learning_data$surf

#Select everyone (rows) that have points zero 
learning_data <- filter(learning_data, Points > 0)

#Name the wanted columns
colnames(learning_data)
learn_variables <- c("gender", "Age", "attitude", "deep", "stra", "surf","Points")

#create this smaller analysis dataset
ldata <- select(learning_data, one_of(learn_variables))

#Check the structure
str(ldata)
colnames(ldata)

#change some column names:
# change the name of the second column
colnames(ldata)[2] <- "age"

# change the name of "Points" to "points"
colnames(ldata)[7] <- "points"

#check the colnames again
colnames(ldata)
str(ldata)
dim(ldata)

#Looks cool. Data in ldata ready for analysis!

#Set working directory
setwd("~/Tyojuttuja/opinnot/GitHub/IODS-project/data")

#Save data table for later use
#help("write.table")
write.table(ldata, file="learning2014.txt", sep="\t", dec=".")

#Test that data is OK when read again
test_table <- read.table(file = "learning2014.txt", sep="\t")

str(test_table) #looks good!
head(test_table)
