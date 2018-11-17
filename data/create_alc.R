# 17.11.2018
# Anna Syreeni
# This script is for combining two alcohol-related datasets from students
# for lated analysis.
# The data were originally downloaded from here: https://archive.ics.uci.edu/ml/datasets/Student+Performance#

# set working directory to where the data now are
setwd("~/Tyojuttuja/opinnot/GitHub/IODS-project/data")

# read in two .csv tables, and check how to do it..
help(read.table)

por <- read.table("student-por.csv", sep = ";", header = TRUE)
mat <- read.table("student-mat.csv", sep = ";", header = TRUE)

# check dimensions and structurese
# por is 649 rows, 33 columns (variables)
dim(por)
str(por)

# mat is 395 rows and 33 columns (variables)
dim(mat)
str(mat)

# common columns to use as identifiers for joining por and mat
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers 
# inner_join to keep only students in both datasets.
library(dplyr)

mat_por <- inner_join(mat, por, by = join_by, suffix = c(".mat", ".por"))

# look the new combined dataset -->OK!
dim(mat_por)
#[1] 382  53

colnames(mat_por)
glimpse(mat_por)

# Because some of the questions were present in both original
# datasets, now they are duplicated in the combined data.
# We'll take averages of the numerical data, if not numeric, the first value
# is taken


# first, I create a new data frame with only the joined columns 
# (no duplicated columns among these)

alc <- select(mat_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(mat)[!colnames(mat) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# Now handle with the duplicated columns...
#for every column name not used for joining...

for(column_name in notjoined_columns) {
  # select two columns from 'mat_por' with the same original name
  two_columns <- select(mat_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new alc data
# There are 382 observations of 33 variables -->OK!
glimpse(alc)

#Take the average of the answers related to weekday and weekend 
#alcohol consumption to create a new column 'alc_use' to the joined
#data. Then use 'alc_use' to create a new logical column 'high_use'
#which is TRUE for students for which 'alc_use' is greater than 2
#(and FALSE otherwise). (1 point)

# Took the average of weekday and weekend alcohol consumption (Dalc, Walc)
# and make new column from it
alc$alc_use <- (alc$Dalc + alc$Walc)/2
# could have been done also with mutate: 
# alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

summary(alc$alc_use) #variable looks OK

#Define a new column of high alcohol use >2
alc <- mutate(alc, high_use = alc_use > 2)
summary(alc$high_use) #OK

#check the alc table created
str(alc)
dim(alc) #382 observations of 35 variables--> OK!

#save alc table
write.table(alc, file="student_alc.txt", sep="\t", dec=".")

test_table <- read.table(file = "student_alc.txt", sep="\t")

str(test_table) #looks good!
head(test_table)
