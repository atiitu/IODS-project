# Open Data Science -course
# Anna Syreeni
# 6.11.2018

# This is a script to create data-table used later.

# read table from internet to R
learning_data <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", header=T, sep="\t", as.is=T)

#check dimensions
dim(learning_data)

#Data includes 183 rows and 60 columns

#check structure
str(learning_data)

#str shows what are the variables, mostly integers, gender is in characters
