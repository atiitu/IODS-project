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



