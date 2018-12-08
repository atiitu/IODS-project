# Anna Syreeni 8.12.2018
# Open data science
# Creating long format of RATS and BPRS data
# Both are read in first and BPRS data is handeled first and RATS later in this
# R script.




#set working directory
setwd("~/Tyojuttuja/opinnot/GitHub/IODS-project/data")



# Read data in
B <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RAT <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')


#############################
#  BPRS data ###############

# Check data dimensions and structures

dim(B) #[1] 40 11
str(B) #
#It's a data.frame of 40 obs (people) and 11 variables
# of which 0-8 measurements from week a part.
#'data.frame':	40 obs. of  11 variables:
#  $ treatment: int  1 1 1 1 1 1 1 1 1 1 ...
#$ subject  : int  1 2 3 4 5 6 7 8 9 10 ...
#$ week0    : int  42 58 54 55 72 48 71 30 41 57 ...
#$ week1    : int  36 68 55 77 75 43 61 36 43 51 ...
#$ week2    : int  36 61 41 49 72 41 47 38 39 51 ...
#$ week3    : int  43 55 38 54 65 38 30 38 35 55 ...
#$ week4    : int  41 43 43 56 50 36 27 31 28 53 ...
#$ week5    : int  40 34 28 50 39 29 40 26 22 43 ...
#$ week6    : int  38 28 29 47 32 33 30 26 20 43 ...
#$ week7    : int  47 28 25 42 38 27 31 25 23 39 ...
#$ week8    : int  51 28 24 46 32 25 31 24 21 32 ...


names(B)
#[1] "treatment" "subject"   "week0"     "week1"     "week2"     "week3"    
#[7] "week4"     "week5"     "week6"     "week7"     "week8" 

# study variable summaries -->OK! undestood the data structure.
summary(B)

#Cathegorical to factors
B$treatment <- factor(B$treatment)
B$subject <- factor(B$subject)

#Now convert to long format
library(dplyr)
library(tidyr)
B_L <- B %>% gather(key = weeks, value = bprs, -treatment, -subject)

#Add variable where is the week number
B_L <-  B_L %>% mutate(week = as.integer(substr(weeks,5,5)))

#Check the long data format
glimpse(B_L)

#Now 360 observations(rows) and 5 variables
#Variables: 5
#$ treatment <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
#$ subject   <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17...
#$ weeks     <chr> "week0", "week0", "week0", "week0", "week0", "week0", "we...
#$ bprs      <int> 42, 58, 54, 55, 72, 48, 71, 30, 41, 57, 30, 55, 36, 38, 6...
#$ week      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...

#This means that all weekly measurements are at the bprs variable (altogether 360 measurements)
#From 40 people (40*9 =360)
#--> long format understood.

#Write to a table
write.table(B_L, file = "bprs_long.txt", sep="\t", col.names=TRUE, row.names=FALSE)



#############################
## RATS #####################

#look at the data
dim(RAT)
str(RAT)
summary(RAT)
#It's all about 16 rats in three groups and weight measuremnts

# Factor variables ID and Group
RAT$ID <- factor(RAT$ID)
RAT$Group <- factor(RAT$Group)

# Glimpse the data
glimpse(RATS)

#Data converted to long format:

#Convert data to long form

RAT_L <- RAT %>% gather(key = WD, value = Weight, -ID, -Group)

# Add a variable
RAT_L <- RAT_L %>% mutate(Time = as.integer(substr(WD,3,4))) 

# Glimpse the data
glimpse(RAT_L)
summary(RAT_L)

#clearly the data is now in long format, where all weight-measurements 
#are in the same column.
# Great, data ready.

#Write table

write.table(RAT_L, file="rats_long.txt", sep="\t", col.names=TRUE, row.names=FALSE)

#test reading both tables in 
RATS_table <- read.table("rats_long.txt", header=T, sep = "\t")
BPRS_table <- read.table("bprs_long.txt", header = T, sep = "\t")

#check that are OK
str(RATS_table)
str(BPRS_table)

