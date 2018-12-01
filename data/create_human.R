# Open data science
# script to modify human table for analysis
# Anna Syreeni 29.11.2018


#The human dataset contains 19 variables from 195 different countries. More detailed description of the data can be read from http://hdr.undp.org/en/content/human-development-index-hdi and variables are explained here as follows
#https://raw.githubusercontent.com/TuomoNieminen/Helsinki-Open-Data-Science/master/datasets/human_meta.txt


#read table online
human <- read.table(file = "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep =",", header = TRUE)

# structure of the data
str(human)
dim(human) #[1] 195  19

#GNI column has commas..remove and change to numbers then
str(human$GNI)

library(tidyr)
library(stringr)

human$GNI <- str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric
str(human$GNI) #ok!


#We only want to keep these variables from the data
keep_columns <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

library(dplyr)
human <- dplyr::select(human, one_of(keep_columns))

#completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human))
str(human_) #OK!

#seven last rows 156-162 are regions so will only include those before it
human_$Country
human_ <- human_[1:155, ]

# add countries as rownames
rownames(human_) <- human_$Country

# remove the Country variable
human_ <- select(human_, -Country)
str(human_)

write.table(human_, file= "human.txt", sep = "\t", col.names =TRUE, row.names =TRUE)

#FINISHED

