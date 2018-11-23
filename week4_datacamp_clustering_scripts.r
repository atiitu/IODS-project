# access the MASS package
library(MASS)

# load the data
data("Boston")
#14 variables, 506 observations
# explore the dataset
str(Boston)
summary(Boston)

# plot matrix of the variables
pairs(Boston)

# MASS, corrplot, tidyverse and Boston dataset are available

# calculate the correlation matrix and round it
cor_matrix <- cor(Boston) %>% round(2)
help(round)

# This is very nice way of presenting correlations!!!
# print the correlation matrix
cor_matrix

# visualize the correlation matrix
# upper tekee korrelaatiokolmion, oli ensin nelio
# was nicer looking, before the 
# cl.pos, tl.pos arguments..
corrplot(cor_matrix, method="circle", type ="upper",
         cl.pos= "b", 
         tl.pos = "d", 
         tl.cex = 0.6)

# MASS and Boston dataset are available

# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)


# MASS, Boston and boston_scaled are available

# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, 
             label = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# boston_scaled is available

# number of rows in the Boston dataset 
n <- nrow(boston_scaled)
n
# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)
ind #random row numbers saved

# create train set
train <- boston_scaled[ind,]
class(train) # now a smaller train data.frame was generated with random rows

# create test set 
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)

# MASS and train are available

# linear discriminant analysis
# now the model uses crime as a target variable
# and all other variables as predictors (put .)
lda.fit <- lda(crime ~ ., data = train)

# print the lda.fit object
lda.fit


# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
# all was black before adding the col and pch
plot(lda.fit, dimen = 2, col = classes, pch = classes)

# lda.fit, correct_classes and test are available
?predict.lda

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

lda.arrows(lda.fit, myscale = 2)

#######################################
###### CLUSTERING

# load MASS and Boston
library(MASS)
data('Boston')

# euclidean distance matrix
dist_eu <- dist(Boston)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
help(dist)
dist_man <- dist(Boston, method = "manhattan")

# look at the summary of the distances
summary(dist_man)

# Boston dataset is available

# k-means clustering
km <-kmeans(Boston, centers = 3)

# plot the Boston dataset with clusters
summary(km$cluster)
km$cluster
#voi valita vain osan sarakkeista kuvaan []
pairs(Boston, col = km$cluster)

# MASS, ggplot2 and Boston dataset are available
# K-means might produce different results every time, because it randomly # assigns the initial cluster centers. The function set.seed() can be used # to deal with that.
set.seed(123)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(Boston, k)$tot.withinss})

# visualize the WCSS results
qplot(x = 1:k_max, y = twcss, geom = 'line')

# k-means clustering
km <-kmeans(Boston, centers = 2)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)

###################################
# principal components analysis####
# read the human data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)

# look at the (column) names of human
names(human)

# look at the structure of human
str(human)
#'data.frame':	195 obs. of  19 variables:

# print out summaries of the variables
summary(human)

# tidyr package and human are available

# access the stringr package
library(stringr)

# look at the structure of the GNI column in 'human'
str(human$GNI)

#it is a factor, should be number..( has thousand commas)
#To get rid of the unwanted commas, we need string manipulation.
# remove the commas from GNI and print out a numeric version of it
# tämä ei vielä tee muutosta taulukon GNI muuttujaan...hmm
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()

# human with modified GNI and dplyr are available
str(human$GNI) #no nyt se on muuttunut...voi varmaan vain osoittaa edellisen tähän sarakkeeseen

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# select the 'keep' columns
human <- select(human, one_of(keep))

# print out a completeness indicator of the 'human' data
# näyttää kuinka monella on arvo kaikissa sarakkeissa
complete.cases(human)

# print out the data along with a completeness indicator as the last column
# sai lisättyä sarakkeen näin! comp= sarakkeen nimi
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values, eli filter ottaa mukaan noi mitä #laitettu
human_ <- filter(human, complete.cases(human))

# human without NA is available
#df[,] # select every row and every column
#df[1:5, ] # select first five rows
#df[, c(2, 5)] # select 2nd and 5th columns

# look at the last 10 observations of human
tail(human, n = 10L)
?tail

# define the last indice we want to keep
last <- nrow(human) - 7
last #155

# choose everything until the last 7 observations
human_ <- human[1:155, ]

# add countries as rownames
rownames(human_) <- human_$Country
rownames(human_)