#Datacamp week 3 logistic regression


url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets"

# tassa siis liitetaan tuon tiedoston nimi tuon osoitteen peraan
# paste-funktiolla.
# web address for math class data
url_math <- paste(url, "student-mat.csv", sep = "/")

# print out the address
url_math

# read the math class questionaire data into memory
math <- read.table(url_math, sep = ";" , header=TRUE)

# web address for portuguese class data
url_por <- paste(url, "student-por.csv", sep ="/")

# print out the address
url_por

# read the portuguese class questionaire data into memory
por <- read.table(url_por, sep = ";", header = TRUE)

# look at the column names of both data
# They are similar
colnames(math)
colnames(por)


# math and por are available

# access the dplyr library
library(dplyr)

# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

# see the new column names
colnames(math_por)

# glimpse at the data
glimpse(math_por)

####################3
# if formula
#You'll do this by using a combination of a for-loop and an if-else structure.
#
#The if() function takes a single logical condition as an argument and performs an action only if that condition is true. if can then be combined with else, which handles the cases where the condition is false.

#if(condition) {
#do something
#} else {
#do something else
#}
######################
# dplyr, math_por, join_by are available

# print out the column names of 'math_por'
colnames(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
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

# glimpse at the new combined data
glimpse(alc)

# alc is available

# access the 'tidyverse' packages dplyr and ggplot2
library(dplyr); library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by sex
g2 + facet_wrap("sex") + geom_bar()

#############################3
#Gather takes multiple columns and collapses into key-value pairs,
#duplicating all other columns as needed. 
#You use gather() when you notice that you have columns 
#that are not variables.
##########################
#I do not understand still....
# alc is available

# access the tidyverse libraries tidyr, dplyr, ggplot2
library(tidyr); library(dplyr); library(ggplot2)

# glimpse at the alc data
glimpse(alc)

# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse
# ei aavistustakaan mita gather tekee
help(gather)

# draw a bar plot of each variable
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

# alc is available

# access the tidyverse libraries dplyr and ggplot2
library(dplyr); library(ggplot2)

# produce summary statistics by group
#alc %>% group_by(sex) %>% summarise(count = n())
#1 F       198
#2 M       184

#alc %>% group_by(sex) %>% summarise(count = n(), mean(G3))
#  sex   count `mean(G3)`
#  <fct> <int>      <dbl>
#1 F       198       9.84
#2 M       184       11.0

alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

#  sex   high_use count `mean(G3)`
#  <fct> <lgl>    <int>      <dbl>
#1 F     FALSE      157       9.69
#2 F     TRUE        41      10.4 
#3 M     FALSE      113      11.6 
#4 M     TRUE        71       9.97

library(ggplot2)

# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

# initialise a plot of high_use and absences
g1 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g2 <- g1 + geom_boxplot() + ylab("absences")

#add title
g2+ ggtitle("Student absences by alcohol consumption and sex")



#######
# LOGISTIC REGRESSION STARTS

# alc is available 

# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model
coef(m)

# alc and dlyr are available 
# For this reason, the exponents of the coefficients of a logistic # # #regression model can be interpret as odds ratios between a unit change # # (vs no change) in the corresponding explanatory variable.

# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# compute odds ratios (OR)
# malli ei siis anna suoraan odds-ratiota...
OR <- coef(m) %>% exp
OR

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

#confint(m)
#                  2.5 %     97.5 %
#(Intercept) -2.42093146 -1.5240479
#failures     0.10930110  0.7024622
#absences     0.03992127  0.1100712
#sexM         0.51542103  1.4760367

#CI
#                 2.5 %    97.5 %
#(Intercept) 0.08883883 0.2178283
#failures    1.11549818 2.0187171
#absences    1.04072883 1.1163576
#sexM        1.67434331 4.3755694

# print out the odds ratios with their confidence intervals
cbind(OR, CI)
#                   OR      2.5 %    97.5 %
#(Intercept) 0.1417107 0.08883883 0.2178283
#failures    1.4987270 1.11549818 2.0187171
#absences    1.0756623 1.04072883 1.1163576
#sexM        2.6871365 1.67434331 4.3755694

# alc, dplyr are available

# fit the model
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

#The predict() function can be used to make predictions with a model #object. If predict() is not given any new data, it will use the data #used #for finding (fitting, leaning, training) the model to make #predictions. 

#In the case of a binary response variable, the 'type' argument of #predict() can be used to get the predictions as probabilities (instead #of log of odds, the default).


# predict() the probability of high_use

probabilities <- predict(m, type = "response")

# ilmeisesti jokaiselle taulukon tyypille tuli joku todennäköisyys
#probabilities

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = (probability > 0.5))

#tsekkaan uuden kolumnin
str(alc$prediction)
#logi [1:382] FALSE FALSE FALSE FALSE FALSE FALSE ...

# see the last ten original classes, predicted probabilities, and class predictions = en ymmarra mita halutaan (original classes?)
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

#  failures absences sex high_use probability prediction
#373        1        0   M    FALSE   0.3633449      FALSE
#374        1       14   M     TRUE   0.6130701       TRUE
#375        0        2   F    FALSE   0.1408685      FALSE
#376        0        7   F    FALSE   0.1910175      FALSE
#377        1        0   F    FALSE   0.1751799      FALSE
#378        0        0   F    FALSE   0.1241213      FALSE
#379        1        0   F    FALSE   0.1751799      FALSE
#380        1        0   F    FALSE   0.1751799      FALSE
#381        0        3   M     TRUE   0.3215447      FALSE
#382        0        0   M     TRUE   0.2757800      FALSE

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)
#        prediction
#high_use FALSE TRUE
#   FALSE   258   12
#   TRUE     86   26

# alc is available

# access dplyr and ggplot2
library(dplyr); library(ggplot2)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
#table(high_use = alc$high_use, prediction = #alc$prediction)
#       prediction
#high_use FALSE TRUE
#   FALSE   258   12
#   TRUE     86   26

#table(high_use = alc$high_use, prediction = #alc$prediction) %>% prop.table()
#                  prediction
#high_use      FALSE       TRUE
#FALSE     0.67539267 0.03141361
#TRUE      0.22513089 0.06806283

table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()
#        prediction
#high_use      FALSE       TRUE        Sum
#   FALSE 0.67539267 0.03141361 0.70680628
#   TRUE  0.22513089 0.06806283 0.29319372
#   Sum   0.90052356 0.09947644 1.00000000


# the logistic regression model m and dataset alc with predictions are available
summary(m)

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}
#en kylla ihan ymmarra naita...ts tota funktiota ylipaatansa
# call loss_func to compute the average number of wrong predictions in the (training) data

#loss_func(class = alc$high_use, prob = 0)
#[1] 0.2931937

#loss_func(class = alc$high_use, prob = 1)
#[1] 0.7068063

loss_func(class = alc$high_use, prob = alc$probability)
#[1] 0.2565445



# the logistic regression model m and dataset alc (with predictions) are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)
#[1] 0.2565445

############################
# K-fold cross-validation
library(boot)

#Perform leave-one-out cross-validation and print out the mean prediction #error for the testing data. (nrow(alc) gives the observation count in #alc and using K = nrow(alc) defines the leave-one-out method. The cv.glm #function from the 'boot' library computes the error and stores it in #delta. See ?cv.glm for more information.)

#cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = nrow(alc))

# average number of wrong predictions in the cross validation
#cv$delta[1]

#Adjust the code: Perform 10-fold cross validation. Print out the mean prediction error for the testing data. Is the prediction error higher or lower on the testing data compared to the training data? Why?
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K =10)

cv$delta[1]
#0.2617801

