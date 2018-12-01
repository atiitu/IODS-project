
# modified human is available
# created with week4 code!!!!!
library(dplyr)

# standardize the variables
human_std <- scale(human)

# print out summaries of the standardized variables
summary(human_std)

# perform principal component analysis (with the SVD method)
pca_human <- prcomp(human_std)
summary(pca_human)

help(prcomp)

#Experiment with the argument cex of biplot(). It should be #a vector of length 2 and it can be used to scale the #labels in the biplot. Try for example cex = c(0.8, 1). #Which number affects what? The first affect the #countrynames, second to variable names in arrows.

# draw a biplot of the principal component representation #and the original variables

#add the argument about color (first for countries, other for variables)

biplot(pca_human, choices = 1:2, cex = c(0.1, 1), col = c("grey40", "deeppink2"))

# pca_human, dplyr are available

# create and print out a summary of pca_human
s <- summary(pca_human)
s

# rounded percentages of variance captured by each PC
pca_pr <- round(100*s$importance[2,], digits = 1) 

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])





# the tea dataset and packages FactoMineR, ggplot2, dplyr and tidyr are available

# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))

# look at the summaries and structure of the data
summary(tea_time)
str(tea_time)
#'data.frame':	300 obs. of  6 variables:


# visualize the dataset
help(gather)

#there comes warning after initializing this, ignore it.
#theme added to show axis better
gather(tea_time) %>% ggplot(aes(value)) + 
  facet_wrap("key", scales = "free") + geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# tea_time is available

# multiple correspondence analysis
# if graph = TRUE, a graph is drawn to pdf-file
mca <- MCA(tea_time, graph = FALSE)

# summary of the model
# the summary is explained in the video...
summary(mca)

# visualize MCA
# you can either plot the variables or the individuals or both
# invisible argument tells which #is invisible (var, ind)
#habillage laittaa eri #värejä sanoihin, ilman #logiikkaa
plot(mca, invisible=c("ind"), 
     habillage = "quali")

