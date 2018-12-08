# Read the BPRS data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)

# Look at the (column) names of BPRS
names(BPRS)

# Look at the structure of BPRS
str(BPRS)

# Print out summaries of the variables
summary(BPRS)

# The data BPRS is available

# Access the packages dplyr and tidyr
library(dplyr)
library(tidyr)

# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))

# Take a glimpse at the BPRSL data
glimpse(BPRSL)



#Access the package ggplot2
library(ggplot2)

# Draw the plot
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

# dplyr, tidyr and ggplot2 packages and BPRSL are available

# Standardise the variable bprs
BPRSL <- BPRSL %>%
  group_by(week) %>%
  mutate(stdbprs = (bprs - mean(bprs))/sd(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL)

# Plot again with the standardised bprs
ggplot(BPRSL, aes(x = week, y = stdbprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")


# dplyr, tidyr & ggplot2 packages and BPRSL are available

# Number of weeks, baseline (week 0) included
n <- BPRSL$week %>% unique() %>% length()

# Summary data with mean and standard error of bprs by treatment and week 
BPRSS <- BPRSL %>%
  group_by(treatment, week) %>%
  summarise( mean = mean(bprs), se = sd(bprs)/sqrt(n) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSS)

# A very nice picture! i think going to be useful.

# Plot the mean profiles
ggplot(BPRSS, aes(x = week, y = mean, linetype = treatment, shape = treatment)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)")


# dplyr, tidyr & ggplot2 packages and BPRSL are available

# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
BPRSL8S <- BPRSL %>%
  filter(week > 0) %>%
  group_by(treatment, subject) %>%
  summarise( mean=mean(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL8S)

# Draw a boxplot of the mean versus treatment
ggplot(BPRSL8S, aes(x = treatment, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")

# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
BPRSL8S1 <- BPRSL8S %>%
  filter(mean < 65) 

# dplyr, tidyr & ggplot2 packages and BPRSL8S & BPRSL8S1 data are available

# Perform a two-sample t-test
t.test(mean ~ treatment, data = BPRSL8S1, var.equal = TRUE)

# Add the baseline from the original data as a new variable to the summary data
BPRSL8S2 <- BPRSL8S %>%
  mutate(baseline = BPRS$week0)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + treatment, data = BPRSL8S2)
summary(fit)

# Compute the analysis of variance table for the fitted model with anova()
# this shows about the same as summary(fit)
anova(fit)

########################################################
# read the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

# Factor variables ID and Group
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# Glimpse the data
glimpse(RATS)


# dplyr, tidyr and RATS are available

# Convert data to long form
str(RATS)
RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 

# Glimpse the data
glimpse(RATSL)

# dplyr, tidyr and RATSL are available

# Check the dimensions of the data
dim(RATSL)

# Plot the RATSL data
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line()

# dplyr, tidyr, RATS and RATSL are available

# create a regression model RATS_reg
RATS_reg <- lm(Weight ~ Time + Group, data = RATSL)

# print out a summary of the model
summary(RATS_reg)

# dplyr, tidyr, RATS and RATSL are available

# access library lme4
library(lme4)

# Create a random intercept model
RATS_ref <- lmer(Weight ~ Time + Group + (1 | ID), data = RATSL, REML = FALSE)

# Print the summary of the model

# dplyr, tidyr, lme4, ggplot2, RATS and RATSL are available

# create a random intercept and random slope model
RATS_ref1 <- lmer(Weight ~ Time + Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref1)

# perform an ANOVA test on the two models

anova(RATS_ref1, RATS_ref)

# dplyr, tidyr, lme4, ggplot2, RATS and RATSL are available

# create a random intercept and random slope model with the interaction
RATS_ref2 <- lmer(Weight ~ Group + Time + (Time|ID) +Time*Group, REML= FALSE, data= RATSL)

# print a summary of the model
summary(RATS_ref2)

# perform an ANOVA test on the two models
anova(RATS_ref2, RATS_ref1)

# draw the plot of RATSL with the observed Weight values
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Observed weight (grams)") +
  theme(legend.position = "top")

# Create a vector of the fitted values
Fitted <- fitted(RATS_ref2)
help(fitted)
# Create a new column fitted to RATSL
RATSL <- RATSL %>%
  mutate(Fitted)

# draw the plot of RATSL with the Fitted values of weight
ggplot(RATSL, aes(x = Time, y = Fitted, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted weight (grams)") +
  theme(legend.position = "top")
