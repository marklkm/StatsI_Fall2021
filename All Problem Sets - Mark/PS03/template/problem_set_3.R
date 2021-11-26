# Applied Stats I 
# Problem Set 3
# Mark Likeman - 19312796
# ASDS - TCD

#install 'tidyverse package and ggplot2 library
# regression is widely used for prediction and forcasting
install.packages("tidyverse")
library(ggplot2)

#######################################
# Question 1: We are interested in knowing how the difference in campaign spending between
# incumbent and challenger affects the incumbent's vote share.

# 1. run a regression where the outcome variable is 'voteshare' and the explanatory variable is 'difflog'
# voteshare - outcome variable is the 'dependent variable on Y'

# difflog - explanatory variable is the 'independent variable (predictor variable) on X' 
# unknown parameter θ
# the basic prediction equation expresses a linear relationship between an independent variable 
# or (x, predictor variable) and a dependent variable (y, a criterian variable)

incumbents <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv")
summary(incumbents) # this is a simple regression or summary(..)
str(incumbents)
attach(incumbents)
names(incumbents)


# difflog depends on voteshare ..
# q1 <- lm(difflog ~ voteshare, data = incumbents)
# summary(q1)
q1 <- lm(voteshare ~ difflog, data = incumbents)
summary(q1)

plot(difflog ~ voteshare, data = incumbents)
abline(lm(difflog ~ voteshare, data = incumbents), col = "blue")

### 2. Make a scatterplot of the two variables and add the regression line ####

# the ggplot plots the regression line
#q1_plot <- ggplot(incumbents, aes(difflog, voteshare)) +
  # geom_point(alpha = 0.5) + # add a scatterplot
#  geom_smooth(method = "lm") # add a linear regression line
# run the plot
#q1_plot

class(voteshare)
class(difflog)
plot(difflog, voteshare, main = "Scatterplot")
cor(difflog, voteshare) # x,y

# testing the incumbents_subset dataset
# scatter.smooth(x=incumbents$difflog, y=incumbents$voteshare, main="Scatter") 

# the ggplot plots the regression
q1_plot <- ggplot(incumbents, aes(difflog, voteshare)) +
  geom_point(alpha = 0.5) + # add a scatterplot
  geom_smooth(method = "lm", col="red") + # add a linear regression line
  labs(x = "difflog", y = "voteshare")
q1_plot

# using the lm() function for plotting the output

summary(lm(data = incumbents, difflog ~ voteshare))

### 3. Save the residuals of the model in a separate object

# plotting the residuals

q1_resid <- resid(q1) #a function for extracting the residuals from
#a model object
plot(incumbents$difflog, q1_resid)
abline(h = 0, col = "red")

q1resid <- q1$residuals
q1resid 

q1_resid <- resid(q1) #a function for extracting the residuals from
#a model object
plot(incumbents$voteshare, q1_resid)
abline(h = 0, col = "red")

### 4. Write the prediction equation

# (Intercept) 0.579031 and difflog  0.041666 (from the summary)
# x is the difference for challenger's spending
 # y (hat) = 0.579031 + 0.041666x 


# Question 2: 
# We are interested in knowing how the difference between incumbent and challenger's spending
# and the vote share of the presidential candidate of the incumbent's party are related.

## 1. Run a regression where the outcome variable is 'prevote' and the explanatory variable
# is 'difflog' 

# 'presvote' is the dependent variable (outcome variable) on Y axis
# ' difflog' is the independent variable (explanatory variable) on X axis

q2 <- lm(presvote ~ difflog, data = incumbents)
summary(q2)
plot(difflog ~ presvote, data = incumbents)
abline(lm(difflog ~ presvote, data = incumbents), col = "blue")


## 2. Make a scatterplot of the two variables and add the regression line

# the ggplot plots the regression
q2_plot <- ggplot(incumbents, aes(difflog, presvote)) +
  geom_point(alpha = 0.5) + # add a scatterplot
  geom_smooth(method = "lm") + # add a linear regression line
  labs(x = "difflog", y = "presvote")
q2_plot

## 3. Save the residuals of the model in a separate object.

q2_resid <- resid(q2) #a function for extracting the residuals from
#a model object
plot(incumbents$voteshare, q2_resid)
abline(h = 0, col = "red")

q2resid <- q2$residuals
q2resid

## 4. Write the prediction equation



### Question 3.

# We are interested in knowing how the vote share of the presidential candidate of the 
# incumbent's party is associated with the incumbent's electoral success. 

# 1. Run a regression where the outcome variable is 'voteshare' and the explanatory varaible'
# is 'presvote'

q3 <- lm(voteshare ~ presvote, data = incumbents)
summary(q3)
plot(presvote ~ voteshare, data = incumbents)
abline(lm(presvote ~ voteshare, data = incumbents), col = "blue")

# ## 2. Make a scatterplot of the two variables and add the regression line

# the ggplot plots the regression
q3_plot <- ggplot(incumbents, aes(presvote, voteshare)) +
  geom_point(alpha = 0.5) + # add a scatterplot
  geom_smooth(method = "lm") # add a linear regression line
labs(x = "presvote", y = "voteshare")
q3_plot


### 3. Write the prediction equation



####### Question 4.
# The residuals from part(a) tell us how much variation in 'voteshare' is not explained
# by the difference in spending between incumbent and challenger. The residuals in part (b)
# tell us how much the variation in 'prevote' is not explained by the difference in spending
# between incumbent and challenger in the district. 

## 1. Run a regression where the outcome variable is the residuals from Question 1 and 
# the explanatory variable is the residuals from Question 2.


# q1resid depends on q2resid
q4resid <- lm(q1resid ~ q2resid)
q4resid
summary(q4resid)


## 2. Make a scatterplot of the two residuals and add the regression line

q4_plot <- ggplot(aes(q2resid, q1resid), data = NULL) + # add the two residuals
  geom_point() + # add a scatterplot
  geom_smooth(method = "lm") # add the linear regression line
q4_plot


# 3. Write the prediction equation

# slope for q2resid is 2.569e-01 


##### Question 5.

## What if the incumbent's vote share is affected by both the president's popularity
# and the difference in spending between incumbent and challenger? 

## 1. Run a regression where the outcome variable is the incumbent's 'voteshare' and 
# the explanatory variables are 'difflog' and 'presvote'


q5resid <- lm(voteshare ~ difflog + presvote, incumbents)
# summary(lm(voteshare ~ difflog + presvote, incumbents))
summary(q5resid)

## 2. Write the prediction equation

# (Intercept) 0.4486442
# difflog     0.0355431
# presvote    0.2568770



## 3. What is it in the output that is identical to the output in Question 4? Why do you think this is 
# the case?

