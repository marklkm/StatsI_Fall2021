# Problem Set 4
# Due on November 26, 2021.
# Applied Stats/Quant Methods 1

install.packages("tidyverse")
library(ggplot2)
library(plyr) 
library(stargazer)
# plyr package to help recode the categorical variable type
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
# The Prestige data frame has 102 rows and 6 columns. 
# The observations are occupations.
# The 6 columns are:
# "education" "income" "women" "prestige" "census" "type"

str(Prestige)
# sumamry is a simple regression
# and enables you to view regression model output
summary(Prestige) 
attach(Prestige)
names(Prestige)

######### Question 1: Economics #########
# We would like to study whether individuals with higher levels of income have more
# prestigious jobs. Moreover, we would like to study whether professionals have more
# prestige jobs than blue and white collar workers. 

# (a) Create a new variable 'professional' by recoding the variable 'type'
# so that 'professionals' are coded as 1, and blue and white color workers
# are coded as 0 (Hint: ifelse)

# This calls for a dummy variable
# A dummy variable is a type of variable that we create in 
# regression analysis so that we can represent a categorical variable 
# as a numerical variable that takes on one of two values: zero or one.
# Create the dummy variable with ifelse() function



# create new variable 'professional'
# recode the variable 'type'
# prof to "1"
# bc to "0" or blue collar
# wc to "0" or white collar

## referenced https://www.statology.org/dummy-variables-in-r/
# ifelse() function to define dummy varaiables

#Prestige$professional <- mutate(Prestige, prof = type)

Prestige$professional <- ifelse(Prestige$type == 'prof', 1, 0)
str(Prestige)


# (b) Run a linear (lm) model with 'prestige' as an outcome and 'income', 'professional'
# and the interaction of the two as predictors (Note: this is a continous X dummy
# interaction)

# prestige depends on income
prof_rg <- lm(prestige ~ income + professional + income * professional, data = Prestige)
summary(prof_rg)

##
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         21.1422589  2.8044261   7.539 2.93e-11 ***
# income               0.0031709  0.0004993   6.351 7.55e-09 ***
# professional        37.7812800  4.2482744   8.893 4.14e-14 ***
# income:professional -0.0023257  0.0005675  -4.098 8.83e-05 ***


# (c) Write the prediction equation based in the result 

# intercept
# y (hat) ŷ  = 21.1422589 + 37.7812800D +  0.0031709x - 0.0023257xD + e



# (d) Interpret the coefficient for income variable

# income coeInfficient = 0.0031709
# and additional dollar for income will mean there is a 0.0031709 increase 
# in the level of prestige 

# (e) Interpret the coefficient for professional
# similiarly the professional coefficient is 37.7812800 relates to the level of
# prestige associated with a professional worker or a non blue and white collar
# professional 

# (f) What is the effect of a $1,000 increase in income on prestige score for
# professional occupation? In other words, we are interested in the marginal
# effect of income when the variable 'professional' takes the value of 1. 
# Calculate the change of ŷ associated with a $1,000 increase in income
# based on your answer for (c).

# for additional/increase of 1000 in income results in 
# 0.0031709 x 1000 -0.0023257 x 1000 = 0.8452 
# professional increase by 1000
# profession:income increase by 1000
# prestige points now show an increase of 0.8452 for an additional increase of $1,000

# (g) What is the effect of changing one's occupation from non-professional 
# to professional when her income if $6,000? We are interested in the marginal effect
# of professional jobs when the variable 'income' takes the value of $6,000.
# Calulate the change of y (hat) based on your answer for (c).

# more or less the same for (f) .. but non-professional
# income for professional + income professional 
# income takes the value of 6000
# 37.7812800 -0.0023257 x 6000 = 23.82708




##TEST
#Prestige$code[data$type=="prof"] <- "1"
#Prestige$code[data$type=="bc"] <- "0"
#Prestige$code[data$type=="wc"] <- "0"
#Prestige$code <- factor(Prestige$code)
#Prestige
##TEST

######### Question 2: Political Science #########

# (a) Use the results from a linear regression to determine whether having
# these yard signs in a precinct affects vote share (e.g., conduct a hypothesis
# test with α = .05 which is the significance level in hypothesis test)

# hypothesis test 0.05
# Null hypothesis H0
# Alternative hypothesis Ha is 𝛽1 
# 𝛽̂ (hat) is the estimator of 𝛽


# t-test is testing for a non-linear relationship
# t = 𝛽̂j / se 𝛽̂j where𝛽̂is the estimated beta and se is standard error
#student t-distribution 

# gives 0.042 / (0.016) = 2.625

# N = 131
pt(2.625, df = 131)




