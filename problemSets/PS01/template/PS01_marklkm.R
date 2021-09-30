#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
#setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")
setwd("/Users/mark/documents/ASDS-applied-stats-2021/StatsI_Fall2021")

#####################
# Problem 1 = My Answer
#####################

# Because the sample or n < 30 a t-distribution can be used as opposed to a normal distribution
#  t-distribution is a type of probability distribution. used where the sample size is small.

# The sample size is 25
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# get the sample mean ( x bar) of y which is 98.4
n <- length(y) 
# sample mean
mean(y)
# standard deviation
sd(y)
(sd(y)/sqrt(n))
# CI of 90%
# From tables look for Degree of Freedom or DF of 24 and alpha level of 0.05
# this gives 1.71 or a t-score of 1.71
# 1. 25 - 1 = 24
# 2. 1 - .90 = .05 or the alpha level
# use alpha level of 0.05
c90 <- qt(.05, 24, lower.tail = FALSE)
c90
# CI tells us to take the mean of 98.44 and plus or minus 4.4778
lower_lvl <- mean(y) - c90*(sd(y)/sqrt(n))
upper_lvl <- mean(y) + c90*(sd(y)/sqrt(n))
# get the s or standard deviation of y which is 13.09287
#sd(y)
# Using a 90% confidence level and need to find the confidence interval
# Confidence Level or CI is the margin of error and written as ±
# CI is to do with how reliable the estimation is
# 1.71 * 13.09287 / square root of 25 = 4.4778
# 98.44 - 4.778 = 93.96 is the lower level
# 98.44 + 4.778 = 102.92 is the upper level
c(lower_lvl, upper_lvl)


# t test can be used for small samples
t.test(y)





#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
str(expenditure)
# rows and columns 2 to 5
expenditure[2:5]

# using pairs() a matrix of scatter plots is produced
pairs(expenditure[2:5], main = "Correlation Matrix shown as a scatter plot")
# This shows that it is not correlated and is random

# Housing assistance and per capita expenditure / income
scatter.smooth(expenditure$Y, expenditure$X1, ylab="per capita income", xlab = "housing assistance")
# cor function calculates correlation among the vectors
cor(expenditure$Y, expenditure$X1)
scatter.smooth(expenditure$Y, expenditure$X2)
cor(expenditure$Y, expenditure$X2)
scatter.smooth(expenditure$Y, expenditure$X3)
cor(expenditure$Y, expenditure$X3)
scatter.smooth(expenditure$X1, expenditure$X2)
cor(expenditure$X1, expenditure$X2)
