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
# Problem 1
#####################

# Because the sample or n < 30 a t-distribution can be used as opposed to a normal distribution
#  t-distribution is a type of probability distribution. used where the sample size is small.

# The sample size is 25
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#length(y)
# get the sample mean ( x bar) of y which is 98.4
mean(y)
sd(y)
# get the s or standard deviation of y which is 13.09287
#sd(y)
# Using a 90% confidence level and need to find the confidence interval
# Confidence Level or CI is the margin of error and written as ±
# allowing for a 2.5%
# sample mean = 98.4
# standard deviation is 13.09287
# sample size is 25

# a for the sample mean
a <- 98.44
n <- 25
# s is for the standard deviation
s <- 13.09287

error <- qt(0.925, df=n-1)*s/sqrt(n)
# left
left<- a-error
# right
right <- a+error
left
right


# generating x coordinates
xpos <- seq(- 100, 100, by = 20) 

print ("X coordinates")
print (xpos)

# generating y coordinates using dt() method
# degreesoffreedom
degree <- 2
ypos <- dt(xpos, df = degree)

print ("Y coordinates")
print (ypos)

# plotting t distribution
plot (ypos , type = "l")

# Q1. part 2
# hypothesis test with alpha = 0.05
# probability of rejecting null hypothesis when it's True
# 0.05 is a 5% risk that there is a difference in IQs when ther is no difference
# in the avergae IQs among all schools in the country



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
str(expenditure)
unique(expenditure$mode)
# exp for expenditure
exp <- expenditure
head(exp)
group_mean <- aggregate(exp$Y, list(exp$STATE), mean)
group_mean <- aggregate(Y ~ STATE, data = df, mean)
group_mean
