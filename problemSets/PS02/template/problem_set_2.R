
# Applied Stats I 
# Problem Set 2
# Mark Likeman - 19312796
# ASDS - TCD

#install 'tidyverse package and ggplot2 library

install.packages("tidyverse")
library(ggplot2)

# built in data sets
?data()

data("df")
#######################################
# Question 1: Political Science
# (a) Calculate the x^2 statistic

# create a table in R for the given data

data_table <- matrix(c(14, 6, 7, 7, 7, 1), ncol=3, byrow=TRUE)
colnames(data_table) <- c('NotStopped','BribeRequested','StoppedGivenWarning')
rownames(data_table) <- c('UpperClass','LowerClass')
data_table <- as.table(data_table)
data_table

barplot(height = data_table, 
        beside = TRUE, 
        legend.text = TRUE,
        args.legend = list(x = "topleft", cex = 0.4, box.col = "purple"))

# build the dataset for the table

df <- data.frame (first_column  = c("NotStopped"),
                  second_column = c("BribeRequested"),
                  third_column = c("StoppedGivenWarning"))

# Using the chisq.test to get the x^2 test statistic or the chi square statistic
# x^2 = 3.7912
# DF = 2 (or n-1)
# value of alpha = .1 (for part b)

# (b) Now calculate thep-value from the test statistic you just created (in R).
# What do you conclude if alpha = .1

chisq <- chisq.test(data_table)
chisq
tail(df, 2)


# x^2 = 3.7912
# DF = 2
# alpha value = .01
# critical value = 1.885618
# x^2(3) = 3.7912, p < 0.1 (the probability was set at 0.1)

# Find the Critical Value
qt(p=0.1, df=2, lower.tail = FALSE)
# This gives a critical value of 1.885618

# The p-value is 0.1502 and the alpha value = 0.1
# This concludes that alpha value 0.1 is less than the p-value of 0.1502
# X^2 > critical value 
# reject the Null hypothesis


# (c) Calculate the standardized residual for each cell and put them in the
# table below

chisq$residuals
# this gives
#             NotStopped BribeRequested StoppedGivenWarning
# UpperClass  0.1360828     -0.8153742           0.8189230
#LowerClass  -0.1825742      1.0939393          -1.0987005

# (d) How might the standardized residuals help you interpret the results?

# to help identify outliers 


#######################################
# Question 2: Economics
# (a) State a null and alternative (two-tailed) hypothesis

west_bengal <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
attach(west_bengal)
west_bengal
summary(west_bengal)
str(west_bengal)
 
# H0 is the null hypothesis
# Ha is the alternative hypothesis

# H0: the reservation policy has no effect on the number of new or repaired drinking
# water facilities in the villages
# Ha: the reservation policy does have an effect on the number of new or repaired drinking
# water facilities in the villages


# (b) Run a bivariate regression to test this hypothesis
# prediction - for the data = women
# water depends/extends on reserved
res_policy <- lm(water ~ reserved, data = women)
summary(res_policy)

# this gives
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   14.738      2.286   6.446 4.22e-10 ***
# reserved       9.252      3.948   2.344   0.0197 *

# and p-value of 0.0197
# F-statistic: 5.493 on 1 and 320 DF,  p-value: 0.0197

# taking an alpha of 5% 
# alpha value = .05
# p-value = .0197
# the p-value is less than the alpha value that I am trying to test, 
# so I can reject the null hypothesis

# (c) Interpret the coefficient for reservation policy
# when measuring the correlation between two variables is trying to put a
# number on their association
plot(water ~ reserved, data = women)
abline(lm(water ~ reserved, data = women), col = "blue")



# Question 3: Biology

# 1. Import the data set and obtain summary statistics and examine the distrubtion of
# the overall lifespan of the fruitflies.

# import data set
dat<-read.csv("http://stat2.org/datasets/FruitFlies.csv")
# or 
dat<-read.table("http://stat2.org/datasets/FruitFlies.csv", sep=',', header=TRUE)
attach(dat)
dat
# can also get the descriptive statistics in R using Hmisec package
install.packages("Hmisc")
library(Hmisc)
describe(dat)

# 2. Plot lifespan vs thorax. 
plot(Longevity~Thorax, xlab="Thorax",
     main="Lifespan", ylab= "in days")

boxplot(Longevity~Treatment, data=dat, frame =FALSE)
summary(dat)

qplot(Sleep, Longevity, data=dat, color=factor(Treatment))+
  labs(title="Longevity and sleep by treatment")

# The correlation coefficient between the two variables Longevity (or lifespan) and thorax
# using the Pearson correlation
cor(dat$Longevity, dat$Thorax)
# the correlation deficit between the two variables lifespan vs thorax is 0.6364835

# this gives a summary of rhe statistics of the data
summary(dat) 
# examines the distrubution of the overall lifespan of the fruitflies
summary(dat$Longevity)

# 3. Regress lifespan on thorax. Interpret the slope of the fitted model


lm(dat$Longevity ~ dat$Thorax, data=dat)
# this gives:
# Call:
# lm(formula = dat$Longevity ~ dat$Thorax)

# Coefficients:
# (Intercept)   dat$Thorax  
# -61.05       144.33 

# 144.3 gives the slope of the line 

abline(lm(dat$Longevity ~ dat$Thorax), col = "red")


# 4. Test for a significant linear relationship between lifespan and thorax. Provide
# and interpret your results.

# 5. Provide the 90% confidence interval for the slope of the fitted model
# DF 123
qt(.05, 123, lower.tail = FALSE)

#Coefficients:
#  (Intercept)   dat$Thorax  
#  -61.05       144.33 



# 6. 


# 7. 
