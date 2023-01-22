#######################################################################
#
# Author: DeAndre Thomas
# Professor: Dr. Anastasopoulos
# Purpose: Homework 10
# Class: IST 772
# Date: 06/17/2022
#
#######################################################################

# Question 2

library(car) 
# install.packages("Rtools")
# install.packages("nlme")
# library(nlme)

data("Blackmore")
dfB<- data.frame(Blackmore)

dfB$age <- round(dfB$age)
summary(dfB)

boxplot(exercise~age, data = dfB)

Bsub <- dfB[dfB$age <= 12,]
Bsub$ageFact <- as.factor(Bsub$age)
list <- rowSums(table(Bsub$subject,Bsub$ageFact))==3
list <- list[list == TRUE]
list <- as.numeric(names(list))

summary(Bsub[Bsub$ageFact == 8,])

summary(Bsub[Bsub$ageFact == 10,])

summary(Bsub[Bsub$ageFact == 12,])

Bsub <- Bsub[Bsub$subject %in% list,]
summary(aov(exercise~ageFact+ Error(subject), data = Bsub))

#######################################################################

# Question 5

install.packages("changepoint")
library(changepoint)

data("AirPassengers")
Adf<- data.frame(AirPassengers)
dfA<- diff(AirPassengers)
plot(dfA)

cpt.var(dfA)
plot(cpt.var(dfA))

# The diff() function evaluates the difference between the most 
# current month's value and previous months' values. The plot() 
# function provides a visual of the diff() function's results, and
# the cpt.var() function draws a red line  where the mean or 
# average value of difference between months changes significantly. 
# This plot in particular states that the most significant change
# in average difference occurs in 1955. 

#######################################################################

# Question 6

ADiffMean<- cpt.mean(dfA, class = FALSE)
plot(ADiffMean)

# This plot tells us from the beginning of this study to the end, 
# there has been a decrease of passengers. In addition, the 
# the change point of the mean occurred. Thus, after 1955, there 
# was a significant decrease in Air Travel passengers. 

#######################################################################

# Question 7

# The world of aviation was began in 1903 when the Wright Brothers 
# took flight for the first time. As time progressed, advancements
# in aviation progressed to aviation becoming an integral part of 
# transportation, military defense, and commerce. With the 
# convenience came a great price for a ticket, especially during the 
# 1950s and 1960s, also known as the Golden Age of Flying. Also, 
# during the Golden Age of Flying, there were many recordings of air
# craft crashes and such significant turbulence that it could cause 
# bodily damage. 
# Ref: https://www.fastcompany.com/3022215/what-it-was-
# really-like-to-fly-during-the-golden-age-of-travel

#######################################################################

# Question 8

install.packages("bcp")
library(bcp)

Abcp<- bcp(as.vector(dfA))
plot(Abcp)

# The Bayesian plot creates similar results to those previously 
# computed. The posterior means and posterior probability plots
# show a shift near the 80th location. This location 
# represents the change-point in the means of the time series,
# which correlate with the change point in 1955. 