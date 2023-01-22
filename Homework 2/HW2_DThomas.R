#
# Author: DeAndre Thomas
# Prof. Jason Anastasopoulos
# Class: IST 772
# Purpose: Homework 2
# Date: 04/22/2022
#
################################################################################

# Question 1

table(rbinom(n=1, size=9, prob=0.5))
# If we flip one (fair) coin 9 times, the coin landed on heads 5 out 
# of 9 times. 

n<-table(rbinom(n=100000, size=9, prob=0.5))
n/100000
# If we are performing the above event, 100,000 times, the trial displays that
# the coin will land on heads roughly 50% of the time when flipped. 

################################################################################

# Question 2

barplot(n)
# This barplot of demonstrated the the results of flipping coin 100,000 times. 

barplot(n/100000)
# This barplot demonstrates the probability of the above data. 

# Both plots demonstrate normal distribution. These plots demonstrate that
# flipping a coin 9 times, there is  great chance (roughly 50% chance) 
# that the coin will land on heads. These plots are also demonstrating 
# that there is a it is least likely to land on heads 0 or 9 times out of 9. 

################################################################################

# Question 6

statsT<- matrix(c(47, 33, 3, 17), ncol= 2, byrow = TRUE)
colnames(statsT)<- c('College', 'High School')
rownames(statsT)<- c('Pass', 'Fail')
statsT<- as.table(statsT)
statsT

# We were able to create this table because the sample size provided was 100. 
# 50 of those students were in High School and the other 50 were in college. 
# Of this amount, per the information provided, we knew that 20 students 
# failed and 3 of those failed tests were of college students. Thus, 
# there were 47 passing students in college. Also, there were 17 failing 
# tests from high school, leaving 33 passing test from high school students. 

statsP<- statsT/margin.table(statsT)
statsP

# If we focus solely on high school students, high school students have a 66% 
# chance of passing and a 34% chance of failing. Here, we multiplied the 
# probability values in the High School column by 2, to make those values 
# total 100%. 

################################################################################

# Question 7

repoT<- matrix(c(0, 93935, 69, 5996), ncol = 2, byrow = TRUE)
colnames(repoT)<- c('Repossessed', 'Non-Repossessed')
rownames(repoT)<- c('Pass', 'Fail')
repoT<- as.table(repoT)
repoT

repoP<- repoT/margin.table(repoT)
repoP

# 93.9% of customers both pass the test and do not have their home 
# repossessed.

################################################################################

# Question 8

repoP[2,]/sum(repoP[2,])

# There is a 1% chance that this customer will default on their mortgage. 