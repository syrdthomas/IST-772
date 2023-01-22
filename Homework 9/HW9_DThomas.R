#######################################################################
#
# Author: DeAndre Thomas
# Professor: Dr. Anastasopoulos
# Purpose: Homework 9
# Class: IST 772
# Date: 06/10/2022
#
#######################################################################

# Question 1
?mtcars
dfc<- mtcars
glmOut<- glm(formula = vs ~ gear + hp
             , family = binomial(link = "logit"), data = dfc)
summary(glmOut)

# In predicting the engine shape while utilzing the horsepower
# and gear as independent variables, we would failt to reject
# the null hypothesis that one cannot determine the shape of an
# engine based on horsepower and gear variables. The intercept 
# (p-value = 0.06) of the model and gear variable 
# (p-value = 0.39) have p-values higher than 0.05, meaning that
# we fail to reject the null hypothesis. However, hp has a 
# p-value of 0.01, possibly indicating that horsepower alone 
# may be a determinant in engine shape.

#######################################################################

# Question 5

# install.packages("BaylorEdPsych")
# library(BaylorEdPsych)
# PseudoR2(glmOut)
# I successfully installed the BaylorEdPsych package, but could not 
# install the BaylorEdPsych library. Thus, I could not run line 34. 

install.packages("https://cran.r-project.org/src/contrib/Archive/BaylorEdPsych/BaylorEdPsych_0.5.tar.gz", repos=NULL, type="source")
library(BaylorEdPsych)
PseudoR2(glmOut)

# The PseduoR2 provides simlar information at the R-squared value 
# would for a linear model in categorical models. The McFadden 
# score (0.63), Cox.Snell score (0.42), and Nagelkerke score (0.78)
# all indicate that the model is strong. 

#######################################################################

# Question 6
install.packages("car")
library(car)
data(Chile)
dfChile <- data.frame(Chile)
ChileN <- dfChile[dfChile$vote =='N',]
ChileY <- dfChile[dfChile$vote =='Y',]
ChileYN <- rbind(ChileY, ChileN)
ChileYN <- ChileYN[complete.cases(ChileYN),]
ChileYN$vote <- factor(ChileYN$vote, levels = c('N','Y'))
chiLM <- glm(vote ~ age + statusquo, family = binomial(), data = ChileYN)
summary(chiLM)

install.packages("MCMCpack")
library(MCMCpack)
ChileYN$vote <- as.numeric(ChileYN$vote) - 1
chiBayes <- MCMClogit(formula = vote ~ age + statusquo, family = binomial(), data = ChileYN)
summary(chiBayes)

# The Logistic Model and Bayes model indicate that the status quo is
# the best predictor. The p-value of the logistic regression model 
# (2e-16) is signficantly lower than the typical alpha level. The Bayes
# model supports the logistic mode in that the status quo's HDI ranges 
# from 2.9-3.5. Being so, one can suggest that this is a strong predictor 
# in the outcome of ones vote. 

#######################################################################

# Question 7

statusQuoLogOdds <- as.matrix(chiBayes[,"statusquo"])
statusOdds <- apply(statusQuoLogOdds,1,exp)
hist(statusOdds)
abline(v=quantile(statusOdds,c(.025)),col='black')
abline(v=quantile(statusOdds,c(.975)),col='black')
