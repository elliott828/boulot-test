#################################
## 2. Advanced Operation       ##
## 2.4 Linear Regression Model ##
## (Basic Plotting included)   ##
#################################

# Data source: baseball
# data from all 30 major league baseball teams
# examine the linear relationship between runs scored in a season 
# and a number of other player statistics

load("baseball.RData")
str(mlb11)

# load the introduction of dataset
read.csv("mlb11_intro.csv") -> intro
intro[1,1:3]  # definition of "runs"

#-------------#
# Correlation #
#-------------#
# cor(x, y) / cor(matrix/data frame of numeric variables)
# measure the strength of linear relationship between/among variables

cor(mlb11$runs, mlb11$at_bats)
cor(mlb11$hits, mlb11$bat_avg)

# scatterplot: visualize the relationship between 2 variables
plot(mlb11$at_bats, mlb11$runs)
# relationship between at bats and runs is positive, linear (moderately strong)
# 1 of the potential outliers is a team with around 5520 at bats
plot(mlb11$hits, mlb11$bat_avg)
# strong positive linearity

# correlation among 3+ variables
cor.total <- cor(mlb11[2:ncol(mlb11)])
cor.total[1:5,1:5]

#=====================================================================
#--------------#
# Linear Model #
#--------------#
# model = lm(formula, data)
# a formula takes the form y~x; 
# y is response variable, x is explanatory variable
# more than 1 explanatory variable: y ~ x1 + x2 + x3 +...
# formula with all independent variables: y ~ .

# recall a linear model: Y = a0 + a1*x1 + a2*x2 + ...
# runs: scored when a player advances around first, second 
#       and third base and returns safely to home plate, 
#       touching the bases in that order, before three outs are recorded
# at_bats: a batter's turn batting against a pitcher
fit1 <- lm(runs ~ at_bats, data = mlb11) 
fit1
# runs = -2789.2429 + 0.6305 * at_bats
# for every additional at bat, the model predicts 0.63 more runs on average

summary(fit1) -> resume
resume
# key parameters
# 1. Residuals: point observed - point estimated
# 2. Pr(>|t|): p-value. significance level of the coefficients of variables
# 3. Multiple R-squred: % variance explained by the model
# 4. p-value of F-statistic: significance level of model fitness

#---------------------------------------------------------------------
# plot models
plot(mlb11$runs ~ mlb11$at_bats)
# compare with plot(mlb11$runs, mlb11$at_bats)
abine(fit1)

#---------------------------------------------------------------------
# elements of summary(fit1)
str(resume)
resid <- resume$residuals # residuals of model fit1

resume$coefficients  # matrix of coefficients and relative statistics
resume$r.square      # r square of model fit1

# To assess whether the linear model is reliable:
# 1. linearity
# 2. nearly normal residuals
# 3. constant variability
#---------------------------------------------------------------------
# plot residuals
# Method 1
plot(resid, ylab = "Residuals", xlab = "Observations",
     main = "Residuals of fit1")
abline(a = 0, b = 0) # add the horizontal line at point 0
# a: intercept; b: slope

# Method 2
plot(resid ~ mlb11$at_bats)
abline(h = 0,lty = 3)
# h: y-value for horizontal line
# lty: line type

# Method 3 - qqplot()
# quantile-quantile plots
qqnorm(fit1$residuals)
qqline(fit1$residuals)

# Method 4 - hist()
# histogram
hist(fit1$residuals)

# residuals are supposed to distributed normally/randomly around 0.
# otherwise the model is not good (i.e.: mostly under 0 or mostly above 0)
# the residuals are right skewed (long tail at right side), 
# hence the normal distribution of residuals condition is not met.

#---------------------------------------------------------------------
# to find the best predictor
# do the modeling for each of the independent variables
r.sq <- function(pred){
  fit <- lm(as.formula(sprintf("%s ~ %s","runs", pred)),mlb11)
  return(round(summary(fit)$r.square,3))
}

sort(sapply(names(mlb11[3:12]),r.sq), decreasing = T)
# the best traditional predictor is bat_avg: batting average
# the best new predictor is new_obs: on base rate + slugging rate

#=====================================================================
#-----------------------#
# Multiple Linear Model #
#-----------------------#
# Is the best predictor explaining everything?
# mtcars dataset

?mtcars

# Plotting based on variable types

# scatterplot between Miles/gallon and Gross horsepower
# plot between 2 numeric variables
plot(mtcars$mpg ~ mtcars$hp)

# side-by-side boxplot for Miles/gallon and transmission method
# categorical variable: transmission  (0 = auto; 1 = manual)
# plot between a numeric and a categorical variables
boxplot(mtcars$mpg ~ mtcars$am)

# mosaic plot for cylinder number and transmission method
# plot between 2 categorical variables
mosaicplot(mtcars$cyl ~ mtcars$am)

#---------------------------------------------------------------------
# build up simple model
#------------------#
# Stepwise forward #
#------------------#
# find out the best predictor
r.sq.bis <- function(pred){
  fit <- lm(as.formula(sprintf("%s ~ %s","mpg", pred)),mtcars)
  return(round(summary(fit)$r.square,3))
}

sort(sapply(names(mtcars[2:ncol(mtcars)]),r.sq.bis), decreasing = T)

# begin modeling by wt (weight of a car)
fit2 <- lm(mpg ~ wt, data = mtcars)
# fit2 <- lm(mtcars$mpg ~ mtcars$wt)
summary(fit2)
# p-value = 1.29e-10, wt is a significant predictor

plot(fit2)
abline(fit2)

# Residual Plot (Residuals vs. Fitted)
# QQ Plot (Normal Q-Q)
# Scale - Location
# Leverage - mark the outliers which has obvious leverage effect on model result

plot(mtcars$mpg ~ mtcars$wt)
abline(fit2)

# Always choose a predictor:
# 1. which improves the model best
# 2. which is significant
# taking into account cyl (no. of cylinder)
fit2.1 <- lm(mpg ~ wt + cyl, data = mtcars)
summary(fit2.1)
# p-value of cyl is 0.001064, significant predictor
# R-square = 0.8302

# taking into account disp (displacement)
fit2.2 <- update(fit2, ~.+ disp)
# if: fit2 <- lm(mtcars$mpg ~ mtcars$wt)
# then: IT'S OBLIGED TO SPECIFY data source
#       fit2.2 <- update(fit2, ~.+ disp, data = mtcars) 
summary(fit2.2)
# p-value of disp is 0.06362, a significant predictor
# R-square = 0.7809, lower than previous model fit2.1

# try hp
fit2.3 <- update(fit2, ~.+ mtcars$hp)
summary(fit2.3)
# p-value of hp is 0.00145, significant predictor
# R-square = 0.8268, still lower than fit2.1
# hp is more or less good, we can continue trying other variables...

# finally
fit.forward <- lm(mpg ~ wt + cyl + hp, data = mtcars)
summary(fit.forward)
# R-squre = 0.8431; Model p-value = 2.184e-11

#---------------------------------------------------------------------
#-------------------#
# Stepwise backward #
#-------------------#
# we put all variables in the model
fit3 <- lm(mpg ~ ., mtcars)
summary(fit3)
# p-value of cyl is the highest: 0.9161

# Always choose the variable to eliminate
# 1. with the highest p-value
# 2. without which the model has the best fitness
# 3. until all predictors become significant
fit3.1 <- update(fit3, ~. -cyl)
summary(fit3.1)
# R square = 0.8689
fit3.2 <- update(fit3.1, ~. -vs)
summary(fit3.2)
# R square = 0.8687
# fit3.1
# and we continue testing...

# finally
fit.backward <- lm(mpg ~ wt + qsec + am, data = mtcars)
# R-square = 0.8497; Model p-value = 1.21e-11

#---------------------------------------------------------------------
#----------------------------------------#
# Comparison between results of 2 models #
#----------------------------------------#
# Why 2 approaches lead different models?
# 1. sometimes they do lead to a same model
# 2. stepwise modeling from different start points possibly lead to different result
#    because the impact (to a model) of removing a predictor and inserting one
#    is totally different.
#    Imagine that you go downhill from different direction, you will always reach
#    the foot of the hill, but the destination might not be of the same height 
# 3. The result of the 2 algorithm would be quite similar (if they are not equal)

#---------------------------------------------------------------------
#---------------------#
# Optional: stepAIC() #
#---------------------#
# automatically execute stepwise regression
library(MASS)
fit.1 <- stepAIC(fit2, scope = ~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, 
                 direction = "forward")
fit.2 <- stepAIC(fit3, trace =F)

#---------------------------------------------------------------------
# Session 2.4 Review
# - Correlation
#   * cor()
#   * Correlation between 2 variables
#   * Correlation among 3+ variables
# - Linear model
#   * Build a model: lm(y~x, data)
#   * Print the model
#   * Model summary
#     ~ key statistics
#     ~ key elements of summary - how to extract them
#   * How to tell the reliability of a model
# - Multiple linear regression
#   * Stepwise forward
#   * Stepwise backward
#   * update(model, formula)
#   * How to explain the difference between the different results of 
#     2 stepwise regression methods
# - Model plotting
#   * Plot a model
#   * 4 methods to plot residuals
#   * Plotting based on variable types
#     ~ Scatter plot
#     ~ Box plot
#     ~ Mosaic plot
#     ~ Histogram
# - Optional: stepAIC()
#---------------------------------------------------------------------

# finished on Thu. 11/3/2014
