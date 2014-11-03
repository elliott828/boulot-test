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
r.square <- function(pred){
  fit <- lm(as.formula(sprintf("%s ~ %s","runs", pred)),mlb11)
  return(round(summary(fit)$r.square,3))
}

sapply(names(mlb11[3:12]),r.square)
# the best traditional predictor is bat_avg: batting average
# the best new predictor is new_obs: on base rate + slugging rate

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
# - Model plotting
#   * Plot a model
#   * 4 methods to plot residuals
#---------------------------------------------------------------------

# finished on Thu. 11/3/2014
