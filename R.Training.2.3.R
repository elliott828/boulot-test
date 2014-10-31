#################################
## 2. Advanced Operation       ##
## 2.3 Fundemental Statistics  ##
#################################

#----------#
# Sampling #
#----------#
# sample(x, size, replace = FALSE, prob = NULL)

echantillon <- 1:10

# random permutation
sample(echantillon)

# get 5 samples from "echantillon"
sample(echantillon, 5)

# bootstrap resampling - sampling with replacement
sample(echantillon, 5, replace = T)
sample(echantillon, 20, replace = T) # sample size can be larger than size of x

#---------------------------------------------------------------------
# 50 bernoulli trials
bern <- sample(c(0,1), 100, replace = TRUE)
bern
table(bern)

# sampling with probability
p.dice1_2 <- 2/6
p.dice3_6 <- 4/6
dice <- sample(c("dice1_2", "dice3_6"), 600, replace = T, prob = c(p.dice1_2, p.dice3_6))
table(dice)

f.dice <- as.factor(sample(c("dice1_2", "dice3_6"), 6000, replace = T, prob = c(p.dice1_2, p.dice3_6)))
summary(f.dice)

#---------------------------------------------------------------------
# sample()'s surprise!
# guess the length of sampling result
sample(echantillon[echantillon >  8])
sample(echantillon[echantillon >  9])
sample(echantillon[echantillon >  10])

#=====================================================================

#--------------#
# Distribution #
#--------------#

# Normal Distribution
# rnorm(n, mean = 0, sd = 1)
# generates a normally distributed data serie
distr1 <- rnorm(1000, 5,5)
distr1
hist(distr1, breaks = 12)
# "breaks" defines the number of cells
# the number assigned to "breaks" is just a suggestion
# the breakpoints will be set to "pretty" values

#---------------------------------------------------------------------
# dnorm(x, mean = 0, sd = 1)
# gives the density
# standard normal distribution density function: f(x) = 1/sqrt(2*pi)*exp(-x^2/2)
# the prabability of and event happends AROUND point x
dnorm(0)
dnorm(0) == 1/sqrt(2*pi)
dnorm(1) == 1/sqrt(2*pi)*exp(-1/2)

#---------------------------------------------------------------------
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE)
# gives the accumulated distribution percentage
# integral of density
sigma = 1 # standard deviation
mu = 0 # mean

# The probability of observing negative numbers (continuous distribution, no discrete)
pnorm(mu)

# The probability of observing the data between (mu-sigma) and (mu+sigma)
one.sigma <- pnorm(mu+sigma) - pnorm(mu-sigma)
# and so on
two.sigma <- pnorm(mu+2*sigma) - pnorm(mu-2*sigma)
three.sigma <- pnorm(mu+3*sigma) - pnorm(mu-3*sigma)

one.sigma
two.sigma
three.sigma

# what's six sigma? :P

# lower.tail = FALSE
left.tail <- pnorm(mu-sigma)
right.tail <- pnorm(mu+sigma, lower.tail=F)
1-left.tail-right.tail == two.sigma

#---------------------------------------------------------------------
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE)
# returns the quantile of certain percentage
qnorm(0)
qnorm(0.2)
qnorm(0.5)
qnorm(0.8,lower.tail=F) == qnorm(.2)
qnorm(1)

#---------------------------------------------------------------------
# Comparison among different means and standard deviations
# Bell curves
set.seed(1)
x <- seq(-5,5,length.out=100)
y <- dnorm(x,0,1)

plot(x,y,col="red",xlim=c(-5,5),ylim=c(0,1),type='l',
     xaxs="i", yaxs="i",ylab='density',xlab='',
     main="The Normal Density Distribution")

lines(x,dnorm(x,0,0.5),col="green")
lines(x,dnorm(x,0,2),col="blue")
lines(x,dnorm(x,-2,1),col="orange")

legend("topright",legend=paste("mean=",c(0,0,0,-2)," sd=", c(1,0.5,2,1)), 
       lwd=1, col=c("red", "green","blue","orange"), cex = 0.8)

#---------------------------------------------------------------------
# Optional: Binomial, Poisson, t distribution
# Pre-fix of distribution functions:
# r- stands for "Randomly generates deviates"
# p- stands for "Percentage (accumulated)"
# d- stands for "density"
# q- stands for "quantile"

# rbinom(), ppois(), dt(), etc.

#=====================================================================
#----------------------#
# Confidence intervals #
#----------------------#

# Population vs. Sample
# Mean (avg. value of population) vs. Sample Mean (avg. value of sample)
# Standard Deviation (sd()) vs. Standard Error (sd()/size)

# i.e. mtcars - hp(horse power) (data extracted in 1974)
# suppose mtcars is a sample randomly picked up from the whole population of cars
avg.hp <- mean(mtcars$hp)
se.hp <- sd(mtcars$hp)/sqrt(nrow(mtcars))
# nrow(mtcars) == length(mtcars$hp)

# Suppose we want to set up a confidence interval at the level of 95%
higher95 <- avg.hp + 1.96 * se.hp
lower95 <- avg.hp - 1.96 * se.hp
# then the 95% confidence interval is
ci95 <- c(lower95, higher95)
ci95
# it means we are 95% confident that the true avg. horse power of all American cars
# is between 123 and 170

#---------------------------------------------------------------------
# where does the "1.96" come from?
# recall one.sigma, two.sigma & three.sigma
one.sigma #pnorm(mu+sigma) - pnorm(mu-sigma)
cat(round(one.sigma*100,2), "%", sep="")
# The probability of observing the data between (mu-sigma) and (mu+sigma) is
# 68.27%

# what if you want to increase the probability to 95%?
# increase the coefficient of sigma
# 95% confidence level means 5% significance level (inconfidence level)
# ~ means 2.5% inconfidence level at both sides of the bell curve
qnorm(0.025, lower.tail = F)
coef95 <- round(qnorm(0.025, lower.tail = F),2)
coef95

coef95 == abs(round(qnorm(0.025),2))

# for the calculation between coefficient(Z value) and probability
# we always use stardard normal distribution law

#---------------------------------------------------------------------
# to make an even stricter confidence interval to 99%?
coef99 <- round(abs(qnorm(0.005)),2)
coef99
# then the 99% confidence interval for  is 
lower99 <- avg.hp - coef99 * se.hp
upper99 <- avg.hp + coef99 * se.hp
ci99 <- c(lower99, upper99)
ci99

#---------------------------------------------------------------------
# Session 2.3 Review
# - Sampling
#   * sample(x, n, replace = FALSE)
#   * Bootstrap resampling
#   * Sampling with probability
# - Distribution
#   * Normal Distribution / Standard Normal Distribution
#   * rnorm(), pnorm(), qnorm(), dnorm()
#   * Characteristics of normal distributed bell curves
# - Confidence Interval
#   * Difference between population and sample in terms of statistics
#   * How to understand confidence interval?
#   * Calculation among confidence level, significance level and Z value
#---------------------------------------------------------------------

# finished on Thu. 10/31/2014
