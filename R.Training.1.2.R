source("https://raw.githubusercontent.com/elliott828/boulot-test/master/r.ex.R")
r.ex()

###############################
## 1. BASIC CONCEPT          ##
## 1.2 Basic Building Blocks ##
###############################

#-----------------#
# Basic Data Type #
#-----------------#

# numeric
4.5
nume <- 4.5

# intergers
5
inte <- 5

# logical
TRUE
FALSE
T
F
logi <- F

# characters
"Hello World"
char <- "Hello World"

# check the data type
# function class()
class(nume) -> type1
class(inte) -> type2
class(logi) -> type3
class(char) -> type4
type1
type2   #? why type2 numeric instead of integer?
type3
type4
class(type1)

inte <- c(5L)

#=====================================================================

#--------#
# Vector #
#--------#

# Only values of same data type can be saved in one single vector!
a <- c(1,2,3)
b <- c("a","b","c")
c <- c(F,T,F)

# what if i store values of different data types in one vector?
a <- c(2, F)
b <- c(1, 0, F)
c <- c(TRUE, "Training", 3)
#---------------------------------------------------------------------

# naming a vector

# salary per day
sal <- rep(200, 5)

# expense per day
exp <- c(-80, -120, -300, -80, -200)

# World cup lottery
wc <- c(400, -80, -40, 200, -120)

# add workday to vector
# names()
names(wc) <- c("Mon", "Tue", "Wed", "Thu", "Fri")
wc
#---------------------------------------------------------------------

# to name all - assign a vector to a vector
workday <- c("Mon", "Tue", "Wed", "Thu", "Fri")
names(sal) <- workday
names(exp) <- workday
# or
names(sal)<-names(exp)<-workday

sal
exp
#---------------------------------------------------------------------

# calculating daily balance
balance <- wc + sal + exp
balance
#---------------------------------------------------------------------

# another way - what's the difference between sum() and "+"
balance.bis <- sum(wc, sal, exp)
balance.bis
# is sum(wc, sal, exp) == sum(sum(wc), sum(sal), sum(exp)) ?
#---------------------------------------------------------------------

# comparing vectors
# which method help you earning more money?
answer1 <- sal > wc
answer2 <- sum(sal) > sum(wc)
answer1
answer2
# what is the data type of answer1 and answer2?

#=====================================================================

#--------#
# Factor #
#--------#
# FACTOR: a statistical data type used to store categorical variables
# The difference between a categorical variable and a continuous variable is that 
# a categorical variable can belong to a limited number of categories.

# how to create a factor
# factor()

gender <- c("Male", "Female", "Female", "Male", "Female", "Female")
f.gender <- factor(gender)

f.gender

# 2 levels are created: Male & Female -> these are 2 categories

#---------------------------------------------------------------------
# 2 types of categorical variables - nominal & ordinal --> 2 types of factors

# nominal --> cannot tell "which one is worth more than the other"
animal <- c("puppy", "piggy", "donkey", "monkey", "kitty")

# ordinal --> have natural odering
fee <- c("low", "medium", "medium", "high", "low")

# how to create 2 types of factors?
f.animal <- factor(animal)
f.fee <- factor(fee, order = T, levels = c("low", "medium", "high"))

#---------------------------------------------------------------------

# what if you are not satisfied on names of levels?
# refer to name assignment of a vector
test.gender <- f.gender
levels(test.gender) <- c("M","F")

