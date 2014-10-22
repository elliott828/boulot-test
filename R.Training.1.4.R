###############################
## 1. BASIC CONCEPT          ##
## 1.4 List & Missing Values ##
###############################

#------#
# List #
#------#

# What is a list?

# Consider LIST as a to-do-list, which stores everything you want
# a list in R can store no matter which kind of data type
# without considering the data coercing, length of object

#---------------------------------------------------------------------

# How to create a list?
# list()

matr <- matrix(1:12, 3, 4)
vect <- LETTERS[1:13]
fact <- factor(sample(rep(c("turkey","monkey","donkey","mickey"),10),10),
               order = T, levels = c("mickey","turkey","monkey","donkey"))
dfrm <- mtcars[1:5,1:4]

# a list can contain vector, factor, matrix & data frame
list1 <- list(matr, vect, fact, dfrm)
list1

# a list can also store another list
list2 <- list("You see this is a list", list1)
list2
str(list2)

#---------------------------------------------------------------------

# How to name a list?

# method1 - name it when you create it
list1 <- list(MATRIX = matr, VECTOR = vect, FACTOR = fact, DATAFRAME = dfrm)
list1

# method2 - names()
names(list2) <- c("INTRO", "LIST")
str(list2)
names(list2$LIST) <- c("MATRIX", "VECTOR", "FACTOR", "DATAFRAME")

#---------------------------------------------------------------------

# How to subset a list?

# method1 - subset with indexes
list1[[1]]
# ATTENTION: there are double square brackets instead of single square brackets

# method2 - subset with names
str(list2)
list2$INTRO
list2$LIST
list2$LIST$DATAFRAME

#=====================================================================

#----------------#
# Missing Values #
#----------------#

# In R, NA is used to represent any value that is 'not available' or 'missing'

# calculation of NA
x <- c(41, NA, 24, NA)
x + 3
x * 3

#---------------------------------------------------------------------

# how do we know if a vector contains NAs or not?
is.na(x)
# complete.cases(): opposite of is.na()
complete.cases(x)

# how about logic operator?
x == NA

#---------------------------------------------------------------------
# How to replace/remove NA?
mean(x)
mean(x, na.rm = T)

# remove NA
y <- x[!is.na(x)]
y

z <- x[complete.cases(x)]
z

# replace NA
x[is.na(x)] <- mean(x, na.rm=T)
x

#---------------------------------------------------------------------

# NaN: Not a number
Inf - Inf
0/0
