################################
## 1. BASIC CONCEPT           ##
## 1.1 Basic Intro & Sequence ##
################################

#---------#
# Hashtag #
#---------#

message("'#' is used to add comments")
# Hashtag is used to add comments

#------------#
# Calculator #
#------------#

# Addition
5+2

# Subtraction
7-13

# Multiplication
3*4

# Division
12/3

# Modulo --> calculate the remainder
10%%7
9%%5

# Quotient
15%/%7
10%/%3

# Exponent
5^2


# QUESTION:
# 1. What's the difference between %/% and /?
# 2. What about %*%?
# 3. What's the difference between %*% and *?
# Scalar product
# Talk about that later

#------------#
# Assignment #
#------------#

# <- most used
# =
# ->

my_var1 <- 42
my_var2 = 21
7 -> my_var3

# show the value of variables
my_var1
my_var2
my_var3

# Assignment of a varible with other variables
my_var4 <- my_var1 + my_var2 + my_var3
my_var4

#---------------------------#
# Concatenation/Combination #
#---------------------------#

student.id1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
student.id2 <- c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
all.id <- c(student.id1, student.id2)
new.id <- c(all.id, 21, 22)
new.id

#----------#
# Sequence #
#----------#
1:10
pi:10
10:1

# seq(from = 1, to = 1, by =((to - from)/(length.out - 1)))
seq(1,20)
seq(1.5, 10.5)
seq(1.6, 10.2)

seq(0, 10, 0.5)
seq(by = 0.5, to = 10, from =0)

my_seq <- seq(0, 10, length = 30)
length(my_seq)
my_seq

seq(along = my_seq)
seq_along(my_seq)

rep(0, times = 40)
rep(0, 40)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)

#--------------------#
# Vector Computation #
#--------------------#

a <- 1:10
b <- seq(2, 20, 2)
c <- 1.1:10.1
a1 <- 1:5
a2 <- 1:7

# addition
a + 2
a + b
a + a1
a + a2

# subtraction
b - 10
a - b
a - a1
a - a2

# multiplication
a * 3
a * b

# division
a / 5
a / b

# scalar multiplication
a %*% b

# quotient
a %/% b
b %/% c

# modulo
a %% b

# square root
sqrt(a)


# finished on Mon. 8/4/2014
