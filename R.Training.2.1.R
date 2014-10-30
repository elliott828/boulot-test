##############################################
## 2. ADVANCED OPERATION                    ##
## 2.1 Control Flow & Self-defined Function ##
##############################################

#--------------#
# Control Flow #
#--------------#

# if, else: testing a condition

# if(<condition>) {
#        ## do something
# } else {
#        ## do something else
# }

# Sale Activity in the Grocery
# Primary price of apple is 10 yuan/kg.
price <- 10

# If the customers buy more than 2 kg apples, they'll get 10% discount.

apple <- 4 # Number of kilograms that the customer buys (apple > 0)
if(apple > 2) {
  dr <- 0.1  # discount rate
} else {
  dr <- 0
}
dr
pay <- price * apple * (1-dr)
pay

# simplify
dr <- if(apple > 2) {
    0.1
} else {
    0
}
pay <- price * apple * (1-dr)
pay

# There can also be no "else"...
if(apple > 2) {
    dr <- 0.1
}

# What if there are more than one condition?
# The answer is "else if"

# if(<condition1>) {
#        ## do something
# } else if(<condition2>) {
#        ## do something different
# } else if(<condition3>) {
#        ## do something different
# } else if(<condition...>) {
#        ## do something different
# }

# "else if" can be repeated for as many times as you like

# Logic Symbols
# "&" and
# "|" or
x <- 5
x < 4
x > 3
x > 2
x > 3 & x > 2
x < 4 & x > 2
x < 4 | x > 2
# Conditions are always evaluated from left to right
x > 3 & x > 2 | x < 4
(x > 3 & x > 2) | x < 4
x > 3 & (x > 2 | x < 4)

# More you buy, larger discount you'll get.
# If buy more than 3 kg apples, discount rate = 30%.
# If buy more than 5 kg apples, discount rate = 50%.
apple <- 4
dr <- if(apple <= 2) {
  0
} else if(apple > 2 & apple <= 3) {
  0.1
} else if(apple > 3 & apple <= 5) {
  0.3
} else {
  0.5
}
dr
pay <- price * apple * (1-dr)
pay


#---------------------------------------------------------------------

# for: execute a loop within a fixed number of times

# for(<condition>){
#           ##do something
# }
# condition: "in" a vector

for(i in 1:10) {
  print(i)
}

for(i in c(5,3,9,6,0)) {
  print(i)
}

x <- c("a","b","c","d")
for(i in 1:4) print(x[i])

# for loops can be nested
y <- matrix(1:6,2,3)
for(i in 1:2) {
  for(j in 1:3) {
    print(y[i,j])
  }
}

# next: skip an iteration of a loop
# Different control flows can be nested 

for(i in 1:30) {
  if(i <= 20) {
    # Skip the first 20 iterations
    next
  }
  print(i)
}


#---------------------------------------------------------------------

# while: execute a loop while a condition is true

count <- 0
while(count < 10) {
  print(count)
  count <- count + 1
}

# Compared with for loops, for loops will execute automatically in an order
# In while loops, in the condition usually an edge is set, then we need to let the variable to get closer and closer to the edge in order to cause a stop, or the loop will just execute again and again
# If not careful, while loops can easily result in infinite loops

# There may be problems whether the condition is ALWAYS true or false
z <- 2
while(z >= 3) {
  print(z)
  z <- z + 1
}


#---------------------------------------------------------------------

# repeat: execute an infinite loop
# break: break the execution of a loop
# The only way to exit a repeat loop is to call break

# Fibonacci Sequence (Rabbit Sequence)
z <- 1; z[2] <- 1; i <- 1
z
repeat {
  z[i+2] <- z[i] + z[i+1]
  i <- i + 1
  if (z[i] + z[i+1] >= 1000) break
}
z


#=====================================================================

#-----------------------#
# Self-defined Function #
#-----------------------#

# Defining:
# name <- function(<arguments>) {
#           #do something interesting
# }

# Using:
# name(.)

# Add two numbers together

add2 <- function(x, y) {
  x + y
}

add2(12, 5)

# Return the elements of a vector that are bigger than a number

above <- function(x, n = 10) { # Default value of n is 10
  use <- x > n # use has logical values
  x[use]
}

x <- 1:20
x
above(x)
above(x, n = 5)

# simpler programming grammar
norm <- function(x) x+x
norm(1:4)

# Optional
## An anonymous function:
# The unnamed function will just disappear, which is not stored in the memory
(function(x) x+x)(1:4)


#=====================================================================

#-------------------------------------------#
# Use Self-defined Function in Control Flow #
#-------------------------------------------#

# Little Example
# Students Evaluation
achieve <- function(math) { # input scores for Maths test
  if(math > 95) {
    ach <- "Excellent"
  } else if(math > 85) {
    ach <- "Great"
  } else if(math > 75) {
    ach <- "Good"
  } else if(math > 65) {
    ach <- "Medium"
  } else if(mat >= 60) {
    ach <- "Pass"
  } else {
    ach <- "Fail"
  }
  ach
}
achieve(80)
achieve(67)

# Optional Example
# Old Game: Rock, Scissors and Paper
# Write a function which has two parameters to decide which person wins the game
# Hints: rock <- 0, Scissors <- 1, paper <- 2

rock <- 0
scissors <- 1
paper <- 2

game1 <- function(person1, person2) {
  if((person1 - person2 == 2)|(person1 - person2 == -1)) {
    print("person1 wins")
  } else {
    print("person2 wins")
  }
}

game1(rock, scissors)
game1(rock, paper)
game1(paper, scissors)
game1(paper, rock)
game1(scissors, rock)
game1(scissors, paper)

# ---------------------------- OPTIONAL ----------------------------
# Have a look at the picture first
# New members: lizard and Spock
# Relationship:
# lizard can poison Spock, can eat paper
# lizard can be crushed by rock, can be decapitated by scissors
# Spock can vaporize rock, can smash scissors
# Spock can be poisoned by lizard, can be disproved by paper

# Write a function which has two parameters to decide which person wins the game
# Hints: rock <- 0, Spock <- 1, paper <- 2, lizard <- 3, scissors <- 4

rock <- 0
Spock <- 1
paper <- 2
lizard <- 3
scissors <- 4

game2 <- function(person1, person2) {
  if((person1 - person2 > 0) & (person1 - person2 < 3)) {
    print("person1 wins")
  } else if((person1 - person2 <= -3)) {
    print("person1 wins")
  } else {
    print("person2 wins")
  }
}

game2(lizard, Spock)
game2(paper, lizard)

#---------------------------------------------------------------------
# Session 2.1 Review
# - Control Flow
#   * if(...){...}else if(...){...}else{...}
#   * for(...){...}
#   * while(...){...}
#   * repeat{... <break> ...}
# - Self-Defined function
#   * function(arg1, arg2, ...){...}
#---------------------------------------------------------------------

# finished on Wed. 10/28/2014 by Katherine
