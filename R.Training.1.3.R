#########################################
## 1. BASIC CONCEPT                    ##
## 1.3 Matrix, Data Frame & Subsetting ##
#########################################

#--------#
# Matrix #
#--------#

# What is a matrix?

# In R, a matrix is:
# - a collection of elements 
# - of the *same data type* (numeric, character, or logical) 
# - arranged into a fixed number of rows and columns

# can be considered as a vector of 2 dimensions (so that it's no more a vector)

#---------------------------------------------------------------------

# how to create a matrix?
# matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)

# the simplest
matrix(data = 1:8)  # do you still remember what does 1:8 generate?

# fix the no. of row and no. of column
matrix(data = 1:8, nrow = 2, ncol = 4)
# what if: nrow * ncol != length(data)?

# fix the data filling direction
matrix(data = 1:8, nrow = 2, ncol = 4, byrow = T)

# name the 2 dimensions
matrix(data = 1:8, nrow = 2, ncol = 4, byrow = T, 
       dimnames = list(c("row1","row2"),
                       c("col1","col2","col3","col4")))
# compare the function above with the one below:
matrix(1:8, 2, 4, T, 
       list(c("row1","row2"),
            c("col1","col2","col3","col4")))

#---------------------------------------------------------------------

# Example's on~
# Barclays Premier League 2013-2014
# Top 4 teams
w <- 27:24             # won
d <- c(5,6,7,7)        # drawn
l <- c(rep(6,3),7)     # lost
gf <- c(102,101,71,68) # goals for
ga <- c(37,59,27,41)   # goals against

top4 <- matrix(c(w,d,l,gf,ga),nrow=4)
top4

#---------------------------------------------------------------------

# name the matrix
# 2 methods -> 2 sets to be prepared
# top4 & top4.bis
top4.bis <- top4

# method 1: dimnames()
dimnames(top4) <- list(c("Manchester.City", "Liverpool", "Chelsea", "Arsenal"),
                       c("Won","Drawn","Lost","Goals.for","Goals.against"))

# method 2: colnames() & rownames()
colnames(top4.bis) <- c("Won","Drawn","Lost","Goals.for","Goals.against")
rownames(top4.bis) <- c("Manchester.City", "Liverpool", "Chelsea", "Arsenal")

#---------------------------------------------------------------------

# basic calculation
# rowSums(), colSums()
# rowMeans(), colMeans()

rowSums(top4)
colSums(top4)
rowMeans(top4)
colMeans(top4)

#---------------------------------------------------------------------

# how to add a column or a row?
# cbind()
# rbind()

top6 <- rbind(top4, c(21,9,8,61,39), c(21,6,11,55,51))
rownames(top6) <- c(rownames(top4),"Everton","Tottenham.Hotspur")

top6 <- cbind(top6, matrix(rep(NA, 12), ncol = 2))
colnames(top6) <- c(colnames(top4),"Goals.diff","Points")
top6

#=====================================================================

#------------#
# Data Frame #
#------------#

# What if your dataset contains multiple data types?
# Intro of data frame~

# A data frame can store different data types.
# A data frame has the variables of a dataset as columns and the observations as rows.
# A data frame is a collection of vector / factor of same length.

# Example: R built-in dataset
data()

?cars
?mtcars

head(mtcars)
head(mtcars,10)
tail(mtcars)
tail(mtcars,3)
str(mtcars)

#---------------------------------------------------------------------

# Create a data frame
# data.frame()

# method1 - data.frame(vector/factor/matrice/data.frame, vector/factor/matrice/data.frame,...)

# example 1.1
a <- rep(c(T,F),each = 4)
b <- seq(1,8)
c <- LETTERS[1:8]   # LEETERS leads to 26 letters.
d <- factor(LETTERS[19:26],order=T)
df <- data.frame(a,b,c,d)
df
str(df)
# all character vectors will be turned to *nominal factors*

# example 1.2
df.top6 <- data.frame(top6, c(rep("C.L.",3),"C.L.PO.",rep("E.L.",2))) # top6 is a matrice
# C.L. for champion league
# C.L.PO. for champoin league play-off round
# E.L. for Europa League

names(df.top6)
colnames(df.top6)
row.names(df.top6)
rownames(df.top6)
# remember the various methods for getting the column/row names

names(df.top6) <- c(colnames(top6),"Qualification")
df.top6

# method2 - as.data.frame(...) (force sth to be come sth...like a vampire...)
df.top6.bis <- as.data.frame(top6)
df.top6.bis

# similarily
df.top6.test <- as.matrix(df.top6) # coerce an object to a matirx
df.top6.test

#---------------------------------------------------------------------

# how to confirm your dataset is a data frame?
is.data.frame(df.top6)
is.data.frame(df.top6.bis)

# similarily
is.matrix(top6)  # matrix
is.vector(a)     # vector
is.factor(c)     # factor
is.factor(df$c)  # factor
is.ordered(d)    # ordinal factor
is.numeric(b)    # numeric vector
is.integer(b)    # integer vector
is.logical(a)    # logical vector
is.character(c)  # character vector

#=====================================================================

#------------#
# Subsetting #
#------------#

# subsetting by number indexes

# subsetting a vector/factor
# imagine in an Excel file, there is a column of data
# vector[i] means the i-th element in this vector

LETTERS
LETTERS[26]

atoz <- factor(LETTERS)
atoz[5]

# subsetting a matrix/data frame
# imagine in an Excel file, there is a block of data
# a dataset has 2 dimensions: rows indexes as i, columns indexes as j
# dataset[i,j]: means the value in i-th row and j-th column of this dataset
df.top6

# indexes with comma subsets a single value or vector/factor
df.top6[2,]
df.top6[,3]
df.top6[1,2]
class(df.top6[,8])

# index without comma subsets a data frame from a *data frame* in corresponding *column* 
df.top6[3]
df.top6[1]

# ATTENTION!!!
# index without comma subsets a value from a matrix in corresponding *element index*
matr1 <- matrix(8:1, 2, 4)
matr1[3]
matr1

matr2 <- matrix(LETTERS, byrow = T, 2, 13)
matr2[16]
matr2
# the elements indexes counts always vertically no matter the matrix 
# is arranged by row or by column!

# what if you want to subset certain columns and rows?
# define a vector of indexes
df.top6[1:3,]
head(df.top6,3)

df.top6[4:6, 1:3]

tail(df.top6, 4)
df.top6[(nrow(df.top6)-3):nrow(df.top6),]
# nrow() returns the count of rows of a dataset
# ncol() returns the count of columns of a dataset

df.top6[c(1,3,5),]
df.top6[,c(1,3,5)]

#---------------------------------------------------------------------

# subsetting by names with []
# similar logic as number indexes

df.top6[,"Won"]
df.top6["Everton",]
df.top6["Liverpool", "Goals.for"]

# subsetting without comma
df.top6["Won"]

# what if you enter a name that doesn't exist?
df.top6["Manchester.United",]

# mixing number and name
df.top6[3, "Goals.against"]

#---------------------------------------------------------------------

# subsetting by names with $
# $ can only be used to subset columns

df.top6$Won
df.top6$Goals.for

# $ cannot be used for subsetting from a matrix!
top6$Lost

#---------------------------------------------------------------------

# subsetting by functions

# subset()
subset(df.top6, df.top6$Qualification == "C.L.")
subset(df.top6, df.top6[4] >= 100)

# which() returns the indexes of *rows* which satisfy the condition
which(top6[,"Won"]>=26)
df.top6[which(df.top6$Qualification == "C.L.")]
df.top6[, which(df.top6$Qualification == "C.L.")]
df.top6[which(df.top6$Qualification == "C.L."),]

# subsetting dataset without NAs
matr3 <- matrix(24:1, 6, 4)
matr3[c(3,12)] <- NA
matr3
df3 <- as.data.frame(matr3)

# complete.cases()
# returns logical values indicating if each *line* of observations contains NA
complete.cases(matr3)

matr3[complete.cases(matr3)]
matr3[complete.cases(matr3),]
df3[complete.cases(df3),]

# is.na() - returns indexes of element
# returns logical values indicating if each *element* is a NA
is.na(matr3)

matr3[is.na(matr3)]
matr3[!is.na(matr3[,1]),]
matr3[!is.na(matr3[,1])&!is.na(matr3[,2])&!is.na(matr3[,3])&!is.na(matr3[,4]),]
# not efficient at all...
# remove rows with NAs
matr3[rowSums(is.na(matr3))==0,]

# remove columns with NAs
matr3[,colSums(is.na(matr3))==0]

#---------------------------------------------------------------------

# Calculation with subset

# do you remember the time~...erh, do you remember the df.top6?
df.top6
df.top6.bis <- df.top6  # prepare a same dataset for comparison

# there are 2 columns of NAs: Goals.diff and Points, how to fill them up?

# Goals.diff = Goals.for - Goals.against
df.top6$Goals.diff <- df.top6$Goals.for - df.top6$Goals.against
df.top6.bis[,6] <- df.top6.bis[,4] - df.top6.bis[,5]

df.top6
df.top6.bis

# Points = Won*3 + Drawn*1 + Lost*0
df.top6$Points <- df.top6$Won*3 + df.top6$Drawn*1 + df.top6$Lost*0
df.top6.bis[,7] <- df.top6.bis[,1]*3 + df.top6.bis[,2]*1 + df.top6.bis[,3]*0

df.top6
df.top6.bis

#=====================================================================

#----------#
# Optional #
#----------#

# Insert a new column with subset
# What if I want to insert the rank for this 6 teams
# previously we mentioned cbind()

df.top6$Rank <- 1:6
df.top6
# vs.
df.top6.bis[,9] <- 1:6
df.top6.bis
names(df.top6.bis)[9] <- "Rank"
df.top6.bis

#---------------------------------------------------------------------

# sorting a dataset by certain variable
# order()
# returns a sorting result which rearranges 
# its first argument into asc./desc. order
best.defense <- order(df.top6$Goals.against)
best.defense
df.top6[best.defense,]

worst.defense <- order(df.top6$Goals.against, decreasing = T)

# think about the 2 functions below,
# what's the different between them and order()?
sort(df.top6$Goals.against)
rank(df.top6$Goals.against)

#=====================================================================

#---------------------------------------------------------------------
# Session 1.3 Review
# - Matrix
#   * Definition
#   * How to create a matrix: matrix()
#   * How to name a matrix
#   * Basic row/column calculation: rowSums(), colSums(), etc.
#   * How to insert a row/column: cbind(),rbind()
# - Data Frame
#   * Definition and difference with matrix
#   * Preview of data frame, basic functions: head(), tail(), str()
#   * How to create a data frame: data.frame(), as.data.frame()
# - Subsetting
#   * Subsetting with indexes
#   * Subsetting with row/column names
#   * Subsetting with functions: subset()
#   * Calculation of subsets within a dataset
# - Optional
#   * Insert new columns into a data frame
#   * Sort a dataset
#---------------------------------------------------------------------

# finished on Fri. 8/22/2014
