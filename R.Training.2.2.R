###########################
## 2. Advanced Operation ##
## 2.2 Input and Output  ##
###########################

#---------------#
# Read R Script #
#---------------#

# source()
# sourcing from local directory
source("r.ex.R")

# sourcing from Internet
source("https://raw.githubusercontent.com/elliott828/boulot-test/master/r.ex.R")

# OPTIONAL: OS X system cannot read content from "https://"
if(!"RCurl" %in% installed.packages()){install.packages("RCurl")}   # CHECK whether "RCurl" is installed
library(RCurl)
ex <- getURL("https://raw.githubusercontent.com/elliott828/boulot-test/master/r.ex.R",
             ssl.verifypeer=0L, followlocation=1L)
writeLines(ex, "temp.R")
source("temp.R")

#=====================================================================

#-----------------------------#
# Export Data / Write A Table #
#-----------------------------#

# write.table()
?write.table
?mtcars
write.table(mtcars, file = "mtcars1.txt")
write.table(mtcars, file = "mtcars2.txt", sep = ",", col.names = F, row.names = F)

# export table to specified directory
write.table(mtcars, file = "C:/Users/mtcars3.txt")
write.table(mtcars, file = "C:\\Users\\mtcars4.txt")
# be careful on the direction of slash
# The address in WINDOWS platform is backslash: "C:\Users\R-Test"
# R read "C:\Users\R-Test" as "C:UsersR-Test" -- "\" is the escape symbol
# Using slash or double backslash for specifying the address in write.table()
# That also applies while using other message output functions (will be mentioned below)

#---------------------------------------------------------------------
# what if i want to save the data frame to a .csv file?
write.table(mtcars, file = "mtcars1.csv", sep = ",")
# the column names are not correctly matched with the data.

# or, we can try this function:
write.csv(mtcars, "mtcars2.csv")
# function specifies "," as separator
# and "." as decimal point

# write.csv2()?
# function specifies ";" as separator
# and "," as decimal point
# i.e. French system usually uses this format

#=====================================================================

#--------------#
# Read A Table #
#--------------#
# read a file in table format and creates a data frame from it, 
# with cases corresponding to lines and variables to fields in the file.

# ?read.table
# read.table(file, header = F, sep = "", ...)
read.table("mtcars1.txt") -> table1
read.table("mtcars1.txt", header = T) -> table2

# Why header is not set as the default value: False?
# header is set to TRUE if and only if 
# the first row contains ONE FEWER field than the number of columns.
read.table("mtcars1.txt", header = F) -> table3
# Error message generates

write.table(mtcars, file = "mtcars2.txt", col.names = F)
read.table("mtcars2.txt") -> table4
table4

#---------------------------------------------------------------------
# read .csv data
# read.csv(file, header = TRUE, sep = ",")
table5 <- read.csv("mtcars.csv")

# read.csv2(file, header = TRUE, sep = ";")
# corresponding to write.csv2()

#=====================================================================

#-------------------------#
# Read & Write Excel File #
#-------------------------#

# What if I want to read and write .xls / .xlsx files?
# search on the Internet
# many solutions are provided: XLConnect, gdata, xlsx
install.packages("XLConnect") 
# or install.packages("xlsx")
# for these 2 packages above make sure "Java" is installed in your PC (free download)
# for "gdata", make sure "Perl" is available in your PC

library(XLConnect)

# or use require(), realize the previous steps together
require(XLConnect)

# write.xlsx(data frame, file)
write.xlsx(mtcars, file = "mtcars.xlsx")
write.xlsx(mtcars, file = "mtcars.xls")

# read.xlsx(file, worksheet name / worksheet index)
read.xlsx("mtcars.xlsx", "Sheet1")
# or
read.xlsx("mtcars.xls", 1)

#=====================================================================

#-----------------#
# Prompt Messages #
#-----------------#

# print()
print("We've learned 6 sesssions!")

x <- "We've learned 6 sesssions!"
print(x)
print(x, quote = F)
class(print("We've learned 6 sesssions!"))

#---------------------------------------------------------------------

# message()
y <- "How many sessions left?"
message(y)
class(message("How many sessions left?"))

#---------------------------------------------------------------------

# cat()
# concatenate and print
cat("My hometown is Beijing")
cat("My hometown is ", "Beijing", sep = "")

hometown <- c("Beijing", "Shanghai", "Guangzhou", "Shenzhen", "Chengdu", "Chongqing")
cat("My hometown is ", hometown[sample(1:6,1)],sep = "") 
# no need to specify replace = 1 since the sample size is 1.


#---------------------------------------------------------------------

# what if I want to print more than 1 row?
cat("All things in their being are good for something.\nR in its being is good for MSU.")
# use "\n" to break the line
# or:
cat("All things in their being are good for something.",
    "R in its being is good for MSU.", sep = "\n")

# warning messages
# warning()
warning("test")

budget <- 300 # budget for a booklist
book <- 27    # avg. price per book
total <- 0    # total cost
for (i in 1:20){
  total <- total + book
  if (total > budget){
    cat("You have bought", i-1 , if(i>1){"books!"}else{"book!"}, sep =" ")
    warning("Not enough money, Bookworm!")
    break
  }
}

#---------------------------------------------------------------------

# warnings()
# print last warning message in a "pleasing" form
warning("Oh! Don't Laugh at Me!")
warnings()
# assign("last.warning", NULL, envir = baseenv())

# warnings(...)
# concatenate with the last warning message, argument to be passed to cat()
warnings("Dude!")

no.books <- 11
warnings("Dude! You have bought", no.books, "books!", sep = " ")


#---------------------------------------------------------------------
# Session 2.2 Review
# - Read R script
#   * source()
#   * sourcing from local directory or from online
#   * difference between WINDOWS and OS X
# - Export / write a table
#   * write .txt file: write.table()
#   * write .csv file: write.csv()
# - Read a table
#   * read .txt file: read.table()
#   * read .csv file: read.csv()
# - Read & write Excel file
#   * install packages: install.packages(), library(), require()
#   * the most popular EXCEL packages:
#     ~ XLConnect: write.xlsx(); read.xlsx()
#     ~ xlsx
#     ~ gdata
#   * make sure relative software (e.g. java, perl, etc.) installed
# - Prompt messages
#   * print a message: print() / message()
#   * concatenate and print: cat()
#   * warning message: warning(), warnings()
#---------------------------------------------------------------------

# finished on Thu. 10/30/2014
