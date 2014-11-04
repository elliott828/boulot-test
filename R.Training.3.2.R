######################################
## 3. Plotting & Application        ##
## 3.2 MSU Modeling Tool: AutoReg() ##
######################################

# Used to do traffic / WINPIN analysis
# The most updated version: AutoReg.V1.5.R
# Basic modeling algorithm: Multiple Linear Model 
#                           (Stepwise Regression on both direction)

# read the function document
source("https://raw.githubusercontent.com/elliott828/boulot-test/master/AutoReg.V1.5.R")
# mind the difference between OS X and WINDOWS

# for OS X:
if(!"RCurl" %in% installed.packages())install.packages("RCurl")
library(RCurl)
ex <- getURL("https://raw.githubusercontent.com/elliott828/boulot-test/master/AutoReg.V1.5.R",
             ssl.verifypeer=0L, followlocation=1L)
writeLines(ex, "temp.R")
source("temp.R")
break

#=====================================================================
#---------------#
# Basic Process #
#---------------#

# 1. Select transformation type
# 2. Try modeling with transformed variables
# 3. Keep the variable if good, or try other variable or other transformation
# 4. Confirm the final model

#=====================================================================
#------#
# Demo #
#------#

AutoReg("tw.fcs.csv")

# finished on Tue. 11/4/2014
