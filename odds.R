##=========================================================================================================
## FUNDAMENTAL FUNCTIONS                 | 
## 1. co(x, co.rate)                     | ~ carryover effect transformation
## 2. sc(x, lamda1, lamda2)              | ~ s-curve transformation
## 3. pc(x, exponent)                    | ~ power curve transformation
## 4. bt(x, type, object)                | ~ +,-,*,/ with certain object and other basic transformation
##=========================================================================================================

#-------------------#
# 1. co(x, co.rate) #
#-------------------#

co <- function(x, co.rate){
  # carry over effect
  # formula: period2 = period1 * carryover rate + period2
  # x: variable to be transformed
  # co.rate: carry over rate
  # make sure the 'x' already exists in the environment
  
  for (p in 2:length(x)){
    if (is.na(x[p-1])){
      # p: position of the element
      x[p] = x[p]
    }else{
      x[p] = x[p-1] * co.rate + x[p]
    }
  }
  
  return(x)
}  

#=====================================================================

#--------------------------#
# 2. sc(x, lamda1, lamda2) #
#--------------------------#

sc <- function(x, lamda1, lamda2) 1 - exp(-lamda1 * x^lamda2)
  # s-curve transformation
  # formula: 1-e^(-lamda1 * x^lamda2)
  # x: variable to be transformed
  # lamda1 & lamda2: 2 parameters of the s-curve
  # make sure the 'x' already exists in the environment

#=====================================================================

#--------------------#
# 3. pc(x, exponent) #
#--------------------#

pc <- function(x,exponent) x^exponent
  # power curve transformation
  # formula: x^i
  # x: variable to be transformed
  # exponent: exponent...
  # make sure the 'x' already exists in the environment

#=====================================================================

#-------------------------------#
# 4. bt(x, type, object = NULL) #
#-------------------------------#

bt <- function(x, type, object = NULL){
  # basic transformation:
  # 1.1~1.4: +,-,*,/
  # 2: logarithm
  # 3: root
  # 4: exponent (exponent > 1; intergers only; different than pc(x, exponent))
  # 5: reciprocal
  # 6: time lag
  #---------------------------------
  # x: variable to be transformed
  # type: index of transformation type - numbers **1.1~1.4** & integers **2~6**!!!
  # object: Default is NULL. When type = certain index, object to be provided
  #---------------------------------  
  # make sure the 'x' is already loaded to upper level environment!!!
  # if 'object' is a variable, make sure 'object' is already loaded to upper level environment!!!
  # the validity/feasibility of using the 'x' and 'object' need to be verified OUTSIDE bt(...)!!!
  #---------------------------------
  
  #----------------------------------------#
  # PART I - Transformation method 1.1~1.4 #
  #----------------------------------------#
  asmd <- function(x, type, object){
    # asmd stands for addition/substraction/multiplication/division
    
    # type to be transformed from numbers to letters in capital
    # type is supposed to be in the format 1.1~1.4, need to capture the 3rd field
    # type is supposed to be formed OUTSIDE bt(...) by asking user questions
    opt <- LETTERS[as.numeric(substr(as.character(type),3,3))]
    
    # check the object OUTSIDE bt(...): a number, min/max/mean or a data serie/variable
    # number keeps as numeric vector (length(object) = 1) in object
    # "min"/"max"/"mean" should be turned into min(x)/max(x)/mean(x) before store into object
    # variable stored as numeric vector (length(object) = length(x))
    
    switch(opt,
           A = x + object,
           B = x - object,
           C = x * object,
           D = x / object)
  }
  
  #-----------------------------------#
  # PART II - Transformation method 6 #
  #-----------------------------------#
  tlag <- function(x, object){
    # create lagged data series (forward or backward)
    
    # object should be an integer (abs(object) < length(x))
    # object validity should be checked OUTSIDE tlag(...) & bt(...)
    
    # check the sign of object
    opt <- LETTERS[sign(object)+2]
    object <- abs(object)
    switch(opt,
           # object < 0: backward time lag
           A = c(x[(object+1):length(x)],rep(NA, object)),
           # object = 0: no time lag
           B = x,
           # object > 0: forward time lag
           C = c(rep(NA, object),x[1:(length(x)-object)]))
    
  }
  
  #-----------------------------------#
  # PART III - General transformation #
  #-----------------------------------#
  general.opt <- letters[round(type,0)]
  switch(general.opt,
         # 1.1~1.4: +,-,*,/
         a = asmd(x , type, object),
         # 2: log
         b = log(x),
         # 3: root
         c = x^(1/object),
         # 4: exponent
         d = x^object,
         # 5: reciprocal
         e = 1/x,
         # 6: time lag
         f = tlag(x, object))
  
}

#=====================================================================












# CREATION: 11/7/2014 co(),sc(),pc(),bt() creation
