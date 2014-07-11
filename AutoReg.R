AutoReg <- function(data){
  
  message("Welcome to AutoReg!")
  
  source("ParaS.R")
  source("Modif.R")
  library(car)
  
  # Judge the data format. "*.csv" is preferable
  data <- as.character(data)
  endstr <- substr(data, nchar(data)-2, nchar(data))
  
  #*****************************************************#
  # should be a repeatable loop instead of if...else... #
  #*****************************************************#
  if (endstr != "csv"){
    stop(
      paste("Please prepare the raw data in .csv format,",
            "and don't forget to put the file name between double quotation mark, honey!",
            sep = "\n"))
  }else{
    df <- read.csv(data)
  }
  
  # Choose the variables for modeling
  repeat{
    resp <- as.character(readline("Please specify the response variable: "))
    check1 <- resp %in% names(df)
    if (check1 == FALSE){
      warning(paste("The variable '", resp, "' does not exist in the database!", sep=""))
    }else{
      break
    }    
  }
  
  ndf <- df   # Prepare a new df for further data modification
  model <- NULL  # Assign NULL to model first 
  
  #############################
  ## REPEAT COULD START HERE ##
  #############################
  ini.pred <- readline("Please specify the first explanatory variable for modeling: ")
  #*************************************************************#
  #* A TRAPPER: DECIDER SI C'EST UNE VARIABLE DANS CETTE TABLE *#
  #*************************************************************#
  
  cat(
    paste("Which kind of transformation does the variable '", ini.pred, "' need to be adapted?", sep=""),
    "  1. carry over + power curve",
    "  2. carry over + s curve",
    "  3. auto-selection between 1 and 2",
    "  4. none", "", sep="\n")

  # assign transforming method to opt1
  opt1 <- readline("Please enter an option number: ")
  
  
  #ParaS(ini.pred, resp, ndf, model, opt1)
  offer <- (ParaS(ini.pred, resp, ndf, model, opt1))
  print(offer)
  #***********************************#
  #* Call ParaS() with method = opt1 *#
  #***********************************#

  # Allocate the best combination of parameters
  if (opt1 == 1){
    cat(
      "Do you agree to transform the variable with recommended parameters?",
      paste("carry over rate = ", co.r, sep=""),
      paste("power curve parameter = ", po.r, sep=""),
      "  1. Yes",
      "  2. No", 
      " ",sep="\n")
    opt2 <- readline("Hurry up! 1 or 2? ")
    
    if(opt2 == 2){
      co.r <- readline("Please suggest another carry over rate: ")
      po.r <- readline("Please suggest another power curve parameter: ")
      sc.1 <- NaN
      sc.2 <- NaN
    }
          
  }else if(opt1 == 2){
    cat(
      "Do you agree to transform the variable with recommended parameters?",
      paste("carry over rate = ", co.r, sep=""),
      paste("power curve parameter = ", po.r, sep=""),
      "  1. Yes",
      "  2. No", 
      " ", sep="\n")
    opt2 <- readline("Hurry up! 1 or 2? ")
    
    if(opt2 == 2){
      co.r <- readline("Please suggest another carry over rate: ")
      sc.1 <- readline("Please suggest another value for the 1st s curve parameter: ")
      sc.2 <- readline("Please suggest another value for the 2nd s curve parameter: ")
      po.r <- NaN
    }
       
  }else if(opt1 == 3){
    #############################
    ## RE-PENSER CETTE PARTIE! ##
    #############################
    cat(
      "Do you agree to transform the variable with recommended parameters?",
      paste("carry over rate = ", co.r, sep=""),
      paste("power curve parameter = ", po.r, sep=""),
      "  1. Yes",
      "  2. No", 
      " ",sep="\n")
    opt2 <- readline("Hurry up! 1 or 2? ")
    
    if(opt2 == 2){
      co.r <- readline("Please suggest another carry over rate: ")
      po.r <- readline("Please suggest another power curve parameter: ")
    }
     
  }else{
    co.r <- NaN
    po.r <- NaN
    sc.1 <- NaN
    sc.2 <- NaN
  }
  
  ##################
  ## Call Modif() ##
  ##################
  df0 <- Modif(resp, pred, df0, co.r, po.r, sc.1, sc.2)
  
  # Build 1st model with resp and the 1st pred.
  ini.fit <- lm(resp ~ pred, df0)
  
  # Repeat the explanatory variable selection loop
  # remember to ask if it is necessary to keep the new var. in the model
  # intermedium model is called med.model
  
  # insert all dummy vars and test models
  
  # after all possibilities tried, build final model fin.fit and make a summary
  # do another model using stepAIC()
  # compare 2 models and get the best
  
}
