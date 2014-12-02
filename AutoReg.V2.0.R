AutoReg <- function(){
  # STEP 0
  # Welcome!
  cat("\n")
  message("Welcome to AutoReg!")
  message("Please follow instructions to finish the regression.")
  message(paste('You can press the button "Esc" to quit this program.',
                '', sep = "\n"))
  cat("Before using this program, please make sure following files are available", 
      "in the working directory or the internet is connectable:",
      "  1. odds.R",
      "  2. main.R",
      "  3. output.R", sep = "\n")
  repeat{
    cat("\n")
    conf <- readline("Can you confirm (Y/N)? ")
    cat("\n")
    if(toupper(conf) == "N"){
      stop("Get them prepared!")
    }else if(toupper(conf) == "Y"){
      break
    }else{
      message('Only "Y" or "N" is acceptable!')
      cat("\n")
    }
  }
  
  
  #----------------------------------------------------
  # STEP 1.1
  # read raw data and assign to df0
  #----------------------------------------------------
  # STEP 1.1.1
  repeat{
      data.name <- readline("Please enter the name of database (.csv file): ")
      cat("\n")
      endstr <- substr(data.name, nchar(data.name)-2, nchar(data.name))
      
      if (endstr != "csv"){
          message('A ".csv" file is expected!')
          cat("\n")
      }else if(file.exists(data.name)){
          df0 <<- read.csv(data.name)
          message(paste("A data frame with ", dim(df0)[2], 
                        " variables and ", dim(df0)[1], 
                        " observations is imported!", sep = ""))
          cat("\n")
          break
      }else{
          message(paste('The file "', data.name, 
                        '" does not exist in default working directory!', sep = ""))
          cat("\n")
      }
  }
  #----------------------------------------------------
  # STEP 1.1.2
  # Check NA in the input data frame
  check.na <- function(x)sum(is.na(x))
  num.na <- sapply(df0, check.na)
  if(names(num.na)[1] == "X")num.na <- num.na[-1] # number of NA for each variable
  if(sum(num.na)!=0){
      message(paste('There are ', sum(num.na!=0),
                    if(sum(num.na!=0)==1)' variable'else ' variables',
                    ' with NAs!', sep = ""))
      cat("\n")
      stop("Please modify your data frame and restart AutoReg()!")
  }
  
  
  #----------------------------------------------------
  # STEP 1.2
  # Download all related packages
  all.pcg <- c("car","MASS","plyr","lmtest","zoo","stats")
  
  req.pkg <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  invisible(req.pkg(all.pcg))
  
  #----------------------------------------------------
  # STEP 1.3
  # Sourcing all functions! Operation System considered
  all.script <- c("odds.R","main.R","output.R")
  
  src.script <- function(file){
    if(file.exists(file)){
      source(file)
    }else{
      if(Sys.info()["sysname"] == "Windows"){
        source(paste("https://raw.githubusercontent.com/elliott828/boulot-test/master/",file,sep=""))
      }else{
        if(!"RCurl" %in% installed.packages())install.packages("RCurl")
        library(RCurl)
        ex <- getURL(paste("https://raw.githubusercontent.com/elliott828/boulot-test/master/",file,sep=""),
                     ssl.verifypeer=0L, followlocation=1L)
        writeLines(ex, "temp.R")
        source("temp.R")
      }
    }
  }
  
  sapply(all.script, src.script)
  
  #----------------------------------------------------
  # STEP 1.4: Initialization
  options(warn = 1)
  fit1 <<- NULL  # Assign NULL to model first
  fit.history <<- NULL
  df1 <<- NULL
  df.temp <<- NULL
  df.history <<- NULL
  prmt.history <<- NULL
  contri <<- NULL # contribution rate
  mape <<- NULL
  # set up an empty data frame
  prmt <<- data.frame(variable = character(),
                     type = character(),
                     co.r = numeric(),
                     sc.1 = numeric(),
                     sc.2 = numeric(),
                     pc.r = numeric(),
                     object = character(),
                     status = character(),  #indicate the availability of variable for the model
                     stringsAsFactors = F)
  
  # initialize other global variables
  pred <<- NULL
  pred.name <<- NULL
  
  # co.r.recom <<- NULL
  # pc.r.recom <<- NULL
  # sc.1.recom <<- NULL
  # sc.2.recom <<- NULL
  
  obj.name <<- NULL
  obj.recom <<- NULL
  opt.recom <<- NULL
  
  #----------------------------------------------------
  # STEP 1.5: Capture response variable
  repeat{
    resp <- as.character(readline("Please enter the name of response variable: "))
    cat("\n")
    if (resp %in% names(df0)){
      break
    }else{
      message(paste('The variable "', resp, 
                    '" does not exist in the database!', sep=""))
      cat("\n")
    }    
  }
  
  #----------------------------------------------------
  # STEP 1.6: Rebuild previous model if any
  repeat{
    rec <- readline("Do you want to continue testing an existed model (Y/N)? ")
    cat("\n")
    if (!toupper(rec) %in% c("Y","N")){
      message("Please only enter Y or N!")
      cat("\n")
    }else{
      if (toupper(rec) == "N"){
        break
      }else{
        repeat{
          message("Be aware that the file should be of format '.csv'!")
          message("Please confirm that the csv file is closed!")
          cat("\n")
          prmt.name <- readline("Please enter the name of transformation parameter file: ")
          cat("\n")
          endstr <- substr(prmt.name, nchar(prmt.name)-2, nchar(prmt.name))
          
          if (endstr != "csv"){
            message("A '.csv' file is expected!")
            cat("\n")
          }else if(file.exists(prmt.name)){
            rebuild(resp, data = df0, prmt.name)
            prmt <<- prmt.history
            #return(step2.4())
            break
          }else{
            message(paste("The file '", prmt.name, "' does not exist!"))
            cat("\n")
          }
        }
        break
      }
    }
  }
  
  #----------------------------------------------------
  # STEP 1.7: Asking if the intercept to be kept or not
  # after this step, go to step 2.4
  repeat{
    incpt <- readline("Do you want to keep the intercept in the model (Y/N)? ")
    cat("\n")
    if (toupper(incpt) == "Y"){
      if(is.null(fit.history)){
        df.temp <<- df0
        fit.temp <<- trial(data = df.temp, resp = resp, action = 1)
      }else{
        df.temp <<- df.history
        fit.temp <<- trial(data = df.temp, resp = resp, fit = fit.history, action = 1)
      }
      break
    }else if (toupper(incpt) == "N"){
      if(is.null(fit.history)){
        df.temp <<- df0
        fit.temp <<- trial(data = df0, resp = resp, action = -1)
      }else{
        df.temp <- df.history
        fit.temp <<- trial(data = df.history, resp = resp, fit = fit.history, action = -1)
      }
      break
    }else{
      message("Would you please give me a clear answer, Y or N?")
      cat("\n")
    }
  }
  
  if(!is.null(prmt.history)){
    message("Per below you could find the summary of current model:")
    cat("\n")
    loop.output(resp = resp, data = df.temp, fit = fit.temp)
    # return(step2.6())
    readline("Continue modeling... \nPress <Enter> to continue...\n")
  }
  
  #----------------------------------------------------
  # STEP 2.1: 
  step2.1 <- function(){
    # get the name of predictor and check its existence in the database and in the model
    repeat{
      pred.i <<- pred <<- pred <- as.character(readline("Please enter the name of predictor: "))
      cat("\n")
      if (!pred %in% names(df.temp)){
        message(paste('The predictor "', pred, '" does not exist in the database!', sep=""))
        cat("\n")
      }else if(pred %in% names(coef(fit.temp))){
        message(paste('The predictor "', pred, '" already exists in the model!', sep=""))
        cat("\n")
      }else{
        # action = 2
        break
      }    
    }
    
    
    #----------------------------------------------------------------------------
    # STEP 2.2:
    step2.2 <- function(key = 0, opt1 = NULL){
      # select the transformation method for the predictor
      repeat{
        # Choose the transforming method (loop)
        if(key == 0){
          cat(
            paste('Which kind of transformation does the variable "', pred, '" need to be adapted?', sep=""),
            "  For media variables",
            "  1.1 carry over + S curve",
            "  1.2 carry over + power curve",
            "  1.3 auto-selection between 1 and 2",
            "",
            "  For other variables",
            "  2.1.1 addition",
            "  2.1.2 substraction",
            "  2.1.3 multiplication",
            "  2.1.4 division",
            "  2.2 logarithm",
            "  2.3 root",
            "  2.4 exponent",
            "  2.5 reciprocal",
            "  2.6 time lag", "",
            "  0 no transformation", "", sep="\n")
          
          # assign transforming method to opt1
          opt1 <- readline("Please enter an option: ")
          cat("\n")
        }
        
        if(!opt1 %in% c("1.1","1.2","1.3","0","2.1.1","2.1.2","2.1.3","2.1.4","2.2","2.3","2.4","2.5","2.6")){
          message("Man (or u r a woman/girl...)! Only 13 options!")
          cat("\n")
        }else{
          # opt.trans <- 
          # object.trans <- 
          #----------------------------------------------------------------------------
          opt.trans <- opt1
          
          if(opt1 %in% c("1.1", "1.2", "1.3", "0")){
            object.trans <- NULL
            break
            
          } else if(opt1 == "2.2") {
            if(all(df0[[pred]] > 0)) {
              object.trans <- NULL
              break
            } else {
              message(paste('The predictor "', pred, '" has NON-POSITIVE values!', sep=""))
              cat("\n")
            }
            
          } else if(opt1 == "2.5") {
            if(all(df0[[pred]] != 0)) {
              object.trans <- NULL
              break
            } else {
              message(paste('The predictor "', pred, '" contains ZEROs!', sep=""))
              cat("\n")
            }
            
            
          } else if(opt1 %in% c("2.1.1", "2.1.2", "2.1.3")) {
            repeat{
              cat("What's the transformation object?",
                  "  1. a number(zero is ok)",
                  paste("  2. min/max/mean of ", pred, sep = ""),
                  "  3. another variable", "", sep = "\n")
              obj1 <- as.numeric(readline("Please enter an option: "))
              cat("\n")
              if(!obj1 %in% 1:3) {
                message("Man (or u r a woman/girl...)! Only 3 options!")
                cat("\n")
              } else {
                break
              }
            }
            
            if(obj1 == 1) {
              repeat{
                obj2 <- readline("Please enter a number(zero is ok but be careful): ")
                cat("\n")
                if(all(strsplit(obj2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                  object.trans <- as.numeric(obj2)
                  break
                } else {
                  message("Please do enter a number!")
                  cat("\n")
                  cat("Do you want to change transformation method?")
                  chg <- readline("Please choose Y/N: ")
                  if(!toupper(chg) %in% c("Y", "N")) {
                    message("Hey! Man! Only Y or N!")
                    cat("\n")
                  } else if(toupper(chg) == "Y") return(step2.2())
                }
              }
              break
              
              
            } else if(obj1 == 2) {
              repeat{
                obj2 <- tolower(readline("Please make a choice(min/max/mean): "))
                cat("\n")
                if(!obj2 %in% c("min", "max", "mean")){
                  message("Please only choose one of the three!")
                  cat("\n")
                  cat("Do you want to change transformation method?")
                  chg <- readline("Please choose Y/N: ")
                  if(!toupper(chg) %in% c("Y", "N")) {
                    message("Hey! Man! Only Y or N!")
                    cat("\n")
                  } else if(toupper(chg) == "Y") return(step2.2())
                } else {
                  if(obj2 == "min") { 
                    object.trans <- min(df0[[pred]])
                  } else if(obj2 == "max") { 
                    object.trans <- max(df0[[pred]])
                  } else if(obj2 == "mean") {
                    object.trans <- mean(df0[[pred]])
                  }
                  break
                }
              }
              break
              
              
            } else if(obj1 == 3) {
              repeat{
                obj2 <- readline("Please enter the variable's name: ")
                cat("\n")
                if(!obj2 %in% colnames(df0)){
                  message(paste('The predictor "', obj2, '" does not exist in the database!', sep=""))
                  cat("\n")
                } else {
                  object.trans <- df0[[obj2]]
                  break
                }
              }
              break
            }
            
            
          } else if(opt1 == "2.1.4") {
            repeat{
              cat("What's the transformation object?",
                  "  1. a number(zero is NOT allowed)",
                  paste("  2. min/max/mean of ", pred),
                  "  3. another variable", "", sep = "\n")
              obj1 <- as.numeric(readline("Please enter an option: "))
              cat("\n")
              if(!obj1 %in% 1:3){
                message("Man (or u r a woman/girl...)! Only 3 options!")
                cat("\n")
              } else {
                break
              }
            }
            
            if(obj1 == 1) {
              repeat{
                obj2 <- readline("Please enter a number(zero is NOT allowed): ")
                if(all(strsplit(obj2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                  if(as.numeric(obj2) == 0){
                    message("Zero is NOT allowed!")
                    cat("\n")
                    cat("Do you want to change transformation method?")
                    chg <- readline("Please choose Y/N: ")
                    if(!toupper(chg) %in% c("Y", "N")) {
                      message("Hey! Man! Only Y or N!")
                      cat("\n")
                    } else if(toupper(chg) == "Y") return(step2.2())
                  } else {
                    object.trans <- as.numeric(obj2)
                    break
                  }
                } else {
                  message("Please do enter a number!")
                  cat("\n")
                  cat("Do you want to change transformation method?")
                  chg <- readline("Please choose Y/N: ")
                  if(!toupper(chg) %in% c("Y", "N")) {
                    message("Hey! Man! Only Y or N!")
                    cat("\n")
                  } else if(toupper(chg) == "Y") return(step2.2())
                }
              }
              break
              
            } else if(obj1 == 2) {
              repeat{
                obj2 <- tolower(readline("Please make a choice(min/max/mean):"))
                if(!obj2 %in% c("min", "max", "mean")){
                  message("Please only choose one of the three!")
                  cat("\n")
                  cat("Do you want to change transformation method?")
                  chg <- readline("Please choose Y/N: ")
                  if(!toupper(chg) %in% c("Y", "N")) {
                    message("Hey! Man! Only Y or N!")
                    cat("\n")
                  } else if(toupper(chg) == "Y") return(step2.2())
                } else {
                  if(obj2 == "min") { 
                    obj3 <- min(df0[[pred]])
                  } else if(obj2 == "max") { 
                    obj3 <- max(df0[[pred]])
                  } else if(obj2 == "mean") {
                    obj3 <- mean(df0[[pred]])
                  }
                  if(obj3 != 0){
                    obj3 -> object.trans
                    break   
                  } else {
                    message("The ", obj2, " of ", pred, " is ZERO!", sep="")
                    cat("\n")
                    cat("Do you want to change transformation method?")
                    chg <- readline("Please choose Y/N: ")
                    if(!toupper(chg) %in% c("Y", "N")) {
                      message("Hey! Man! Only Y or N!")
                      cat("\n")
                    } else if(toupper(chg) == "Y") return(step2.2())
                  }
                }
              }
              break
              
            } else if(obj1 == 3) {
              repeat{
                obj2 <- readline("Please enter the variable's name: ")
                cat("\n")
                if(!obj2 %in% colnames(df0)){
                  message(paste('The predictor "', obj2, '" does not exist in the database!', sep=""))
                  cat("\n")
                } else {
                  if(any(df0[[obj2]] == 0)){
                    message(paste('The predictor "', obj2, '" contain ZEROs!', sep=""))
                    cat("\n")
                    cat("Do you want to change transformation method?")
                    chg <- readline("Please choose Y/N: ")
                    if(!toupper(chg) %in% c("Y", "N")) {
                      message("Hey! Man! Only Y or N!")
                      cat("\n")
                    } else if(toupper(chg) == "Y") return(step2.2())
                  } else {
                    object.trans <- df0[[obj2]]
                    break
                  }
                }
              }
              break
            }
            
            
          } else if(opt1 %in% c("2.3", "2.4")) {
            repeat{
              obj2 <- readline("Please enter a positive integer: ")
              cat("\n")
              if(all(strsplit(obj2, split = "")[[1]] %in% as.character(0:9))) {
                if(as.numeric(obj2) > 0) {
                  object.trans <- as.numeric(obj2)
                  break
                } else {
                  message("The integer object should be POSITIVE!")
                  cat("\n")
                  cat("Do you want to change transformation method?")
                  chg <- readline("Please choose Y/N: ")
                  if(!toupper(chg) %in% c("Y", "N")) {
                    message("Hey! Man! Only Y or N!")
                    cat("\n")
                  } else if(toupper(chg) == "Y") return(step2.2())
                }
              } else {
                message("Please do enter a positive integer!")
                cat("\n")
                cat("Do you want to change transformation method?")
                chg <- readline("Please choose Y/N: ")
                if(!toupper(chg) %in% c("Y", "N")) {
                  message("Hey! Man! Only Y or N!")
                  cat("\n")
                } else if(toupper(chg) == "Y") return(step2.2())
              }
            }  
            break
            
            
          } else if(opt1 == "2.6") {
            repeat{
              obj2 <- readline("Please enter an integer (either positive or negative is ok): ")
              cat("\n")
              if(all(strsplit(obj2, split = "")[[1]] %in% as.character(0:9))) {
                if(abs(as.numeric(obj2)) < 0.5*nrow(df0)) {
                  object.trans <- as.numeric(obj2)
                  break
                } else {
                  message("The time lag influence need to be SHORTER THAN HALF of the data length!")
                  cat("\n")
                  cat("Do you want to change transformation method?")
                  chg <- readline("Please choose Y/N: ")
                  if(!toupper(chg) %in% c("Y", "N")) {
                    message("Hey! Man! Only Y or N!")
                    cat("\n")
                  } else if(toupper(chg) == "Y") return(step2.2())
                }
              } else {
                message("Please do enter an integer!")
                cat("\n")
                cat("Do you want to change transformation method?")
                chg <- readline("Please choose Y/N: ")
                if(!toupper(chg) %in% c("Y", "N")) {
                  message("Hey! Man! Only Y or N!")
                  cat("\n")
                } else if(toupper(chg) == "Y") return(step2.2())
              }
            }
            break
          }
        }
      }
      #----------------------------------------------------------------------------
      # output of step 2.2: object.trans & opt.trans
      
      # STEP 2.3
      # call recom() and recommend parameters
      if(opt1 %in% c("1.1", "1.2", "1.3", "0", "2.2", "2.5")){
          obj.name <<- NULL
      } else {
          obj.name <<- obj2
      }
      
      prmt.recom <- recom(pred = pred, resp = resp, data = df.temp, 
                          type = opt.trans, fit = fit.temp, object = object.trans, obj.name = obj.name)
      
      # store parameters, NA is there is no corresponding value
      co.r.recom <<- prmt.recom[[6]]
      sc.1.recom <<- prmt.recom[[7]]
      sc.2.recom <<- prmt.recom[[8]]
      pc.r.recom <<- prmt.recom[[9]]
      opt.recom <<- prmt.recom[[1]]
      # opt.recom <- opt.trans
      obj.recom <<- if(is.null(object.trans)){NA}else{object.trans}
      
      #----------------------------------------------------------------------------
      # output of step 2.3
      # bunch of transformation method and recommended parameters
      
      #----------------------------------------------------------------------------
      # STEP 2.4
      step2.4 <- function(key = 0){
        # confirm transformation method and parameters
        if(key == 0){
          repeat{
            cat("Do you feel comfortable with the recomended transformation and parameters?",
                "  1. Yes, totally agree!",
                "  2. No, I want to try other parameters.",
                "  3. No, I want to try other transformation method.",
                "  4. No, I want to switch a variable.",
                "", sep = "\n")
            opt2 <- readline("Please enter an option number: ")
            cat("\n")
            if(!opt2 %in% c("1","2","3","4")){
              cat("\n")
              message("Please only enter a number between 1 and 4!")
              cat("\n")
            }else{
              opt2 <- as.numeric(opt2)
              break
            }
          }
        }
        
        if(opt2 == 1){
          # OPTION 2.4.1
          # opt2 = 1 update the fit and dataset based on recommendation
          pred.name <<- NULL
          df.temp <<- modif(pred, type = opt.recom, df.temp, co.r.recom, sc.1.recom, sc.2.recom, pc.r.recom, obj.recom, obj.name)
          if(!is.null(pred.name)){
              if((pred != pred.name)) pred <<- pred <- pred.name
          }
          fit.temp <<- trial(df.temp, resp, fit.temp, action = 2, pred)
          
          loop.output(resp, df.temp, fit.temp)
          warn(fit1 = fit1, fit2 = fit.temp, p.cons = 0.2)
          # p.cons = 0.2: p-value cannot excceed 20%, otherwise wanrning message generated
          
        }else if(opt2 == 2){
          # OPTION 2.4.2
          # opt2 = 2 same transformation method, different parameters
          # repeat{ 
          # repeat 2.4.2
          if(opt.trans == "1.1"){
            
            repeat{
              co.r <- readline("Please suggest alternative carry over rate: ")
              if(all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                co.r.recom <<- as.numeric(co.r)
                break
              } else {
                message("Please do enter a number!")
                cat("\n")
              }
            }
            repeat{
              sc.1 <- readline("Please suggest alternative value for the 1st s-curve parameter: ")
              if(all(strsplit(sc.1, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                sc.1.recom <<- as.numeric(sc.1)
                break
              } else {
                message("Please do enter a number!")
                cat("\n")
              }
            }
            repeat{
              sc.2 <- readline("Please suggest alternative value for the 2nd s-curve parameter: ")
              cat("\n")
              if(all(strsplit(sc.2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                sc.2.recom <<- as.numeric(sc.2)
                break
              } else {
                message("Please do enter a number!")
                cat("\n")
              }
            }
            pc.r.recom <<- NULL
            obj.recom <<- NULL
            
          }else if(opt.trans == "1.2"){
            
            repeat{
              co.r <- readline("Please suggest alternative carry over rate: ")
              if(all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                co.r.recom <<- as.numeric(co.r)
                break
              } else {
                message("Please do enter a number!")
                cat("\n")
              }
            }
            repeat{
              pc.r <- readline("Please suggest alternative power curve rate: ")
              if(all(strsplit(pc.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                pc.r.recom <<- as.numeric(pc.r)
                break
              } else {
                message("Please do enter a number!")
                cat("\n")
              }
            }
            sc.1.recom <<- NULL
            sc.2.recom <<- NULL
            obj.recom <<- NULL
            
          }else if(opt.trans == "1.3"){
            
            repeat{
              cat("Which transformation method do you want?",
                  "  1.1 carry over + s curve",
                  "  1.2 carry over + power curve", "", sep = "\n")
              opt.trans2 <- readline("Please enter an option: ")
              cat("\n")
              if(!opt.trans2 %in% c("1.1", "1.2")){
                message("Man (or u r a woman/girl...)! Only 2 options!")
                cat("\n")
              } else {
                opt.trans <- opt.trans2
                if(opt.trans == "1.1"){
                  opt.recom <<- "1.1"
                  repeat{
                    co.r <- readline("Please suggest alternative carry over rate: ")
                    if(all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                      co.r.recom <<- as.numeric(co.r)
                      break
                    } else {
                      message("Please do enter a number!")
                      cat("\n")
                    }
                  }
                  repeat{
                    sc.1 <- readline("Please suggest alternative value for the 1st s-curve parameter: ")
                    if(all(strsplit(sc.1, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                      sc.1.recom <<- as.numeric(sc.1)
                      break
                    } else {
                      message("Please do enter a number!")
                      cat("\n")
                    }
                  }
                  repeat{
                    sc.2 <- readline("Please suggest alternative value for the 2nd s-curve parameter: ")
                    cat("\n")
                    if(all(strsplit(sc.2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                      sc.2.recom <<- as.numeric(sc.2)
                      break
                    } else {
                      message("Please do enter a number!")
                      cat("\n")
                    }
                  }
                  pc.r.recom <<- NULL
                  obj.recom <<- NULL
                  
                }else if(opt.trans == "1.2"){
                  opt.recom <<- "1.2"
                  repeat{
                    co.r <- readline("Please suggest alternative carry over rate: ")
                    if(all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                      co.r.recom <<- as.numeric(co.r)
                      break
                    } else {
                      message("Please do enter a number!")
                      cat("\n")
                    }
                  }
                  repeat{
                    pc.r <- readline("Please suggest alternative power curve rate: ")
                    if(all(strsplit(pc.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
                      pc.r.recom <<- as.numeric(pc.r)
                      break
                    } else {
                      message("Please do enter a number!")
                      cat("\n")
                    }
                  }
                  sc.1.recom <<- NULL
                  sc.2.recom <<- NULL
                  obj.recom <<- NULL
                }
                break
              }
              
            }
            
          }else if(opt.trans %in% c("2.1.1", "2.1.2", "2.1.3", "2.1.4", "2.2","2.3","2.4","2.5","2.6")){
            return(step2.2(key = 1, opt1 = opt.trans))
          } else { # if(opt.trans == "0")
            message("You've chosen No Transformation! So no parameter can be changed! \n")
            cat("\n")
            return(step2.4())
          }
          
          if(opt.trans %in% c("1.1","1.2","2.1.1","2.1.2","2.1.3","2.1.4","2.2","2.3","2.4","2.5","2.6")){
            pred.name <<- NULL
            df.temp <<- modif(pred, type = opt.recom, df.temp, co.r.recom, sc.1.recom, sc.2.recom, pc.r.recom, obj.recom, obj.name)
            if(!is.null(pred.name)){
                if((pred != pred.name)) pred <<- pred <- pred.name
            }
            fit.temp <<- trial(df.temp, resp, fit.temp, action = 2, pred)
            full.opt <- c("1.1","1.2","2.1.1","2.1.2","2.1.3","2.1.4","2.2","2.3","2.4","2.5","2.6")
            opt <- letters[which(opt.trans == full.opt)]
            
            object.bis <- if(length(obj.recom)>1){obj2}else{obj.recom}
                        
            trans.opt <- switch(opt,
                                a = paste("Carry Over and S Curve of ", pred, ":", sep = ""),
                                b = paste("Carry Over and Power Curve of ", pred, ":", sep = ""),
                                c = paste(pred, " plus ", object.bis, sep = ""),
                                d = paste(pred, " minus ", object.bis, sep = ""),
                                e = paste(pred, " times ", object.bis, sep = ""),
                                f = paste(pred, " divided by ", object.bis, sep = ""),
                                g = paste("Logarithm on ", pred, sep = ""),
                                h = paste(pred, "to the square of ", object.bis, sep = ""),
                                i = paste(pred, "to the power of ", object.bis, sep = ""),
                                j = paste("Reciprocal of ", pred, sep =""),
                                k = paste("Time lag for ", pred, " by ", object.bis, " time unit(s)", sep = ""))
            
            coef.temp <- c(summary(fit.temp)$coefficients[length(coef(fit.temp)),1],summary(fit.temp)$coefficients[length(coef(fit.temp)),4],
                           summary(fit.temp)$r.squared, summary(fit.temp)$adj.r.squared)
            
            names(coef.temp) <- c("coef", "p-value", "r.squared", "adjusted.r.squared")
            
            cat(paste(" You choose to do: ", trans.opt, sep = ""), sep = "\n")
            
            if(opt == "a"){
              cat(
                paste(" The carry over rate is ", co.r, sep = ""),
                paste(" The lamda1 of S curve is ", sc.1, sep = ""), 
                paste(" The lamda2 of S curve is ", sc.2, sep = ""), sep = "\n")
            } else if(opt == "b"){
              cat(
                paste(" The carry over rate is ", co.r, sep = ""),
                paste(" The power rate is ", pc.r, sep = ""), sep = "\n")
            }
            
            cat(paste(" ", paste(rep("-",40),collapse = ""), sep = ""),
                paste(" The coefficient of ", pred, " in this model is ", round(coef.temp[1],4), sep = ""),
                paste(" The p-value of the coefficient is ", as.numeric(format(coef.temp[2],scientific=T)), sep = ""),
                paste(" The r-squared of the model is ", round(coef.temp[3],4), sep = ""),
                paste(" The adjusted r-squared of the model is ", round(coef.temp[4],4), sep = ""),
                "", sep = "\n")

            return(step2.4())
          } 
          
          # } # end repeat 2.4.2 & end OPTION 2.4.2
          
          
        }else if(opt2 == 3){
          # OPTION 2.4.3
          return(step2.2())
        }else{
          # opt2 == 4
          # OPTION 2.4.4
          return(step2.1())
        }
        
      } # end function step2.4()
      step2.4()
      #----------------------------------------------------------------------------
      # output of step 2.4
      # df.temp & fit.temp after confirming a predictor and its transformation
      # parameter history to be generated / tracked (unwritten yet)
      if(opt1 %in% c("1.1", "1.2", "1.3", "0", "2.2", "2.5")){
        obj.name <<- NULL
      } else {
        obj.name <<- obj2
      }
      
    } # end function step2.2()
    step2.2()
    
    #----------------------------------------------------------------------------
    # STEP2.5
    # store df.temp & fit.temp to df1 & fit1 for futher comparison
    df1 <<- df.temp
    fit1 <<- fit.temp
    
    null_to_na <- function(a){
      if(is.null(a)) a <- NA
      return(a)
    }
    s.prmt <- sapply(list(opt.recom, co.r.recom, sc.1.recom, sc.2.recom, pc.r.recom, obj.name), 
                        null_to_na)
    
    prmt.temp <- data.frame(pred.i, type = s.prmt[1], co.r = s.prmt[2], 
                            sc.1 = s.prmt[3], sc.2 = s.prmt[4], 
                            pc.r = s.prmt[5], object = s.prmt[6], 
                            status = "alive", stringsAsFactors = F)
    prmt <<- rbind(prmt, prmt.temp)
    
    #----------------------------------------------------------------------------
    # STEP2.6
    # final confirm of the parameters and asking for next move
    step2.6 <<- function(){
      
      repeat{
        cat("What is the next move?",
            "  1. Continue modeling & go select another predictor!",
            "  2. Remove a predictor / an intercept from the model.",
            "  3. Add the intercept (I shouldn't have it removed...).",
            "  4. Stop! Show me the final model and the statistics!",
            "", sep = "\n")
        opt3 <- readline("Enter an option number please: ")
        cat("\n")
        if(!opt3 %in% as.character(1:4)){
          message("Only the number between 1 and 4 is acceptable, dude!")
          cat("\n")
        }else{
          opt3 <- as.numeric(opt3)
          break
        }
      } # end repeat for step2.6
      
      if(opt3 == 1){
        # OPTION 2.6.1 return to the very beginning - selecting a predictor
        return(step2.1())
        
      }else if(opt3 == 2){
        # OPTION 2.6.2
        step2.6.2 <- function(){
          #----------------------------------------------------------------------------
          # STEP 2.6.2.1
          # remove predictors / intercept from the model, 
          # store the modified df1 & fit1 to df.temp & fit.temp
          repeat{
            # repeat STEP 2.6.2.1
            message("You choose to remove a predictor / the intercept.", 
                    "The name of predictor (case sensitive) should be provided below.",
                    'If you want to delete the intercept, please enter "intercept".',
                    sep = "\n")
            cat("\n")
            var.tbr <- readline("Please tell me the one to be removed: ") 
            # define the predictor (or intercept) to be removed
            cat("\n")
            
            if(toupper(var.tbr) == "INTERCEPT"){
              if("(Intercept)" %in% names(coef(fit1))){
                # only execute when intercept does exist in the model
                fit.temp <<- trial(data = df1, resp, fit = fit1, action = -1)
                df.temp <<- df1 # nothing changed here, assign to df.temp for next step
                loop.output(resp, df.temp, fit.temp)
                warn(fit1 = fit1, fit2 = fit.temp, p.cons = 0.2)
                
                break
              }else{
                # if the intercept is already removed, pop up warning message
                message("The intercept does not exist in the model!")
                cat("\n")
                repeat{
                  cat("\n")
                  cont <- readline("Do you still want to remove a predictor / the intercept (Y/N)? ")
                  cat("\n")
                  if(cont == "N"){
                    return(step2.6())
                  }else if(!cont %in% c("Y","N")){
                    message('Only "Y" or "N" is acceptable!')
                    cat("\n")
                  }else{
                    break
                  }
                }
              }
            }else{
              if(!var.tbr %in% names(df.temp)){
                # if the name of predictor does not exist in the dataset, pop up warning message
                message(paste('The variable "', var.tbr, '" does not exist in the dataset!', sep = ""))
                cat("\n")
                repeat{
                  cat("\n")
                  cont <- readline("Do you still want to remove a predictor / the intercept (Y/N)? ")
                  cat("\n")
                  if(cont == "N"){
                    return(step2.6())
                  }else if(!cont %in% c("Y","N")){
                    message('Only "Y" or "N" is acceptable!')
                    cat("\n")
                  }else{
                    break
                  }
                }
                
              }else if(!var.tbr %in% names(coef(fit1))){
                # if the name of predictor does not exist in the model, pop up warning message
                message(paste('The variable "', var.tbr, '" does not exist in the model!', sep = ""))
                cat("\n")
                repeat{
                  cat("\n")
                  cont <- readline("Do you still want to remove a predictor / the intercept (Y/N)? ")
                  cat("\n")
                  if(cont == "N"){
                    return(step2.6())
                  }else if(!cont %in% c("Y","N")){
                    message('Only "Y" or "N" is acceptable!')
                    cat("\n")
                  }else{
                    break
                  }
                }
                
              }else{
                # remove the predictor when it does exist in the model
                fit.temp <<- trial(data = df1, resp, fit = fit1, action = -2, pred = var.tbr)
                df.temp <<- df1
                df.temp[[var.tbr]] <<- df0[[var.tbr]]
                message('The predictor "', var.tbr, '" is chosen to be removed.', sep = "")
                cat("\n")
                loop.output(resp, df.temp, fit.temp)
                warn(fit1 = fit1, fit2 = fit.temp, p.cons = 0.2)
                break
              }
            }
          } # end repeat OPTION 2.6.2.1
          # end STEP 2.6.2.1
          
          #----------------------------------------------------------------------------
          # STEP 2.6.2.2
          # asking for confirmation of the predictor / intercept deletion
          repeat{
            conf.rmv <- readline(paste('Could you confirm the removal of "', var.tbr, '" (Y/N)? ', sep = ""))
            cat("\n")
            if(toupper(conf.rmv) == "Y"){
              message(paste('The predictor "',var.tbr,'" is successfully removed!', sep = ""))
              cat("\n")
              df1 <<- df.temp
              fit1 <<- fit.temp
              
              # modify the status of predictor in prmt
              if(toupper(var.tbr) != "INTERCEPT"){
                pos.var <- which(prmt[,8] == "alive" & prmt[,1] == var.tbr)
                prmt[pos.var, 8] <<- "dead"
              }
              
              repeat{
                cont.rmv <- readline('Do you want to remove another element from the model (Y/N)? ')
                cat("\n")
                if(toupper(cont.rmv) == "Y"){
                  
                  return(step2.6.2())
                  
                }else if(toupper(cont.rmv) == "N"){
                  
                  return(step2.6())
                  
                }else{
                  message('Come on! Only "Y" or "N" is acceptable!')
                  cat("\n")
                }
              }
            }else if(toupper(conf.rmv) == "N"){
              cat("\n")
              return(step2.6())
            }else{
              message('Come on! Only "Y" or "N" is acceptable!')
              cat("\n")
            }
       
          }
          
        } # end function step2.6.2
        step2.6.2()

      }else if(opt3 == 3){
        # OPTION 2.6.3
        #----------------------------------------------------------------------------
        # STEP 2.6.3.1
        # add the intercept to the model
        if(!"(Intercept)" %in% names(coef(fit1))){
          #if intercept is not in the model, add it
          fit.temp <<- trial(data = df1, resp, fit = fit1, action = 1)
          df.temp <<- df1
          loop.output(resp, df.temp, fit.temp)
          warn(fit1 = fit1, fit2 = fit.temp, p.cons = 0.2)
          
        }else{
          #intercept exists in the model, warning message pops up and back to step 2.6()
          message("The intercept exisits in the model, no meed for implement it again!")
          cat("\n")
          return(step2.6())
        }
        
        #----------------------------------------------------------------------------
        # STEP 2.6.3.2
        # ask for confirmation of the intercept implementation
        repeat{
          conf.add <- readline("Could you confirm the implementation of the intercept (Y/N)? ")
          cat("\n")
          if(toupper(conf.add) == "Y"){
            fit1 <<- fit.temp
            df1 <<- df.temp  # nothing changed in df, assigned to df1 for next loop
            return(step2.6())
          }else if(toupper(conf.add) == "N"){
            return(step2.6())
          }else{
            message('Come on! Only "Y" or "N" is acceptable!')
            cat("\n")
          }
        }
        
      }else{
        # if opt3 == 4
        # OPTION 2.6.4
        # Final Output
        final.output(resp, data = df1, fit = fit1, prmt, contri)
        repeat{
          lnch.aic <- readline("Do you want to perform a stepwise regression check - stepAIC (Y/N)? ")
          cat("\n")
          if(toupper(lnch.aic) == "Y"){
            fit1 <- update(fit1, data = df1) 
            # specify data = df1 for further stepAIC use
            fit.aic <- stepAIC(fit1, trace = F)
            loop.output(resp, df1, fit.aic)
            warn(fit1 = fit1, fit2 = fit.aic, p.cons = 0.2)
            pos.aic <- which(!names(coef(fit1)) %in% names(coef(fit.aic)))
            prmt.aic <- prmt
            if(length(pos.aic)!=0){
              prmt.aic[which(prmt[,1] %in% names(coef(fit1))[pos.aic]&prmt[,8]=="alive"),8]<-"dead"
            }
            
            final.output(resp, data = df1, fit = fit.aic, prmt.aic, contri, aic = T)
            
            message("Thanks for using AutoReg system! See you around!")
            cat("\n")
            break
            # end of the program
          }else if(toupper(lnch.aic) == "N"){
            message("Thanks for using AutoReg system! See you around!")
            cat("\n")
            break
            # end of the program
          }else{
            message('Come on! Only "Y" or "N" is acceptable!')
            cat("\n")
          }
        }
      }
       
    } # end function step2.6()
    step2.6()
    
  }  # end function step2.1()
  step2.1()
  
  rm(list=ls())
  
  
# CREATION on Thursday 11/13/2014: STEP 1.1 - 2.1
# UPDATE on Monday 11/17/2014: STEP 2.2 - 2.3
# UPDATE on Tuesday 11/18/2014: STEP 2.4 - 2.6
# UPDATE + ISSUE SHOOTING on Wednesday 11/19/2014: STEP 2.6
# ISSUE SHOOTING on Thursday 11/20/2014
#   * unable to add new predictor to the model
#   * unable to change the predictor status to "dead" after removal
#   * transformation option 0 (no trans) is not considered in modif() and prmt recording
#   * transformation cannot be done by reading historical prmt.csv (in rebuild())
# UPDATE on Monday 11/24/2014
#   * if(!is.na(transformation object)) create new variable, variable name indicates trans-method
#   * clear the environment after the whole modeling process ends
#
# NEED TO BE MODIFIED IN VERSION 2.1
#   * if variable already exists in the model, then bt() cannot be used to do transformation
#   * predictor can be removed right after an existed model is imported

}

