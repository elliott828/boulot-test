AutoReg <- function(data){
  
  #####################
  ## MODULE 1 STARTS ##
  #####################
  
  #++++++++++++++++++++++++++++#
  #        SUB MODULE 1.1      #
  # Welcome & Preparation Part #
  #++++++++++++++++++++++++++++#

  # Check the data format.         
  # "*.csv" is preferable.        
  # If no .csv, exit the function. 

  data <- as.character(data)
  endstr <- substr(data, nchar(data)-2, nchar(data))
  
  if (endstr != "csv"){
    stop(
      paste("Please prepare the raw data in .csv format,",
            "and don't forget to put the file name between double quotation mark, honey!",
            "Come on! Start over again!",
            sep = "\n"))
  }else{
    df <- read.csv(data)
  }
  
  message("Welcome to AutoReg!")
  message("Please follow instructions to finish the regression.")
  message(paste('You can press the button "Esc" to quit this program.',
                '', sep = "\n"))
  cat("Before using this program, please make sure following files are available in the working directory or the internet is connectable:",
      "  1. BasicTrans.R",
      "  2. ParaS.R",
      "  3. Modif.R", sep = "\n")
  conf <- readline("Can you confirm (Y/N)? ")
  if(conf == "N") stop("Get them prepared!")
  
  # Load basic self-defined functions
  if(file.exists("BasicTrans.R")){
    source("BasicTrans.R")
  }else{
    source("https://raw.githubusercontent.com/elliott828/boulot-test/master/BaiscTrans.R")
  }
  
  if(file.exists("ParaS.R")){
    source("ParaS.R")
  }else{
    source("https://raw.githubusercontent.com/elliott828/boulot-test/master/ParaS.R")
  }
  
  if(file.exists("Modif.R")){
    source("Modif.R")
  }else{
    source("https://raw.githubusercontent.com/elliott828/boulot-test/master/Modif.R")
  }
  
  # Load packages
  if (!"car" %in% rownames(installed.packages())) install.packages("car")
  if (!"zoo" %in% rownames(installed.packages())) install.packages("zoo")
  if (!"lmtest" %in% rownames(installed.packages())) install.packages("lmtest")
  library(car)     # 
  library(stats)   # for update() & as.formula()
  library(zoo)     # for library(lmtest) 
  library(lmtest)  # for durbin-watson test
  library(MASS)    # for stepAIC()
  
  # Basic settings
  options(warn = 1)
  fit <- NULL  # Assign NULL to model first
  # set up an empty data frame
  prmt <- data.frame(variable = character(),
                     co.rate = numeric(),
                     pc.rate = numeric(),
                     sc.rate1 = numeric(),
                     sc.rate2 = numeric(),
                     status = character(),  #indicate the availability of variable for the model
                     stringsAsFactors = F)
  
  #++++++++++++++++++++++++++++++++++#
  #          SUB MODULE 1.2          #
  # Resp.Variable & Intercept Config #
  #++++++++++++++++++++++++++++++++++#
  
  # Specify the response variable
  repeat{
    resp <- as.character(readline("Please enter the name of response variable: "))
    check1 <- resp %in% names(df)
    if (check1 == TRUE){
      break
    }else{
      warning(paste('The variable "', resp, '" does not exist in the database!', sep=""))
    }    
  }

  ndf <- df   # Prepare a new df for further data modification
  
  # Specify the intercept - asking the necessity of keeping intercept
  repeat{
    incpt <- readline("Do you want to keep the intercept in the model (Y/N)? ")
    if (toupper(incpt) == "Y"){
      incpt <- "Y"
      break
    }else if (toupper(incpt) == "N"){
      incpt <- "N"
      break
    }else{
      warning("Would you please give me a clear answer, Y or N?")
    }
  }

  #####################
  ##  MODULE 1 ENDS  ##
  #####################
  
  #===============================================================================================#
  
  #####################
  ## MODULE 2 STARTS ##
  #####################
  
  # Predictor | Predictor transformation test
  
  #+++++++++++++++++++++#
  #   SUB MODULE 2.1    #
  # Predictor Selection #
  # FUNCTION pred.sel() #
  #+++++++++++++++++++++#
  pred.sel <- function(){
    
    #------------------------------#
    #       KEY MODULE 2.1.1       #
    # Transformation method select #
    #------------------------------#
    
    co.r <- NaN   # carry over rate
    pc.r <- NaN   # power curve rate
    sc.1 <- NaN   # S curve rate 1 (lamda1)
    sc.2 <- NaN   # S curve rate 2 (lamda2)
    t.lag <- NaN  # no. of time lag
    
    # Specify the predictor
    repeat{
      # loop 2.1.1.1
      
      pred <- as.character(readline("Please enter the name of predictor: "))
      check1 <- pred %in% names(ndf)
      if (check1 == TRUE){
        break
      }else{
        warning(paste('The predictor "', pred, '" does not exist in the database!', sep=""))
      }
      # end of loop 2.1.1.1
    }
    
    # Choose the transforming method (loop)
    repeat{
      # loop 2.1.1.2
      
      cat(
        paste('Which kind of transformation does the variable "', pred, '" need to be adapted?', sep=""),
        "Media methods:",
        "  1. carry over + power curve",
        "  2. carry over + S curve",
        "  3. auto-selection between 1 and 2",
        "And others:",
        "  4. no transformation",
        "  5. time lag",
        "  6. other basic transformation",
        "",sep="\n")
      
      # assign transforming method to opt1
      opt1 <- as.numeric(readline("Please enter an option number: "))
      if(opt1 %in% 1:6){
        break
      }else{
        warning("Man (or u r a woman/girl...)! Only 4 options!")
      }
      
      # end of loop 2.1.1.2
    }
    #------------------------------#
    #     END KEY MODULE 2.1.1     #
    # Transformation method select #
    #------------------------------#
    
    #..................................................................................................#
    
    #-------------------------#
    #     KEY MODULE 2.1.2    #
    # Parameters Confirmation #
    #-------------------------#
    trans.meth <- function(){
      # START: trans.meth()
      
      # Check the simulated modeling result.
      # Confirm the recommended parameters,
      # or specify other parameters.
      
      if (opt1 %in% c(1:4)){
        
        #** START: if.else.flow 2.1.2.1 (opt1 = 1~4) **#
        
        #ParaS(pred, resp, df, model = NULL, method = 3)
        offer <- (ParaS(pred, resp, ndf, fit, opt1))
        message("Per below you could find the statistics if we transform and model based on your choice:")
        print(offer)
        
        # Allocate the best combination of parameters
        if (opt1 == 1){
          co.r <- offer$best.c.p.transform.parameters[1]
          pc.r <- offer$best.c.p.transform.parameters[2]
          cat(
            "Program recommends those parameter values:",
            paste("carry over rate = ", co.r, sep=""),
            paste("power curve parameter = ", pc.r, sep=""),
            " ",
            "Do you agree to transform the variable with recommended parameters?",
            "  1. Yes",
            "  2. No", 
            " ",sep="\n")
          opt2 <- as.numeric(readline("Hurry up! 1 or 2? "))
          
          if(opt2 == 2){
            co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
            pc.r <- as.numeric(readline("Please suggest alternative power curve parameter: "))
            
          }else if(!opt2 %in% 1:2){
            warning("Please enter the right number (1 or 2)!")
          }
          
        }else if(opt1 == 2){
          co.r <- offer$best.c.s.transform.parameters[1]
          sc.1 <- offer$best.c.s.transform.parameters[2]
          sc.2 <- offer$best.c.s.transform.parameters[3]
          cat(          
            "Program recommends those parameter values:",
            paste("carry over rate = ", co.r, sep=""),
            paste("1st S curve parameter = ", sc.1, sep=""),
            paste("2nd S curve parameter = ", sc.2, sep=""),
            " ",
            "Do you agree to transform the variable with recommended parameters?",
            "  1. Yes",
            "  2. No", 
            " ", sep="\n")
          opt2 <- as.numeric(readline("Hurry up! 1 or 2? "))
          
          if(opt2 == 2){
            co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
            sc.1 <- as.numeric(readline("Please suggest alternative value for the 1st S curve parameter: "))
            sc.2 <- as.numeric(readline("Please suggest alternative value for the 2nd S curve parameter: "))
            
          }else if(!opt2 %in% 1:2){
            warning("Please enter the right number (1 or 2)!")
          }
          
        }else if(opt1 == 3){
          cat("Which approach do you prefer?",
              "  1. carry over + power curve",
              "  2. carry over + S curve",
              " ", sep = "\n")
          appr <- as.numeric(readline("which one is preferred, 1 or 2? "))
          
          if (appr == 1){
            co.r <- offer$best.c.p.transform.parameters[1]
            pc.r <- offer$best.c.p.transform.parameters[2]          
            cat(
              "Program recommends those parameter values:",
              paste("carry over rate = ", co.r, sep=""),
              paste("power curve parameter = ", pc.r, sep=""),
              " ",
              "Do you agree to transform the variable with recommended parameters?",
              "  1. Yes",
              "  2. No", 
              " ",sep="\n")
            opt2 <- as.numeric(readline("Hurry up! 1 or 2? "))
            
            if(opt2 == 2){
              co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
              pc.r <- as.numeric(readline("Please suggest alternative power curve parameter: "))
              
            }else if(!opt2 %in% 1:2){
              warning("Please enter the right number (1 or 2)!")
            }
            
          }else if (appr == 2){
            co.r <- offer$best.c.s.transform.parameters[1]
            sc.1 <- offer$best.c.s.transform.parameters[2]
            sc.2 <- offer$best.c.s.transform.parameters[3]
            cat(
              "Program recommends those parameter values:",
              paste("carry over rate = ", co.r, sep = ""),
              paste("1st S curve parameter = ", sc.1, sep = ""),
              paste("2nd S curve parameter = ", sc.2, sep = ""),
              " ",
              "Do you agree to transform the variable with recommended parameters?",
              "  1. Yes",
              "  2. No", 
              " ", sep="\n")
            opt2 <- as.numeric(readline("Hurry up! 1 or 2? "))
            
            if(opt2 == 2){
              co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
              sc.1 <- as.numeric(readline("Please suggest alternative value for the 1st S curve parameter: "))
              sc.2 <- as.numeric(readline("Please suggest alternative value for the 2nd S curve parameter: "))
              
            }else if(!opt2 %in% 1:2){
              warning("Please enter the right number (1 or 2)!")
            }
            
          }else{
            warning("Man (or you are a woman/girl...)! Only 2 options!")
          }
        }
        
        #** END: if.else.flow 2.1.2.1 (opt1 = 1~4) **#
      }
#      else if (opt1 == 5){
#        #** START: if.else.flow 2.1.2.2 (opt1 = 5) **#
#        
#        
#
#        #** END: if.else.flow 2.1.2.2 (opt1 = 5) **#
#      }
      else if (opt1 == 6){
        #** START: if.else.flow 2.1.2.3 (opt1 = 6) **#
        
        if (! class(ndf[[pred]]) %in% c("numeric","integer")){
          warning(paste('The predictor "', pred, '" is not a numeric / an integer data serie!', sep = ""))
          return(pred.sel())
        }
        
        repeat{
          #** START: loop basic transformation method **#
          
          repeat{
            cat(
              "The available basic transforming method in this program: ",
              "  1. logrithm (log)",
              "  2. root (x^y)",
              "  3. additoin (+)",
              "  4. substraction (-)",
              "  5. multiplication (*)",
              "  6. division (/)",
              "  7. reciprocal (1/x)",
              " ", sep = "\n"
            )
            b.trans <- as.numeric(readline("Please tell me your choice (1-7): "))
            if (b.trans %in% 1:7){
              break
            }else{
              warning("Only numbers between 1 and 7 (inclusive) are acceptable!")
            }
          }
          
          if (b.trans == 1){
            #** START: if.else.flow 2.1.2.3 - 1 (b.trans = 1) **#
            
            # firstly check the class of the variable, make sure it's numeric
            # secondly, check and make sure elements of the variable are all postive numbers
            # otherwise return to the step of predictor selection
            if (any(ndf[[pred]]<=0) == TRUE){
              warning("The predictor cannot be transformed by log because it contains 0 or negative numbers!")
              return(pred.sel())
            }else{
              repeat{
                default.base <- readline("Do you want to use default base number - natural base e (Y/N): ")
                if (toupper(default.base) == "Y"){
                  new.pred <- paste("log.e.", pred, sep = "")
                  ndf[[new.pred]] <- log(ndf[[pred]])
                  message(paste('New variable "', new.pred, '" is inserted in current database!'))
                  break
                }else if (toupper(default.base) == "N"){
                  repeat{
                    log.base <- as.numeric(readline("Please enter the base number: "))
                    if (is.numeric(log.base) == F){
                      warning("Please enter a positive number!")
                    }else if(log.base <= 0){
                      warning("Only positive number is acceptable!")
                    }else{
                      new.pred <- paste("log.", log.base, ".", pred, sep = "")
                      ndf[[new.pred]] <- log(ndf[[pred]], log.base)
                      message(paste('New variable "', new.pred, '" is inserted in current database!'))
                      break
                    }
                  }
                  break
                }else{
                  warning("Please only enter Y or N!")
                }
              }
            }
            
            #** END: if.else.flow 2.1.2.3 - 1 (b.trans = 1) **#
          }else if (b.trans == 2){
            #** START: if.else.flow 2.1.2.3 - 2 (b.trans = 2) **#
            
            repeat{
              exponent <- as.numeric(readline("Please provide the exponent: "))
              if (exponent%%1 != 0 & any(ndf[[pred]]<0)){
                warning(paste('The exponent does not work because the predictor "', pred, 
                              '" contains negative numbers!', sep = ""))
                return(pred.sel())
              }else if (exponent <= 0 & any(ndf[[pred]]==0)){
                warning(paste('The exponent does not work because the predictor "', pred, 
                              '" contains 0!', sep = ""))
                return(pred.sel())
              }else{
                new.pred <- paste("exp.", exponent, ".", pred, sep = "")
                ndf[[new.pred]] <- (ndf[[pred]])^exponent
                message(paste('New variable "', new.pred, '" is inserted in current database!'))
                break
              }
            }
            
            #** END: if.else.flow 2.1.2.3 - 2 (b.trans = 2) **#
          }else if (b.trans == 7){
            #** START: if.else.flow 2.1.2.3 - 3 (b.trans = 7) **#
            
            if (any(ndf[[pred]] == 0) == TRUE){
              warning(paste('The reciprocal is not computable because the predictor "', pred, 
                            '" contains 0!', sep = ""))
              return(pred.sel())
            }else{
              new.pred <- paste("reciprocal.", pred, sep = "")
              ndf[[new.pred]] <- 1/(ndf[[pred]])
              message(paste('New variable "', new.pred, '" is inserted in current database!'))
            }
            
            #** END: if.else.flow 2.1.2.3 - 3 (b.trans = 7) **#
          }else{
            #** START: if.else.flow 2.1.2.3 - 4 (b.trans = 3:6) **#
            
            repeat{
              # START loop 2.1.2.3 - 4 number choosing
              
              cat(
                paste('Which number do you want to use for transforming predictor "', pred, '"? ', sep = ""),
                '  1. I will enter a number',
                '  2. I will choose another predictor',
                '  3. The minimum value of this predictor',
                '  4. The maximum value of this predictor',
                '  5. The mean value of this predicotr',
                "", sep = "\n"
              )
              b.trans.num <- as.numeric(readline("Pick up a number between 1 and 5 please: "))
              
              if (!b.trans.num %in% 1:5){
                warning("I'm tired, could you enter the right number...? Appreciated!")
              }else{
                # START: loop 2.1.2.3 - 4; if.else.flow: sub level-1
                
                if (b.trans.num == 1){
                  # START: loop 2.1.2.3 - 4; if.else.flow: sub level-2.1
                  
                  repeat{ # loop sub level 2.1
                    num <- as.numeric(readline("Please enter the number: "))
                    if (num == 0){
                      warning("The number should not be 0!")
                    }else{
                      if (b.trans == 3){
                        new.pred <- paste(pred, ".Add.", num, sep = "")
                        ndf[[new.pred]] <- ndf[[pred]] + num
                      }else if (b.trans == 4){
                        new.pred <- paste(pred, ".Minus.", num, sep = "")
                        ndf[[new.pred]] <- ndf[[pred]] - num
                      }else if (b.trans == 5){
                        new.pred <- paste(pred, ".Times.", num, sep = "")
                        ndf[[new.pred]] <- ndf[[pred]] * num
                      }else if (b.trans == 6){
                        new.pred <- paste(pred, ".Div.by.", num, sep = "")
                        ndf[[new.pred]] <- ndf[[pred]] / num
                      }
                      break  # break loop sub level 2.1
                    }
                  }
                  break  # break loop 2.1.2.3 - 4 number choosing
                  
                  # END: loop 2.1.2.3 - 4; if.else.flow: sub level-2.1
                }else if (b.trans.num == 2){
                  # START: loop 2.1.2.3 - 4; if.else.flow: sub level-2.2
                  
                  repeat{
                    # START: loop sub level-2.2.3
                    
                    repeat{
                      pred.bis <- readline("Please enter the predictor's name: ")
                      if(pred.bis %in% names(ndf)){
                        if(length(ndf[[pred]])==length(ndf[[pred.bis]])){
                          break
                        }else{
                          warning("The length of 2 predictors are not the same!")
                        }
                      }else{
                        warning("This predicotr does not exist in dataset!")
                      }
                    }
                    num <- pred.bis
                    if (b.trans == 3){
                      new.pred <- paste(pred, ".Add.", num, sep = "")
                      ndf[[new.pred]] <- ndf[[pred]] + num
                      break  # break loop sub level-2.2.3
                    }else if (b.trans == 4){
                      new.pred <- paste(pred, ".Minus.", num, sep = "")
                      ndf[[new.pred]] <- ndf[[pred]] - num
                      break  # break loop sub level-2.2.3
                    }else if (b.trans == 5){
                      new.pred <- paste(pred, ".Times.", num, sep = "")
                      ndf[[new.pred]] <- ndf[[pred]] * num
                      break  # break loop sub level-2.2.3
                    }else if (b.trans == 6){
                      if (any(num)==0){
                        warning("This predictor contains 0, it cannot be used as denominator!")
                      }else{
                        new.pred <- paste(pred, ".Div.by.", num, sep = "")
                        ndf[[new.pred]] <- ndf[[pred]] / num
                        break  # break loop sub level-2.2.3
                      }
                    }
                    
                    # END: loop sub level-2.2.3
                  }
                  break  # break loop 2.1.2.3 - 4 number choosing
                  
                  # END: loop 2.1.2.3 - 4; if.else.flow: sub level-2.2
                }else if (b.trans.num == 3){
                  # START: loop 2.1.2.3 - 4; if.else.flow: sub level-2.3
                  
                  num <- min(ndf[[pred]])
                  if (b.trans == 3){
                    new.pred <- paste(pred, ".Add.Min", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] + num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 4){
                    new.pred <- paste(pred, ".Minus.Min", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] - num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 5){
                    new.pred <- paste(pred, ".Times.Min", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] * num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 6){
                    if (any(num)==0){
                      warning("This predictor contains 0, it cannot be used as denominator!")
                    }else{
                      new.pred <- paste(pred, ".Div.by.Min", sep = "")
                      ndf[[new.pred]] <- ndf[[pred]] / num
                      break  # break loop 2.1.2.3 - 4 number choosing
                    }
                  }
                  
                  # END: loop 2.1.2.3 - 4; if.else.flow: sub level-2.3
                }else if (b.trans.num == 4){
                  # START: loop 2.1.2.3 - 4; if.else.flow: sub level-2.4
                  
                  num <- max(ndf[[pred]])
                  if( b.trans == 3){
                    new.pred <- paste(pred, ".Add.Max", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] + num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 4){
                    new.pred <- paste(pred, ".Minus.Max", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] - num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 5){
                    new.pred <- paste(pred, ".Times.Max", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] * num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 6){
                    if (any(num)==0){
                      warning("This predictor contains 0, it cannot be used as denominator!")
                    }else{
                      new.pred <- paste(pred, ".Div.by.Max", sep = "")
                      ndf[[new.pred]] <- ndf[[pred]] / num
                      break  # break loop 2.1.2.3 - 4 number choosing
                    }
                  }
                  
                  # END: loop 2.1.2.3 - 4; if.else.flow: sub level-2.4
                }else{
                  # START: loop 2.1.2.3 - 4; if.else.flow: sub level-2.5
                  
                  num <- mean(ndf[[pred]])
                  if (b.trans == 3){
                    new.pred <- paste(pred, ".Add.Mean", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] + num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 4){
                    new.pred <- paste(pred, ".Minus.Mean", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] - num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 5){
                    new.pred <- paste(pred, ".Times.Mean", sep = "")
                    ndf[[new.pred]] <- ndf[[pred]] * num
                    break  # break loop 2.1.2.3 - 4 number choosing
                  }else if (b.trans == 6){
                    if (any(num)==0){
                      warning("This predictor contains 0, it cannot be used as denominator!")
                    }else{
                      new.pred <- paste(pred, ".Div.by.Mean", sep = "")
                      ndf[[new.pred]] <- ndf[[pred]] / num
                      break  # break loop 2.1.2.3 - 4 number choosing
                    }
                  }
                  
                  # END: loop 2.1.2.3 - 4; if.else.flow: sub level-2.5
                }
                message(paste('New variable "', new.pred, '" is inserted in current database!'))
                
                # END: loop 2.1.2.3 - 4; if.else.flow: sub level-1
              }
              
              # END: loop 2.1.2.3 - 4 number choosing
            }
            
            #** END: if.else.flow 2.1.2.3 - 4 (b.trans = 3:6) **#
          }
          
          offer <- ParaS(new.pred, resp, ndf, fit, 4)
          message("Per below you could find the statistics if we transform and model based on your choice:")
          print(offer)
          
          repeat{
            cat(
              "Are you OK with the transformation?",
              "  1. Yes.",
              "  2. No. I want to try other parameters on same variable.",
              "", sep = "\n")
            
            opt3 <- as.numeric(readline("Make your choice darling, 1 or 2? "))
            
            if (!opt3 %in% 1:2){
              warning("Babe, only 1 and 2 are acceptable...Tell me your choice again~")
            }else{
              break
            }
          }
          
          if (opt3 == 1){
            break
          }else{
            ndf <- ndf[, 1:(ncol(ndf)-1)]
          }
          
          #** END: loop basic transformation method **#
        }
        
        #** END: if.else.flow 2.1.1.3 (opt1 = 6) **#
      }
      
    #-------------------------#
    #   END KEY MODULE 2.1.2  #
    # Parameters Confirmation #
    #-------------------------#
    
    #..................................................................................................#
    
    #-------------------------#
    #     KEY MODULE 2.1.3    #
    #  Create / Update Model  #
    #-------------------------#
      
      if (opt1 %in% c(1:4)){
        #** START: if.else.flow 2.1.3.1 (opt1 = 1~4) **#
        
        repeat{
          #** START: opt1~4 - loop of repeating trial for other parameters **#
          
          # ---------------------------------------------------------------- #
          # repeat when user wants to try other parameters on same predictor #
          # otherwise this loop is broken                                    #
          # ---------------------------------------------------------------- #
          
          ndf <- Modif(pred, ndf, co.r, pc.r, sc.1, sc.2)
          
          # Build the model
          if (is.null(fit)){
            if (incpt == "Y"){
              fit <- lm(as.formula(sprintf('%s ~ %s', resp, pred)), data = ndf, na.action = na.exclude)
            }else{
              fit <- lm(as.formula(sprintf('%s ~ %s + 0', resp, pred)), data = ndf, na.action = na.exclude)
            }
            
          }else{
            fit <- update(fit, as.formula(sprintf('~. + %s', pred)), data = ndf)
          }
          message("Per below you could find the summary of updated model: ")
          print(summary(fit))
          
          repeat{
            cat("Are you OK with the chosen variable and its transformation?",
                "  1. Yes",
                "  2. No, I want to try other parameters",
                "  3. No, I want to try another variable",
                " ", sep = "\n")
            ok.trans <- as.numeric(readline("Make your choice, 1, 2 or 3? "))
            if(ok.trans %in% 1:3){
              break
            }else{
              warning("There is no option other than 1, 2 and 3!")
            }
          }
          
          if (ok.trans == 2){
            ndf[[pred]] <- df[[pred]]
            
            repeat{
              cat("Which approach do you prefer?",
                  "  1. carry over + power curve",
                  "  2. carry over + s curve",
                  " ", sep = "\n")
              appr <- as.numeric(readline("which one is preferred, 1 or 2? "))
              
              if (appr == 1){
                co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
                pc.r <- as.numeric(readline("Please suggest alternative power curve parameter: "))
                sc.1 <- NaN
                sc.2 <- NaN
                break
              }else if (appr ==2){
                co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
                sc.1 <- as.numeric(readline("Please suggest alternative value for the 1st s curve parameter: "))
                sc.2 <- as.numeric(readline("Please suggest alternative value for the 2nd s curve parameter: "))
                pc.r <- NaN
                break
              }else{
                warning("Please enter the right number (1 or 2)!")
              }
            }
          }else{
            break  # break the loop of repeating trial for other parameters
          }
          
          #** END: opt1~4 - loop of repeating trial for other parameters **#
        }
        
        if (ok.trans == 1){
          prmt[nrow(prmt)+1, ] <- c(pred, co.r, pc.r, sc.1, sc.2, "alive")
          # only when ok.trans = 1, the parameters can be added into "prmt"
        }else if (ok.trans == 3){
          fit <- update(fit, as.formula(sprintf('~. - %s', pred)))
          ndf[[pred]] <- df[[pred]]
          return(pred.sel())
        }
        
        #** START: if.else.flow 2.1.3.1 (opt1 = 1~4) **#
      }else if (opt1 %in% 5:6){
        #** START: if.else.flow 2.1.3.2 (opt1 = 6) **#
        
        repeat{
          #** START: opt5~6 - loop of repeating trial for other parameters **#
          
          if (is.null(fit)){
            if (incpt == "Y"){
              fit <- lm(as.formula(sprintf('%s ~ %s', resp, new.pred)), data = ndf, na.action = na.exclude)
            }else{
              fit <- lm(as.formula(sprintf('%s ~ %s + 0', resp, new.pred)), data = ndf, na.action = na.exclude)
            }
          }else{
            fit <- update(fit, as.formula(sprintf('~. + %s', new.pred)), data = ndf)
          }
          message("Per below you could find the summary of updated model: ")
          print(summary(fit))
          
          repeat{
            cat("Are you OK with the chosen variable and its transformation?",
                "  1. Yes",
                "  2. No, I want to try other parameters",
                "  3. No, I want to try another variable",
                " ", sep = "\n")
            ok.trans <- as.numeric(readline("Make your choice, 1, 2 or 3? "))
            if(ok.trans %in% 1:3){
              break
            }else{
              warning("There is no option other than 1, 2 and 3!")
            }
          }
          
          if (ok.trans == 2){
            ndf <- ndf[, 1:(ncol(ndf)-1)]
            return(trans.meth())
          }else{
            break
          }
          
          #** END: opt5~6 - loop of repeating trial for other parameters **#
        }
        
        if (ok.trans == 1){
          prmt[nrow(prmt)+1, ] <- c(new.pred, co.r, pc.r, sc.1, sc.2, "alive")
          # only when ok.trans = 1, the parameters can be added into "prmt"
        }else if (ok.trans == 3){
          fit <- update(fit, as.formula(sprintf('~. - %s', new.pred)))
          ndf <- ndf[, 1:(ncol(ndf)-1)]
          return(pred.sel())
        }

        #** END: if.else.flow 2.1.3.2 (opt1 = 5~6) **#
      }
      
    #-------------------------#
    #  END KEY MODULE 2.1.3   #
    #  Create / Update Model  #
    #-------------------------#
    
    #..................................................................................................#
    
    #-------------------------#
    # START KEY MODULE 2.1.4  #
    #   Model Final Confirm   #
    #-------------------------#
    
    repeat{
      # Loop: final confirm of a loop
      repeat{
        cat("Do you want to continue testing new predictors?",
            "  1. Yes, I do (but I'm not marrying you!)",
            "  2. Yes, but I want to remove a predictor/the intercept from the model first",
            "  3. No, Please show me the final model statistics (I had enough!)",
            "  4. I want to add the intercept to current model",
            " ", sep = "\n")
        
        opt4 <- as.numeric(readline("Your choice: 1, 2, 3 or 4? "))
        if (opt4 %in% 1:4){
          break
        }else{
          warning("Please go back to your primary school and complete the basic math course!")
        }
      }
      
      if (opt4 == 3){
        message("Per below you could check the statistics of final model:")
        print(summary(fit))
        
        repeat{
          prmt.exp <- readline("Do you want to export the parameters history (Y/N)? ")
          
          if (toupper(prmt.exp) == "Y"){
            write.csv(prmt, paste(getwd(), "/prmt.csv", sep = ""))
            message('Variable parameters history is exported to "prmt.csv" under default working directory')
            break
          }else if (toupper(prmt.exp) == "N"){
            break
          }else{
            warning("Only Y or N is acceptable!")
          }
        }
        
        # add AIC test
        repeat{
          bin2 <- readline("Do you want to perform stepwise selection on current model by AIC (Y/N)? ")
          
          if (toupper(bin2) == "Y"){
            fit.step <- stepAIC(fit, trace =F)
            print(summary(fit.step))
            
            repeat{
              prmt.step.exp <- readline("Do you want to export the parameters history after stepwise selection (Y/N)? ")
              if (toupper(prmt.step.exp) == "Y"){
                step.coef.list <- rownames(coef(summary(fit.step)))
                step.coef.list <- step.coef.list[step.coef.list!="(Intercept)"]
                # print(step.coef.list)
                # print(which(!prmt[,1] %in% step.coef.list))
                prmt[which((!prmt[,1] %in% step.coef.list) & (prmt[,6] == "alive")), 6]<- "step.removed" 
                write.csv(prmt, paste(getwd(), "/prmt.step.csv", sep = ""))
                message('Variable parameters history is exported to "prmt.step.csv" under default working directory')
                break
              }else if (toupper(prmt.step.exp) == "N"){
                break
              }else{
                warning("Only Y or N is acceptable!")
              }
              
            }
            break
          }else if (toupper(bin2) == "N"){
            break
          }else{
            warning("Only Y or N is acceptable!")
          }
          
        }
        
        # TO BE IMPLEMENTED: print other possible statistics (MAPE, contribution rate, dwtest, etc.)
        
        break # break upper loop
        
      }else if (opt4 == 2){
        
        repeat{
          # Loop: remove predictors / intercept
          message('If you want to remove intercept, please enter "Intercept",')
          message('otherwise, please enter the name of predictor.')
          pred.rm <- as.character(readline("Which one to be removed from the model? "))
          
          if (tolower(pred.rm) == "intercept"){
            pred.rm <- "(Intercept)"
          }
          
          check1 <- pred.rm %in% names(coef(fit))
          if (check1 == FALSE & pred.rm != "(Intercept)"){
            warning(paste("The predictor '", pred.rm, "' does not exist in the model!", sep=""))
          }else if(check1 == FALSE & pred.rm == "(Intercept)"){
            warning("The intercept is already removed from current model!")
          }else if(check1 == TRUE & pred.rm != "(Intercept)"){
            
            fit <- update(fit, as.formula(sprintf('~. - %s', pred.rm)))
            ndf[[pred.rm]] <- df[[pred.rm]]
            
            # change the variable status in 'prmt' from 'alive' to 'dead'
            pos <- which((prmt[,1] == pred.rm) & (prmt[,6] == "alive"))
            prmt[pos,6] <- "dead"
            
            message("Per below you could check the updated model after the predictor removed:")
            print(summary(fit))
            
            bin <- as.character(readline("Still want to remove predictor / intercept (Y/N)? "))
            
          }else{
            
            fit <- update(fit, ~. -1)
            message("Per below you could check the updated model after the predictor removed:")
            print(summary(fit))
            
            bin <- as.character(readline("Still want to remove predictor / intercept (Y/N)? "))
            
          }
          
          if (toupper(bin) == "N"){
            message("Let's continue modeling with other predictors(OMG)...")
            break # break upper loop: remove predictors / intercept
          }
        }
        
      }else if (opt4 == 4){
        if ("(Intercept)" %in% rownames(coef(fit))){
          warning("Intercept already exists in the model! No need to add it!")
          break # break upper loop: remove predictors / intercept
        }else{
          fit <- update(fit, ~. + 1)
          message("Per below you could check the updated model after the intercept added:")
          print(summary(fit))
        }
        
      }else{
        return(pred.sel())
        break #break upper loop: final confirm of a loop
      }
    }
    
    # START: trans.meth() - start from line 193
    }
    
    
    #-------------------------#
    #  END KEY MODULE 2.1.4   #
    #   Model Final Confirm   #
    #-------------------------#
    
    trans.meth() 
    
  }  # END: function pred.sel() - start from line 129
  
  #+++++++++++++++++++++++#
  # END OF SUB MODULE 2.1 #
  #+++++++++++++++++++++++#
  
  pred.sel() # call this function to let it run / treat it like a loop
  
  #####################
  ##  MODULE 2 ENDS  ##
  #####################
  
}



