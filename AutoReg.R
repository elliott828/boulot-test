AutoReg <- function(data){
  
  ###############################
  ## refer to the process flow ##
  ###############################
  
  message("Welcome to AutoReg!")
  message("Please follow instructions to finish the regression.")
  message(paste('You can press the button "Esc" to quit this program.',
                '', sep = "\n"))
  
  source("ParaS.R")
  source("Modif.R")
  library(car)
  
  options(warn = 1)
  fit <- NULL
# --------------------------- #
# STEP: read data file (loop) #
# --------------------------- #
  # Judge the data format. "*.csv" is preferable
  # if no .csv, exit the function
  data <- as.character(data)
  endstr <- substr(data, nchar(data)-2, nchar(data))
  
  if (endstr != "csv"){
    stop(
      paste("Please prepare the raw data in .csv format,",
            "and don't forget to put the file name between double quotation mark, honey!",
            "Come on! Start over again!",
            sep = "\n"))
    exit()
  }else{
    df <- read.csv(data)
  }
  
# ------------------------------------------------------- #
# STEP: Choose the response variables for modeling (loop) #
# ------------------------------------------------------- #
  repeat{
    # options(warn = 1) # already defined in the upper environment
    resp <- as.character(readline("Please enter the name of response variable: "))
    check1 <- resp %in% names(df)
    if (check1 == TRUE){
      break
    }else{
      warning(paste("The variable '", resp, "' does not exist in the database!", sep=""))
    }    
  }
  
  ndf <- df   # Prepare a new df for further data modification
  model <- NULL  # Assign NULL to model first 
  
# ----------------------------------------------------- #
# STEP: Choose predictors for modeling (super big loop) #
# ----------------------------------------------------- #
  repeat{
    # Loop: continue adding new predictors to a model
    
    repeat{
      # Loop: if user is not satisfied with the modeling result,
      # remove the current one from the model and try another.
      
      repeat{
        pred <- as.character(readline("Please enter the name of predictor: "))
        check1 <- pred %in% names(df)
        if (check1 == TRUE){
          break
        }else{
          warning(paste("The predictor '", pred, "' does not exist in the database!", sep=""))
        }    
      }
      
      repeat{
        # Choose the transforming method (loop)
        cat(
          paste("Which kind of transformation does the variable '", pred, "' need to be adapted?", sep=""),
          "  1. carry over + power curve",
          "  2. carry over + s curve",
          "  3. auto-selection between 1 and 2",
          "  4. none", "", sep="\n")
        
        # assign transforming method to opt1
        opt1 <- readline("Please enter an option number: ")
        if(opt1 %in% 1:4){
          break
        }else{
          warning("Man (or u r a woman/girl...)! Only 4 options!")
        }
      }
      
      #ParaS(ini.pred, resp, ndf, model, opt1)
      offer <- (ParaS(ini.pred, resp, ndf, model, opt1))
      print(offer)
      
      # Allocate the best combination of parameters
      if (opt1 == 1){
        cat(
          "Do you agree to transform the variable with recommended parameters?",
          paste("carry over rate = ", co.r, sep=""),
          paste("power curve parameter = ", pc.r, sep=""),
          "  1. Yes",
          "  2. No", 
          " ",sep="\n")
        opt2 <- readline("Hurry up! 1 or 2? ")
        
        if(opt2 == 2){
          co.r <- readline("Please suggest alternative carry over rate: ")
          pc.r <- readline("Please suggest alternative power curve parameter: ")
          sc.1 <- NaN
          sc.2 <- NaN
        }else if(opt2 == 1){
          co.r <- offer$best.c.p.transform.parameters[1]
          pc.r <- offer$best.c.p.transform.parameters[2]
          sc.1 <- NaN
          sc.2 <- NaN
        }else{
          warning("Please enter the right number (1 or 2)!")
        }
        
      }else if(opt1 == 2){
        cat(
          "Do you agree to transform the variable with recommended parameters?",
          paste("carry over rate = ", co.r, sep=""),
          paste("power curve parameter = ", pc.r, sep=""),
          "  1. Yes",
          "  2. No", 
          " ", sep="\n")
        opt2 <- readline("Hurry up! 1 or 2? ")
        
        if(opt2 == 2){
          co.r <- readline("Please suggest alternative carry over rate: ")
          sc.1 <- readline("Please suggest alternative value for the 1st s curve parameter: ")
          sc.2 <- readline("Please suggest alternative value for the 2nd s curve parameter: ")
          pc.r <- NaN
        }else if(opt2 == 1){
          co.r <- offer$best.c.s.transform.parameters[1]
          pc.r <- NaN
          sc.1 <- offer$best.c.s.transform.parameters[2]
          sc.2 <- offer$best.c.s.transform.parameters[3]
        }else{
          warning("Please enter the right number (1 or 2)!")
        }
        
      }else if(opt1 == 3){
        cat("Which approach do you prefer?",
            "  1. carry over + power curve",
            "  2. carry over + s curve",
            " ", sep = "\n")
        appr <- readline("which one is preferred, 1 or 2? ")
        
        if (appr == 1){
          
          cat(
            "Do you agree to transform the variable with recommended parameters?",
            paste("carry over rate = ", co.r, sep=""),
            paste("power curve parameter = ", pc.r, sep=""),
            "  1. Yes",
            "  2. No", 
            " ",sep="\n")
          opt2 <- readline("Hurry up! 1 or 2? ")
          
          if(opt2 == 2){
            co.r <- readline("Please suggest alternative carry over rate: ")
            pc.r <- readline("Please suggest alternative power curve parameter: ")
            sc.1 <- NaN
            sc.2 <- NaN
          }else if(opt2 == 1){
            co.r <- offer$best.c.p.transform.parameters[1]
            pc.r <- offer$best.c.p.transform.parameters[2]
            sc.1 <- NaN
            sc.2 <- NaN
          }else{
            warning("Please enter the right number (1 or 2)!")
          }
          
        }else if (appr == 2){
          
          cat(
            "Do you agree to transform the variable with recommended parameters?",
            paste("carry over rate = ", co.r, sep=""),
            paste("power curve parameter = ", pc.r, sep=""),
            "  1. Yes",
            "  2. No", 
            " ", sep="\n")
          opt2 <- readline("Hurry up! 1 or 2? ")
          
          if(opt2 == 2){
            co.r <- readline("Please suggest alternative carry over rate: ")
            sc.1 <- readline("Please suggest alternative value for the 1st s curve parameter: ")
            sc.2 <- readline("Please suggest alternative value for the 2nd s curve parameter: ")
            pc.r <- NaN
          }else if(opt2 == 1){
            co.r <- offer$best.c.s.transform.parameters[1]
            pc.r <- NaN
            sc.1 <- offer$best.c.s.transform.parameters[2]
            sc.2 <- offer$best.c.s.transform.parameters[3]
          }else{
            warning("Please enter the right number (1 or 2)!")
          }
          
        }else{
          warning("Man (or you are a woman/girl...)! Only 2 options!")
        }
        
      }else{
        co.r <- NaN
        pc.r <- NaN
        sc.1 <- NaN
        sc.2 <- NaN
      }
      
      repeat{
        # ---------------------------------------------------------------- #
        # repeat when user wants to try other parameters on same predictor #
        # otherwise this loop is broken                                    #
        # ---------------------------------------------------------------- #
        
        # Call function Modif()
        # Modif(pred, data, co.r, pc.r, sc.1, sc.2)
        if (opt1 %in% 1:3){
          ndf <- Modif(pred, ndf, co.r, pc.r, sc.1, sc.2)
        }
        
        # Build the model
        if (is.null(fit)){
          fit <- lm(resp ~ pred, data = ndf, na.action = na.exclude)
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
          ok.trans <- readline("Make your choice, 1, 2 or 3? ")
          if(ok.trans %in% 1:3){
            warning("There is no options other than 1, 2 and 3!")
            break}
        }
        
        if (ok.trans == 2){
          ndf[[pred]] <- df[[pred]]
          
          repeat{
            cat("Which approach do you prefer?",
                "  1. carry over + power curve",
                "  2. carry over + s curve",
                " ", sep = "\n")
            appr <- readline("which one is preferred, 1 or 2? ")
            
            if (appr == 1){
              co.r <- readline("Please suggest alternative carry over rate: ")
              pc.r <- readline("Please suggest alternative power curve parameter: ")
              sc.1 <- NaN
              sc.2 <- NaN
              break
            }else if (appr ==2){
              co.r <- readline("Please suggest alternative carry over rate: ")
              sc.1 <- readline("Please suggest alternative value for the 1st s curve parameter: ")
              sc.2 <- readline("Please suggest alternative value for the 2nd s curve parameter: ")
              pc.r <- NaN
              break
            }else{
              warning("Please enter the right number (1 or 2)!")
            }
          }
          
        }else{break}
      }
      
      if (ok.trans == 1){
        break
      }else if (ok.trans == 3){
        fit <- update(fit, as.formula(sprintf('~. - %s', pred)))
        ndf[[pred]] <- df[[pred]]
      }
    }
    
    repeat{
      cat("Do you want to continue testing new predictors?",
          "  1. Yes, I do (but I'm not marrying you!)",
          "  2. Yes, but I want to remove a variable from the model first",
          "  3. No, Please show me the final model statistics (I had enough!)",
          " ", sep = "\n")
      
      opt3 <- readline("Your choice: 1, 2 or 3? ")
      if (opt3 %in% 1:3){
        break
      }else{
        warning("Please go back to your primary school and complete the basic math course!")
      }
    }    
    
    if (opt3 == 3){
      message("Per below you could check the statistics of final model:")
      print(summary(fit))
      # print other possible statistics (MAPE, contribution rate, dwtest, etc.)
      # print the parameters for each variables in the model
      break
    }else if (opt3 == 2){
      
      repeat{
        # Loop: remove predictors
        pred.rm <- as.character(readline("Please enter the name of predictor that you want to remove from the model: "))
        check1 <- pred.rm %in% names(coef(fit))
        if (check1 == TRUE){
          break
        }else{
          warning(paste("The predictor '", pred.rm, "' does not exist in the model!", sep=""))
        }
        
        fit <- update(fit, as.formula(sprintf('~. - %s', pred.rm)))
        ndf[[pred.rm]] <- df[[pred.rm]]
        message("Per below you could check the updated model after the predictor removed:")
        print(summary(fit))
        
        bin <- as.character(readline("Do you want to remove another predictor (Y/N)? "))
        if (bin == "N"){
          break
        }
      }
      break
    }
    
  }
  
  



  
  # Repeat the explanatory variable selection loop
  # remember to ask if it is necessary to keep the new var. in the model
  # intermedium model is called med.model
  
  # insert all dummy vars and test models
  
  # after all possibilities tried, build final model fin.fit and make a summary
  # do another model using stepAIC()
  # compare 2 models and get the best
  
}
