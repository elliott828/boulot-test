AutoReg <- function(data){
  
  ###############################
  ## refer to the process flow ##
  ###############################
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
  
  
  if (!"car" %in% rownames(installed.packages())) install.packages("car")
  if (!"zoo" %in% rownames(installed.packages())) install.packages("zoo")
  if (!"lmtest" %in% rownames(installed.packages())) install.packages("lmtest")
  library(car)     # 
  library(stats)   # for update() & as.formula()
  library(zoo)     # for library(lmtest) 
  library(lmtest)  # for durbin-watson test
  library(MASS)    # for stepAIC()
  
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
  # to add a new row to this empty data frame:
  # prmt[nrow(prmt)+1,] <- c(b,3,2,5,442)
  
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
      warning(paste('The variable "', resp, '" does not exist in the database!', sep=""))
    }    
  }
  
  ndf <- df   # Prepare a new df for further data modification
  # --------------------------------------- #
  # Asking for keeping the intercept or not #
  # --------------------------------------- #
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
  
  
  # ----------------------------------------------------- #
  # STEP: Choose predictors for modeling (super big loop) #
  # ----------------------------------------------------- #
  repeat{
    # Loop: continue adding new predictors to a model
    
    repeat{
      # Loop: if user is not satisfied with the modeling result,
      # remove the current predictor from the model and try another.
      co.r <- NaN
      pc.r <- NaN
      sc.1 <- NaN
      sc.2 <- NaN      
      
      repeat{
        pred <- as.character(readline("Please enter the name of predictor: "))
        check1 <- pred %in% names(ndf)
        if (check1 == TRUE){
          break
        }else{
          warning(paste('The predictor "', pred, '" does not exist in the database!', sep=""))
        }    
      }
      
      repeat{
        # Choose the transforming method (loop)
        cat(
          paste('Which kind of transformation does the variable "', pred, '" need to be adapted?', sep=""),
          "  1. carry over + power curve",
          "  2. carry over + s curve",
          "  3. auto-selection between 1 and 2",
          "  4. none", "", sep="\n")
        
        # assign transforming method to opt1
        opt1 <- readline("Please enter an option number: ")
        if(!opt1 %in% as.character(1:4)){
          warning("Man (or u r a woman/girl...)! Only 4 options!")
        }else{
          opt1 <- as.numeric(opt1)
          break
        }
      }
      
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
        
        repeat{
          cat("",
            "Program recommends those parameter values:",
            paste("carry over rate = ", co.r, sep=""),
            paste("1st s curve parameter = ", sc.1, sep=""),
            paste("2nd s curve parameter = ", sc.2, sep=""),
            " ",
            "Do you agree to transform the variable with recommended parameters?",
            "  1. Yes",
            "  2. No", 
            " ", sep="\n")
          opt2 <- readline("Hurry up! 1 or 2? ")
          
          if (!opt2 %in% as.character(1:2)){
            message("Please enter a valid number!")
          }else{
            opt2 <- as.numeric(opt2)
            break
          }
        }
        
        
        
        if(opt2 == 2){
          co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
          sc.1 <- as.numeric(readline("Please suggest alternative value for the 1st s curve parameter: "))
          sc.2 <- as.numeric(readline("Please suggest alternative value for the 2nd s curve parameter: "))
          
        }else if(!opt2 %in% 1:2){
          warning("Please enter the right number (1 or 2)!")
        }
        
      }else if(opt1 == 3){
        
        repeat{
          cat("Which approach do you prefer?",
              "  1. carry over + power curve",
              "  2. carry over + s curve",
              " ", sep = "\n")
          appr <- readline("which one is preferred, 1 or 2? ")
          
          if (!appr %in% c("1","2")){
            message("Please enter a valid number!")
          }else{
            appr <- as.numeric(appr)
            break
          }
        }
        
        
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
          
          repeat{
            cat("",
                "Program recommends those parameter values:",
                paste("carry over rate = ", co.r, sep = ""),
                paste("1st s curve parameter = ", sc.1, sep = ""),
                paste("2nd s curve parameter = ", sc.2, sep = ""),
                " ",
                "Do you agree to transform the variable with recommended parameters?",
                "  1. Yes",
                "  2. No", 
                " ", sep="\n")
            opt2 <- readline("Hurry up! 1 or 2? ")
            
            if (!opt2 %in% as.character(1:2)){
              message("Please enter a valid number!")
            }else{
              opt2 <- as.numeric(opt2)
              break
            }
          }
          
          
          if(opt2 == 2){
            co.r <- as.numeric(readline("Please suggest alternative carry over rate: "))
            sc.1 <- as.numeric(readline("Please suggest alternative value for the 1st s curve parameter: "))
            sc.2 <- as.numeric(readline("Please suggest alternative value for the 2nd s curve parameter: "))
            
          }else if(!opt2 %in% 1:2){
            warning("Please enter the right number (1 or 2)!")
          }
          
        }else{
          warning("Man (or you are a woman/girl...)! Only 2 options!")
        }
        
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
          if (incpt == "Y"){
            fit <- lm(as.formula(sprintf('%s ~ %s', resp, pred)), data = ndf, na.action = na.exclude)
          }else{
            fit <- lm(as.formula(sprintf('%s ~ %s + 0', resp, pred)), data = ndf, na.action = na.exclude)
          }
          
        }else{
          fit <- update(fit, as.formula(sprintf('~. + %s', pred)), data = ndf)
        }
        message("Per below you could find the summary of updated model: ")
        print(mdl.smry(fit, ndf, resp))        
        
        repeat{
          cat("Are you OK with the chosen variable and its transformation?",
              "  1. Yes",
              "  2. No, I want to try other parameters",
              "  3. No, I want to try another variable",
              " ", sep = "\n")
          ok.trans <- readline("Make your choice, 1, 2 or 3? ")
          if(!ok.trans %in% as.character(1:3)){
            warning("There is no option other than 1, 2 and 3!")
          }else{
            ok.trans <- as.numeric(ok.trans)
            break
          }
        }
        
        if (ok.trans == 2){
          ndf[[pred]] <- df[[pred]]
          
          repeat{
            repeat{
              cat("Which approach do you prefer?",
                  "  1. carry over + power curve",
                  "  2. carry over + s curve",
                  " ", sep = "\n")
              appr <- readline("which one is preferred, 1 or 2? ")
              
              if (!appr %in% as.character(1:2)){
                message("Please enter a valid number!")
              }else{
                appr <- as.numeric(appr)
                break
              }
            }
            
            
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
          
        }else{break}
      }
      
      if (ok.trans == 1){
        prmt[nrow(prmt)+1, ] <- c(pred, co.r, pc.r, sc.1, sc.2, "alive")
        # only when ok.trans = 1, the parameters can be added into "prmt"
        break
      }else if (ok.trans == 3){
        fit <- update(fit, as.formula(sprintf('~. - %s', pred)))
        ndf[[pred]] <- df[[pred]]
      }
    }
    
    repeat{ 
      # Loop: final confirm of a loop
      repeat{
        cat("Do you want to continue testing new predictors?",
            "  1. Yes, I do (but I'm not marrying you!)",
            "  2. Yes, but I want to remove a predictor/the intercept from the model first",
            "  3. No, Please show me the final model statistics (I had enough!)",
            "  4. I want to add the intercept to current model",
            " ", sep = "\n")
        
        opt3 <- readline("Your choice: 1, 2, 3 or 4? ")
        if (!opt3 %in% as.character(1:4)){
          warning("Please go back to your primary school and complete the basic math course!")
        }else{
          opt3 <- as.numeric(opt3)
          break
        }
      }
      
      if (opt3 == 3){
        message("Per below you could check the statistics of final model:")
        print(mdl.smry(fit, ndf, resp))
        
        repeat{
          prmt.exp <- readline("Do you want to export the parameters history (Y/N)? ")
          
          if (prmt.exp == "Y"){
            write.csv(prmt, paste(getwd(), "/prmt.csv", sep = ""))
            message('Variable parameters history is exported to "prmt.csv" under default working directory')
            
            resid <- summary(fit)$residuals
            subset.resp <- as.numeric(names(resid))
            act.resp.mod <- act.resp <- ndf[[resp]][subset.resp]
            act.resp.mod[which(act.resp.mod == 0)] <- mean(act.resp.mod)
            ape <- abs(resid)/act.resp.mod
            mape.elmt <- cbind(resid, act.resp, ape)
            colnames(mape.elmt) <- c("Residual", "Actual", "APE")
            write.csv(mape.elmt, paste(getwd(), "/mape.element.csv", sep = ""))
            message('The elements of MAPE "mape.element.csv" is exported.')
            
            break
          }else if (prmt.exp == "N"){
            break
          }else{
            warning("Please do enter Y or N")
          }
        }
        
        # TO BE IMPLEMENTED: print other possible statistics (MAPE, contribution rate, dwtest, etc.)
        
        break # break upper loop
        
      }else if (opt3 == 2){
        
        repeat{
          # Loop: remove predictors / intercept
          message('If you want to remove intercept, please enter "Intercept",')
          message('otherwise, please enter the name of predictor.')
          pred.rm <- as.character(readline(paste("Which one to be removed from the model? ", sep = "\n")))
          
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
            print(mdl.smry(fit, ndf, resp))
            
            bin <- as.character(readline("Still want to remove predictor / intercept (Y/N)? "))
            
          }else{
            
            fit <- update(fit, ~. -1)
            message("Per below you could check the updated model after the predictor removed:")
            print(mdl.smry(fit, ndf, resp))
            
            bin <- as.character(readline("Still want to remove predictor / intercept (Y/N)? "))
            
          }
          
          if (toupper(bin) == "N"){
            message("Let's continue modeling with other predictors(OMG)...")
            break # break upper loop: remove predictors / intercept
          }
        }
        
      }else if (opt3 == 4){
        if ("(Intercept)" %in% rownames(coef(fit))){
          warning("Intercept already exists in the model! No need to add it!")
          break # break upper loop: remove predictors / intercept
        }else{
          fit <- update(fit, ~. + 1)
          message("Per below you could check the updated model after the intercept added:")
          print(mdl.smry(fit, ndf, resp))
        }
        
      }else{
        break #break upper loop: final confirm of a loop
      }
    }
    if(opt3 == 3){
      break # quit the function
    }
    
  }
  
  
  
  # things to be realized in next version:
  # - insert all dummy vars and test models
  
}
