########################################################################################
## A collection of 6 self-defined functions                                           ##
## 1. co() - transform variable with carry-over rate                                  ##
## 2. pc() - transform variable with power curve rate                                 ##
## 3. sc() - transform variable with s curve rates                                    ##
## 4. meth.c.p() - find out best parameters for transforming with co()+pc()           ##
## 5. meth.c.s() - find out best parameters for transforming with co()+sc()           ##
## 6. cp.vs.cs() - compare the 2 methodology (4 & 5) and offer the best               ##
## 7. lm.lag() - produce lagged data in both backward & forward direction             ##
## 8. mdl.smry() - combination of basic model summary, MAPE & dwtest                  ##
## 9. ContM() - read variables and their transformation variables and rebuild a model ##
########################################################################################

##################################################
# define carry-over rate transformation function #
##################################################
co <- function(variable, i){
  var1 <- variable
  for (p in (2:length(var1))){
    if (is.na(var1[p-1])){
      var1[p] = var1[p]
    }else{
      var1[p] = var1[p-1] * i + var1[p]
    }
  }
  return(var1)
}

###############################################
# define power curver transformation function #
###############################################
pc <- function(variable, j){
  var2 <- variable
  var2 <- sapply(var2, function(x)return(x^j))
  return(var2)
}

##########################################
# define s-curve transformation function #
##########################################
sc <- function(variable, k, l){
  var2 <- variable
  for (q in (1:length(var2))){
    var2[q] = 1 - exp(-k * var2[q]^l)
  }
  return(var2)
}

########################################
# transform following method = co + pc #
########################################
meth.c.p <- function(pred, resp, data, model = NULL){
  # make sure the raw data is already loaded into global environment
  
  df <- data
  arsq <- NaN  # adjusted r-square
  coef <- matrix(c(NaN,NaN), 1, 2)   # coefficient of the transformed predictor and its p-value of t-test
  index <- matrix(c(NaN,NaN), 1, 2)  # combination of indexes i & j
  
  for (i in seq(0.05, 0.95, 0.05)){
    # carry-over loop starting empirically from .4 to .95 by step .05
    for (j in seq(0.4, 1, 0.05)){
      # power-curve loop starting from .1 to 1 by step .05
      df[[pred]] <- pc(co(df[[pred]], i), j)
      #pred1 <- pc(co(data[[pred]], i), j)
      
      # check if the parameter selection is based on a existing model
      if (is.null(model)){
        mdl <- lm(as.formula(sprintf('%s ~ %s', resp, pred)), data = df)

      }else{
        mdl <- update(model, as.formula(sprintf('~. + %s', pred)), data = df)
      }
      pos <- which(names(coef(mdl)) == pred)

      coef.test <- try(coef(summary(mdl))[pos,1],silent = TRUE)
      if(inherits(coef.test, "try-error")) {
        # next
        cf <- NaN
        pv <- NaN
      }else{
        cf <- coef(summary(mdl))[pos,1]
        pv <- coef(summary(mdl))[pos,4]
      }
      arsq <- append(arsq, summary(mdl)$adj.r.square, length(arsq))
      coef <- rbind(coef, c(cf, #coef(summary(mdl))[pos,1]
                            pv))#coef(summary(mdl))[pos,4]
      index <- rbind(index, c(i, j))
      
      ######################################################
      ##                   SUPER IMPORTANT                ## 
      ## Set back the original value at the end of a loop ##
      ######################################################
      df[[pred]] <- data[[pred]]
    }
  }

  
  arsq <- arsq[2:length(arsq)] # get rid of c(NaN, NaN) - the iniital values
  index <- index[2:nrow(index),]  
  coef <- coef[2:nrow(coef),]
  summary <- as.data.frame(cbind(index, coef, arsq))
  names(summary) <- c(paste("parameter",seq(1:ncol(index)), sep = ""),
                      "estimate","p-value", "adjusted.r.square")
  write.csv(summary, paste("meth.c.p.",pred,".parameters.csv",sep=""))
  message(paste("'meth.c.p.",pred,".parameters.csv' is generated.",sep=""))
  
  bestarsq <- round(match(max(arsq), arsq), digits = 0) # index of the highest value of adjusted r-square
  
  best <- list(max(arsq), index[bestarsq,], coef[bestarsq,])
  
  names(best) <- c("cp.maximum.arsq", "best.c.p.transform.parameters", 
                   "coef.and.p.value")
  
  return(best)
}

########################################
# transform following method = co + sc #
########################################
meth.c.s <- function(pred, resp, data, model = NULL){
  # make sure the raw data is already loaded into global environment
  
  df <- data
  arsq <- NaN  # adjusted r-square
  coef <- matrix(c(NaN,NaN), 1, 2)   # coefficient of the transformed predictor and its p-value of t-test
  index <- matrix(c(NaN,NaN,NaN), 1, 3)  # combination of indexes i, k & l
  
  for (i in seq(0.05, 0.95, 0.05)){
    for (k in seq(0.0001, 0.0009, 0.0001)){
      for (l in seq(1.1, 1.9, 0.1)){
        # s-curve loop concerning 2 parameters
        df[[pred]] <- sc(co(df[[pred]], i), k, l)
        
        if (is.null(model)){
          mdl <- lm(as.formula(sprintf('%s ~ %s', resp, pred)), data = df)
        }else{
          mdl <- update(model, as.formula(sprintf('~. + %s', pred)), data = df)
        }
        pos <- which(names(coef(mdl)) == pred)
        
        # test if the coefficient is available after transformation
        coef.test <- try(coef(summary(mdl))[pos,1],silent = TRUE)
        if(inherits(coef.test, "try-error")) {
          # next
          cf <- NaN
          pv <- NaN
        }else{
          cf <- coef(summary(mdl))[pos,1]
          pv <- coef(summary(mdl))[pos,4]
        }
        arsq <- append(arsq, summary(mdl)$adj.r.square, length(arsq))
        coef <- rbind(coef, c(cf, #coef(summary(mdl))[pos,1]
                              pv))  #coef(summary(mdl))[pos,4]
        index <- rbind(index, c(i, k, l))

        ######################################################
        ##                   SUPER IMPORTANT                ## 
        ## Set back the original value at the end of a loop ##
        ######################################################
        df[[pred]] <- data[[pred]]
        
      }
    }
  }
  
  arsq <- arsq[2:length(arsq)] # get rid of c(NaN, NaN) - the iniital values
  index <- index[2:nrow(index),]  
  coef <- coef[2:nrow(coef),]
  summary <- as.data.frame(cbind(index, coef, arsq))
  names(summary) <- c(paste("parameter",seq(1:ncol(index)), sep = ""),
                      "estimate","p-value", "adjusted.r.square")
  write.csv(summary, paste("meth.c.s.",pred,".parameters.csv",sep=""))
  message(paste("'meth.c.s.",pred,".parameters.csv' is generated.",sep=""))
  
  bestarsq <- round(match(max(arsq), arsq), digits = 0) # index of the highest value of adjusted r-square
  
  best <- list(max(arsq), index[bestarsq,], coef[bestarsq,])
  
  names(best) <- c("cs.maximum.arsq", "best.c.s.transform.parameters", 
                   "coef.and.p.value")
  
  return(best)
}

###########################################################
# compare the results between co() + pc() and co() + sc() #
###########################################################
cp.vs.cs <- function(pred, resp, data, model = NULL){
  # make sure the raw data is already loaded into global environment
  call("meth.c.p")
  call("meth.c.s")
  cp <- meth.c.p(pred, resp, data, model)
  cs <- meth.c.s(pred, resp, data, model)
  best <- as.list(c(cp, cs))
  return(best)
}

####################################
# making lag data for linear model #
####################################
lm.lag <- function(var, data, i){
  df <- data
  for (iter in i){
    if (i < 0){
      df$new <- c(data[[var]][(abs(iter)+1):nrow(data)], rep(NaN, abs(iter)))
      
    }else{
      df$new <- c(rep(NaN, iter), data[[var]][1:(nrow(data)-iter)])
    }
    colnames(df) <- c(names(df)[1:(ncol(df)-1)],
                      paste(var, "lag", iter, sep=".")) 
  }
  return(df)
}


################################
# necessary summary of a model #
################################
mdl.smry <- function(model, data, var){
  if (!"lmtest" %in% rownames(installed.packages())) install.packages("lmtest")
  library(lmtest)
  
  pre.summary <- summary(model)
  
  # MAPE (residual <- either residual from summary() or residual from proj())
  proj.mdl <- proj(model)
  abs.res <- abs(proj.mdl[,ncol(proj.mdl)])
  a <- as.numeric(rownames(proj.mdl))
  actual <- data[[var]][a]
  actual[which(actual == 0)] <- mean(actual)
  mape <- mean(abs.res/actual)
  
  # Durbin-Watson Test
  dw <- dwtest(model)
  
  # Contribution Rate
  # To be implemented
  
  consolidation <- list(pre.summary, mape, dw)
  names(consolidation) <- c("SUMMARY", "MAPE", "DWTEST")
  return(consolidation)
  
}


#=============================================#
# Continue modeling based on a built up model #
#=============================================#
ContM <- function(resp, data){
  
  if(file.exists("Modif.R")){
    source("Modif.R")
  }else{
    source("https://raw.githubusercontent.com/elliott828/boulot-test/master/Modif.R")
  }
  
  fit <- NULL
  df <- data
  
  # read the parameter file which realizes the transformation of variables
  
  # the file should have 6 basic columns
  # - variable: variable names
  # - type: transformation type (not developped yet)
  # - co.rate: carry-over rate
  # - pc.rate: power curve rate
  # - sc.rate1: s curve rate 1
  # - sc.rate2: s curve rate 2
  # - status: is the variable still alive in the model
  
  repeat{
    cat("\n")
    message("Be aware that the file should be of format '.csv'!")
    cat("\n")
    csv <- readline("Please enter the name of transformation parameter file: ")
    endstr <- substr(csv, nchar(csv)-2, nchar(csv))
    
    if (endstr != "csv"){
      cat("\n")
      message("A '.csv' file is expected!")
      cat("\n")
    }else if(file.exists(csv)){
      prmt <- read.csv(csv)
      break
    }else{
      cat("\n")
      message(paste("The file '", csv, "' does not exist!"))
      cat("\n")
    }
  }
  
  # read the variable name and variable status
  loop.len <- nrow(prmt)
  for (i in 1:loop.len){
    if(!prmt[i,2] %in% names(data)){
      
      # if the variable cannot match then stop the function
      stop(paste("The variable ",prmt[i,2]," does not exist in this dataset!"))
      cat("\n")
    }else{
      
      # else check the status of variable
      if (prmt[i,7] == "dead"){
        
        # if status == "dead" go to next loop
        next
      }else{
        
        # else call Modif() to transform the variable
        pred <- as.character(prmt[i,2]) # predictor
        co.r <- prmt[i,3] # co.rate
        pc.r <- prmt[i,4] # pc.rate
        sc.1 <- prmt[i,5] # sc.rate1
        sc.2 <- prmt[i,6] # sc.rate2
        
        df <- Modif(pred, df, co.r, pc.r, sc.1, sc.2)
                
        # if i = 1, build a new model; if i > 1, update previous model
        if (i == 1){
          fit <- lm(as.formula(sprintf('%s ~ %s', resp, pred)),
                    data = df, na.action = na.exclude)
        }else{
          fit <- update(fit, as.formula(sprintf('~. + %s', pred)), 
                        data = df)
        }
      }
    }
  }
  
  # return a list of both model result and updated data frame
  return(list(fit, df, prmt))
  
}

# 8/7/2014: Creation - mdl.smry(), listing basic summary, MAPE and dwtest
# 8/29/2014: Creation - ContM(), continue modeling based on the list of variables and their transformation parameters
# 9/1/2014: Update - ContM(), bug fixed on reading predictor to be transformed - all data will be coerced to char concerning variable names
# 9/1/2014: Update - ContM(), full dataset of prmt is a part of new list returned at the end of this program
