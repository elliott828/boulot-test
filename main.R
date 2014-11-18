#---------------------------------------------------------------------------------------#
# modif: modify a dataset based on pointed variable, transformation type and parameters #
# trial: update a model  based on pointed variables, transformation type and parameters #
# recom: test transformation and provide recommendation for trans method and parameters #
# rebuild: rebuild a model based on parameter testing history from last modeling result #
#---------------------------------------------------------------------------------------#

modif <- function(pred, type, data, co.r=NULL, sc.1=NULL, sc.2=NULL, pc.r=NULL, object=NULL){
  #---------------------------------
  # transform specified variable with(out) parameters
  # this function returns **a dataset** with selected variable transformed
  # make sure the raw data is already loaded into global environment
  #---------------------------------
  # pred: the name of variable to be transormed of class "character"
  # type: transformation method:
  #       1.1~1.2 media: carryover + s-curve / carryover + power curve
  #       2.1.1~2.1.4 basic: +,-,*,/; corresponding to parameter "object"
  #       2.2 basic: logarithm
  #       2.3 basic: root
  #       2.4 basic: exponent
  #       2.5 basic: reciprocal
  #       2.6 basic: time lag
  #       the input for type should be strictly controlled to the list of option number above!!!
  # data: data frame to store the variable transformed (original var. will be covered)
  #---------------------------------
  
  trans.collection <- c("co","sc","pc","bt")
  if(!sum(sapply(trans.collection, existsFunction))==4){
    # check if the functions co(), sc(), pc() & bt() exist or not, if not, source "odds.R"
    
    if(file.exists("odds.R")){
      source("odds.R")
    }else{
      source("https://raw.githubusercontent.com/elliott828/boulot-test/master/odds.R")
    }
  }
  
  df <- data
  x <- data[[pred]]
  
  if(type == 1.1){
    df[[pred]] <- cs(data[[pred]], co.r, sc.1, sc.2)
  }else if(type == 1.2){
    df[[pred]] <- cp(data[[pred]], co.r, pc.r)
  }else{
    opt <- substr(as.character(type),3,nchar(as.character(type)))
    # turn 2.1.1~2.1.4 and 2.2~2.6 to 1.1~1.4 and 2~6
    df[[pred]] <- bt(data[[pred]], opt, object)
  }
  
  return(df)
}

#----------------------------------------------------------------------------------

trial <- function(data, resp, fit = NULL, action = 2, pred = NULL) {      			
  # Build the model				
  # based on NULL or fit1, add/remove a predictor/the intercept, output summary				
  
  # action: -1 means delete the Intercept
  #          1 means add the Intercept
  #          2 means add a predictor
  #         -2 means delete a predictor
  
  if(action == -1 | action == 1) pred <- "(Intercept)"
  
  if(is.null(fit)){ # if(exists("fit", mode = "list"))
    if(action == 1) {
      lm(as.formula(sprintf('%s ~ 1', resp)), data = data, na.action = na.exclude)
    } else if(action == 2) {
      lm(as.formula(sprintf('%s ~ %s + 0', resp, pred)), data = data, na.action = na.exclude)
    } else if(action == -1) {
      lm(as.formula(sprintf('%s ~ -1', resp)), data = data, na.action = na.exclude)
    } else { # if(action == -2)
      warning("There's no existed model to let you delete ", pred, " from!")
    }
    
  } else { #if(!is.null(fit))
    if(pred %in% names(coef(fit))) {
      if(action == -1) {
        update(fit, ~. -1)
      } else if(action == -2) {
        update(fit, as.formula(sprintf('~. - %s', pred)))
      } else {
        warning(if(action == 1)"Intercept"else pred, " is already in the model!")
      }
    } else {
      if(action == 1){
        update(fit, ~. +1)
      } else if(action == 2){
        update(fit, as.formula(sprintf('~. + %s', pred)))
      } else {
        warning(if(action == -1)"Intercept"else pred, " isn't in the model!")
      }
    }    
  }    
}

#----------------------------------------------------------------------------------

recom <- function(pred, resp, data, type, fit = NULL, object = NULL){
  #---------------------------------
  # pred: predictor to be inserted to the model, to be quoted. i.e.: "cly" (mtcars)
  # resp: response variable in the model, to be quoted. i.e.: "mpg" (mtcars)
  # data: data base for the model
  # type: transformation method for the predictor
  # fit: default value is NULL; if a model exists, then put the name of fit list
  #---------------------------------
  # When type = 1.1: test all combinations of parameters of carryover & s-curve
  # When type = 1.2: test all combinations of parameters of carryover & power curve
  # When type = 1.3: do both of the actions above
  # When type >= 2: call bt()
  # Check the best R-square / p-value of coef, give recommendations
  #---------------------------------
  
  #----------------------#
  # PART I - Preparation #
  #----------------------#
  
  # Check if the function trial() & the package exists
  if(!existsFunction("bt") == T){
    if(file.exists("odds.R")){
      source("odds.R")
    }else{
      source("https://raw.githubusercontent.com/elliott828/boulot-test/master/odds.R")
    }
  }
  
  if(!"plyr" %in% installed.packages()){
    install.packages("plyr")
    require(plyr)
  }
  
  #---------------------------------
  
  # Load the data
  df <- data
  x <- df[[pred]]
  y <- df[[resp]]
  
  # The default parameter ranges are as below:
  # (To be modified on demand)
  co.range <- seq(0.05, 0.95, 0.05)  # 
  lamda1.range <- seq(0.0001, 0.0009, 0.0001)
  lamda2.range <- seq(1.1, 1.9, 0.1)
  pc.range <- seq(0.4, 0.95, 0.05)
  
  #---------------------------------#
  # PART II - Fundamental functions #
  #---------------------------------#
  
  # Function 1.1
  #---------------------------------
  # Set sub functions (cs.trans & cp.trans) for generating predictor matrix 
  # (transformed by different group of parameters)
  cstrans <- function(x){
    cs.mat <- t(mdply(expand.grid(co.rate = co.range, 
                                  lamda1 = lamda1.range, 
                                  lamda2 = lamda2.range),
                      cs, x))
    return(cs.mat)
  }
  
  # Function 1.2
  #---------------------------------
  cptrans <- function(x){
    cp.mat <- t(mdply(expand.grid(co.rate = co.range, 
                                  exponent = pc.range),
                      cp, x))
    return(cp.mat)
  }
  
  # Function 2
  #---------------------------------
  # Check if a fit exists, update the fit if it does, otherwise create one
  fit.update <- function(resp, x, fit.check = NULL, pred, data){
    df <- data
    df[[pred]] <- x
    fit.new <-  if (is.null(fit.check)){
      lm(as.formula(sprintf("%s ~ %s", resp, pred)), data = df)
    }else{
      update(fit.check, as.formula(sprintf("~.+ %s", pred)), data = df)
    }
    
    return(fit.new)
  }
  
  # Function 3
  #---------------------------------
  # Extract the key modeling summary statatistics
  # Call Function 2: fit.update()
  summary.stats <- function(resp, x, fit.coef = fit, pred, data){
    smr <- summary(fit.update(resp, x, fit.coef, pred, data))
    smr.coef <- subset(smr$coefficients, rownames(smr$coefficients)==pred)[c(1,4)]
    smr.key <- c(smr.coef, smr$r.squared, smr$adj.r.squared)
    names(smr.key) <- c("Coefficient", "P.value", "R.squared", "Adj.R.squared")
    # to generate a group of key statistics, how many variables corresponding to how many groups
    return(t(smr.key))
  }
  
  # Function 4
  #---------------------------------
  # Test all the combinations of parameters & the respective modeling result
  testall <- function(resp, x, pred, fit.coef = fit, type, data){
    opt <- LETTERS[as.numeric(substr(as.character(type),3,3))]
    #both <- function(x)list(cstrans(x),cptrans(x))
    
    mat <- switch(opt,
                  A = cstrans(x),
                  B = cptrans(x))
    
    len <- switch(opt,
                  A = 3,
                  B = 2)
    
    nam <- switch(opt,
                  A = "cs",
                  B = "cp")
    
    met <- switch(opt,
                  A = "Carryover + S-curve",
                  B = "Carryover + Power curve")
    
    prmt <- mat[1:len,]    # capture the parameter combinations
    var <- as.data.frame(mat[(len+1):nrow(mat),])    # capture the transfored variables
    #colnames(var) <- paste(pred, 1:ncol(var), sep = "")
    
    test.stats <- t(sapply(var, summary.stats, resp = resp, pred = pred, fit.coef = fit.coef, data = data))
    # coef <- test.stats[1:2]
    # rsq <- test.stats[3]
    # adj.rsq <- test.stats[4]
    
    prmt.all <- as.data.frame(cbind(t(prmt), test.stats))
    curve.prmt <- if(len == 2){"exponent"}else{c("lamda1","lamda2")}
    colnames(prmt.all) <- c("carryover.rate",curve.prmt,"coef","p-value","r.squared","adjusted.r.squared")
    rownames(prmt.all) <- NULL
    
    # export all parameters and related model statistics to local working directory
    write.csv(prmt.all, paste("prmt", nam, pred, "csv",sep = "."))
    cat("\n")
    message(paste("The parameter reference: 'prmt", nam, pred, "csv' is exported!",sep = "."))
    
    # capture the best group of parameters
    # get the best adj. r squared first then allocate the position
    best <- max(prmt.all[,ncol(prmt.all)])
    posi <- which(round(prmt.all$adjusted.r.squared,6)==round(best,6))
    if (length(posi)>1)posi <- sample(posi,1)
    best.stats <- prmt.all[posi,]
    rownames(best.stats) <- NULL   # remove the row index assigned automatically by the program
    
    # print(paste("the size of prmt.all is ", paste(dim(prmt.all),collapse=" and "),sep=""))
    # print(paste("the value of best is ", best,sep=""))
    # print(paste("the best row number is ", which(round(prmt.all$adjusted.r.squared,6)==round(best,6)), sep =""))
    # print(paste("the size of best.stats is ", paste(dim(best.stats),collapse=" and "),sep=""))
    
    # print the message indicating the best transformation parameters and results
    cat("",
        paste(" For the transformation method ", met, ":", sep=""),
        paste(" ", paste(rep("-",40), collapse = ""),sep = ""),
        paste(" - The recommended carryover rate is ", best.stats[1], sep=""),
        sep = "\n")
    
    if(as.character(type)==1.1){
      cat(paste(" - The recommended lamda1(S-curve) is ", best.stats[2], sep = ""),
          paste(" - The recommended lamda2(S-curve) is ", best.stats[3], sep = ""),
          sep = "\n")
      curve.prmt <- c(best.stats[1], best.stats[2], best.stats[3], NA)
    }else if(as.character(type)==1.2){
      cat(paste(" - The recommended power rate is ", best.stats[2], sep = ""),
          sep = "\n")
      curve.prmt <- c(best.stats[1], NA, NA, best.stats[2])
    }
    
    cat(paste(" ", paste(rep("-",40), collapse = ""),sep = ""),"\n")
    
    cat(paste(" - The coefficient of ", pred, " in this model is ", round(best.stats[ncol(prmt.all)-3],4)),
        paste(" - The p-value of the coefficient is ", as.numeric(format(best.stats[ncol(prmt.all)-2],scientific=T))),
        paste(" - The r-squared of the model is ", round(best.stats[ncol(prmt.all)-1],4)),
        paste(" - The adjusted r-squared of the model is ", round(best.stats[ncol(prmt.all)],4)),
        paste(" ", paste(rep("-",40), collapse = ""),sep = ""), sep = "\n")
    
    if (best.stats[ncol(prmt.all)-2] > 0.2){
      message("Please be aware that the p-value of predictor coefficient is larger than 0.2!")
      message("The estimate of coefficient is not significant!")
      cat("")
    }
    
    
    # return(list(prmt.all, best.stats)) return the variable transformed by all combination of parameters
    op.recom <- c(type, best.stats[ncol(prmt.all)-3], best.stats[ncol(prmt.all)-2], 
                  best.stats[ncol(prmt.all)-1], best.stats[ncol(prmt.all)], curve.prmt)
    names(op.recom) <- c("Trans.Type","Coef","P.Value","R.Sq","Adj.R.Sq","Carryover.Rate",
                         "Lamda1","Lamda2","Power.Rate")
    return(op.recom)
    
    # write.csv(var, paste("transformation",pred,"csv",sep="."))
  }
  
  # Function 5
  #---------------------------------
  # Test other transformation methods (or no transformation)
  # Here the transformation type could be:
  # - 2.1.1~2.1.4 & 2.2~2.6: basic transformation
  # - 0: no transformation
  testoth <- function(resp, x, pred, fit.coef = fit, type, data, object = object){
    
    if (as.character(type) == "0"){
      x.new <- x
    }else{
      type.new <- as.numeric(substr(as.character(type),3,nchar(as.character(type))))
      x.new <- bt(x, type.new, object)
    }
    
    fit.new <- fit.update(resp = resp, x = x.new, fit.check = fit.coef, pred = pred, data = data)
    full.opt <- c("0","2.1.1","2.1.2","2.1.3","2.1.4","2.2","2.3","2.4","2.5","2.6")
    opt <- letters[which(full.opt == as.character(type))]
    
    object.bis <- if(length(object)>1){"another variable"}else{object}
    
    trans.opt <- switch(opt,
                        a = "No transformation",
                        b = paste(pred, " plus ", object.bis, sep = ""),
                        c = paste(pred, " minus ", object.bis, sep = ""),
                        d = paste(pred, " times ", object.bis, sep = ""),
                        e = paste(pred, " divided by ", object.bis, sep = ""),
                        f = paste("Logarithm on ", pred, sep = ""),
                        g = paste(pred, "to the square of ", object.bis, sep = ""),
                        h = paste(pred, "to the power of ", object.bis, sep = ""),
                        i = paste("Reciprocal of ", pred, sep =""),
                        j = paste("Time lag for ", pred, " by ", object.bis, " time unit(s)", sep = ""))
    
    coef.new <- c(summary(fit.new)$coefficients[length(coef(fit.new)),1],summary(fit.new)$coefficients[length(coef(fit.new)),4],
                  summary(fit.new)$r.squared, summary(fit.new)$adj.r.squared)
    names(coef.new) <- c("coef", "p-value", "r.squared", "adjusted.r.squared")
    cat("",
        paste(" You choose to do: ", trans.opt, sep = ""),
        paste(" ", paste(rep("-",40),collapse = ""), sep = ""),
        paste(" The coefficient of ", pred, " in this model is ", round(coef.new[1],4), sep = ""),
        paste(" The p-value of the coefficient is ", as.numeric(format(coef.new[2],scientific=T)), sep = ""),
        paste(" The r-squared of the model is ", round(coef.new[3],4), sep = ""),
        paste(" The adjusted r-squared of the model is ", round(coef.new[4],4), sep = ""),
        "", sep = "\n")
    
    # return(list(coef.new, c(object, type)))
    op.recom <- c(type, coef.new[1], coef.new[2], coef.new[3], coef.new[4],
                  rep(NA, 4))
    names(op.recom) <- c("Trans.Type","Coef","P.Value","R.Sq","Adj.R.Sq","Carryover.Rate",
                         "Lamda1","Lamda2","Power.Rate")
    return(op.recom)
  }
  
  #----------------------------------#
  # PART III - Fundamental functions #
  #----------------------------------#
  
  if(as.character(type) == "1.1"){
    # carryover + s-curve only
    prmt.rec <- testall(resp, x, pred, fit.coef = fit, type = 1.1, data = df)
  }else if(as.character(type) == "1.2"){
    # carryover + power curve only
    prmt.rec <- testall(resp, x, pred, fit.coef = fit, type = 1.2, data = df)
  }else if(as.character(type) == "1.3"){
    # compary carryover + s-cu4ve and carryover + power curve 
    prmt.cs <- testall(resp, x, pred, fit.coef = fit, type = 1.1, data = df)
    prmt.cp <- testall(resp, x, pred, fit.coef = fit, type = 1.2, data = df)
    if(as.numeric(prmt.cs[5]) > as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cs
      cat("\n")
      message("Concerning r-squared, the method **CARRYOVER + S-CURVE** is preferred.")
      cat("\n")
    }else if(as.numeric(prmt.cs[5]) < as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cp
      cat("\n")
      message("Concerning r-squared, the method **CARRYOVER + POWER CURVE** is preferred.")
      cat("\n")
    }else{
      prmt.rec <- prmt.cp
      message("Both transformation methods are OK for the model.")
      cat("\n")
    }
  }else{
    # modeling result of other transformation
    # caliberate the option number for type > 2
    prmt.rec <- testoth(resp = resp, x, pred = pred, fit.coef = fit, type = type, data = df, object = object)
    
  }
  return(prmt.rec)
  
}

#----------------------------------------------------------------------------------

rebuild <- function(resp, data, prmt.name) {
  
  # resp and data is already in the global environment
  # data is raw without modification
  # source(modif)
  
  # need one step to confirm the model, then 
  # fit <- fit.temp; df <- df.temp
  
  prmt.history <<- read.csv(paste(getwd(), "/", prmt.name, sep = ""))
  prmt.alive <- prmt.history[prmt.history$status == "alive"]
  
  for(i in 1:nrow(prmt.alive)) {
    
    pred <- prmt.alive[[1]][i]
    type <- prmt.alive[[2]][i]
    co.r <- prmt.alive[[3]][i]
    sc.1 <- prmt.alive[[4]][i]
    sc.2 <- prmt.alive[[5]][i]
    pc.r <- prmt.alive[[6]][i]
    object <- prmt.alive[[7]][i]
    
    df.history <<- modif(pred, type, data, co.r, sc.1, sc.2, pc.r, object)
    # type: character
  }
  fit.history <<- lm(as.formula(paste(c(resp, paste(pred, collapse = " + ")), collapse = " ~ ")), data)
}

#----------------------------------------------------------------------------------
# modif: created 11/12/2014 by Elliott
# trial: created 11/12/2014 by Katherine
# recom: created 11/13/2014 and modified 11/17/2014 by Elliott
# rebuild: created 11/12/2014 by Katherine
#----------------------------------------------------------------------------------
