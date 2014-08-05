ParaS <- function(pred, resp, data, model = NULL, method = 3){

  ##########################
  ## Parameters Selection ##
  ##########################
  
  # Transformation of single predictor/variable by 2 steps:
  # First step: carry-over
  # Second step: power curve or s curve
  # Auto-select the best parameters
  
  # pred - the predictor/variable which need transformation
  # resp - the respondent variable in the model to be studied
  # data - database to be studied
  
  # model - if the var. is tested within a model, need to provide the model
  # method - 1: carry-over + power; 2: carry-over+s; 3:auto-select between 1 & 2; 
  #          4: no transforming. Default-value: 3 - auto-select
  # sig.level - list the coefs with p-value of test under a certain level. Default = 0.05

  # make sure the raw data is already loaded into global environment
  
  df <- data    # prepare a new data frame - to receive the transformed variable
  
  source("BasicTrans.R")  
  #read 3 transformation functions co(), pc() & sc()
  #read 3 transformation test functions meth.c.p(), meth.c.s() & cp.vs.cs()
  
  if(method == 1){
    meth.c.p(pred, resp, data, model)
  }else if(method == 2){
    meth.c.s(pred, resp, data, model)
  }else if(method == 3){
    cp.vs.cs(pred, resp, data, model)
  }else if(method == 4){
    if (is.null(model)){
      mdl <- lm(as.formula(sprintf('%s ~ %s', resp, pred)), data = df, na.action = na.exclude)
    }else{
      mdl <- update(model, as.formula(sprintf('~. + %s', pred)), data = df)
    }
    pos <- which(names(coef(mdl)) == pred)
    arsq <- summary(mdl)$adj.r.square
    coef <- c(coef(summary(mdl))[pos, 1],
              coef(summary(mdl))[pos, 4])
    result <- list(arsq, coef)
    names(result) <- c("adjusted.r.square", "coef.and.p.value")
    return(result)
  }else{
    stop("Come on! Please enter the correct method option number from 1 to 4!")
  }
  
}
