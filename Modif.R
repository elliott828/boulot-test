Modif <- function(pred, data, co.r = NaN, pc.r = NaN, sc.1 = NaN, sc.2 = NaN){
  # transform specified variable with parameters
  # this function **return a dataset** with certain variable transformed
  # make sure the raw data is already loaded into global environment
  
  # source('BasicTrans.R')
  df <- data
  
  # check if the variable exists in the data frame
  check <- pred %in% names(df)
  if (check == FALSE){
    stop(paste("The variable '", pred, "' does not exist in the database!", sep=""))
  }
  pred <- as.character(pred) # ensure input like factor to be coerced to character
  
  if(is.na(co.r)){
    # if no parameter for co rate, then no transformation happens
    message(paste("No transformation for variable '", pred, "'!", sep=""))
  }else if(is.na(sc.1)){
    # if sc parameters is NA, then do co+pc transformation
    df[[pred]] <- pc(co(df[[pred]], co.r), pc.r)
  }else{
    df[[pred]] <- sc(co(df[[pred]], co.r), sc.1, sc.2)
  }
  return(df)
}
