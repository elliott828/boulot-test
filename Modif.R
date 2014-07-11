Modif <- function(pred, data, co.r = NaN, pc.r = NaN, sc1 = NaN, sc2 = NaN){
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
  
  if(is.na(co.r)){
    # if no parameter for co rate, then no transformation happens
    message(paste("No transformation for variable '", pred, "'!", sep=""))
  }else if(is.na(sc1)){
    # if sc parameters is NA, then do co+pc transformation
    df[[pred]] <- pc(co(df[[pred]], co.r), pc.r)
  }else{
    df[[pred]] <- sc(co(df[[pred]], co.r), sc1, sc2)
  }
  return(df)
}
