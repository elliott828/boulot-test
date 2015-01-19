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