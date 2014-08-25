extra.ex <- function(){
  if (!"mail" %in% installed.packages()) install.packages("mail")
  library(mail)
  
  # grab the working directory
  workdir <- paste(getwd(),"/",sep="")
  
  # copy your name
  name <- readline("Please type your full name (using dot for combining, i.e: john.doe): ")
  
  # initial score
  ans131 <- 0
  ans132 <- 0
  
  
  repeat{
    cat("",
        "Which extra exercise do you want to submit?",
        "  1. Session 1.3, extra exercise 1",
        "  2. Session 1.3, extra exercise 2",
        "", sep = "\n")
    opt <- readline("Make your choice please: ")
    
    if (!opt %in% c(1,2)){
      message("Please enter a valid option number!")
    }else if (opt == "1"){
      
      #----------------#
      # Extra ex 1.3.1 #
      #----------------#
      
      exists131 <-  file.exists(paste(workdir,"mtcar1.csv",sep=""))
      if(exists131){
        extra131 <- read.csv(paste(workdir,"mtcar1.csv",sep=""))
        rownames(extra131) <- extra131[,1]
        extra131 <- extra131[,2:ncol(extra131)]
        ans <- mtcars[order(mtcars$mpg,decreasing=T),]
        
        if(sum(!extra131 == ans)==0){
          message("Bravo! Your first extra exercise is correct!")
          ans131 <- ans131 + 1
          if (ans131 == 1){
            time <- gsub(":",".",gsub(" ",".",Sys.time()))
            write.csv(ans131, paste("M://Drop Folders//Mingmin Z//R extra//",
                                    paste(name,"131.ok",time,"csv",sep="."),sep=""))
          }
        }else{
          message(":P You can try again~")
          ans131 <- ans131 + 10
        }
        
      }else{
        message('"mtcar1.csv" does not exist! Please check before submit your answer again!')
        
      }
      break
    }else if (opt == "2"){
      
      #----------------#
      # Extra ex 1.3.2 #
      #----------------#
      
      exists132 <-  file.exists(paste(workdir,"mtcar2.csv",sep=""))
      if(exists132){
        extra132 <- read.csv(paste(workdir,"mtcar2.csv",sep=""))
        rownames(extra132) <- extra132[,1]
        extra132 <- extra132[,2:ncol(extra132)]
        ans <- mtcars[order(mtcars$hp,decreasing=T),]
        ans <- ans[c(1:3,(nrow(ans)-2):nrow(ans)),c("mpg","cyl","hp","wt","qsec","vs")]
        
        if(sum(!extra132 == ans)==0){
          message("Bravo! Your first extra exercise is correct!")
          ans132 <- ans132 + 1
          if (ans132 == 1){
            time <- gsub(":",".",gsub(" ",".",Sys.time()))
            write.csv(ans131, paste("M://Drop Folders//Mingmin Z//R extra//",
                                    paste(name,"132.ok",time,"csv",sep="."),sep=""))
          }
        }else{
          message(":P You can try again~")
          ans132 <- ans132 + 10
        }
        
        
      }else{
        message('"mtcar2.csv" does not exist! Please check before submit your answer again!')
        
      }
      break
    }
  }
  
  # CREATION: 8/25/2014. 2 extra exercises checking programs created for session 1.3
  
}
