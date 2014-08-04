##########################
## R training exercises ##
##########################

r.ex <- function(){
  
  # DEVELOPED FOR BH MSU R TRAINING
  # EXERCISES ARE PROVIDED BELOW FOR PRACTICE
  # DEVELOPED BY MINGMIN
  
  message("Welcome to MSU R training exercise system!")
  message("Every training session comes up with corresponding exercises.")
  message("No score will be granted.")
  message("Just practice and make sure you fully understand the content of training.")
  message("You can press 'Esc' anywhere when you want to exit.")
  cat("\n")
  
  message("You can try 3 times for each exercise.") 
  message("If you failed all, correct answer will be printed.")
  message("Don't be shy, just try! :P")
  
  no.ex <- function(){
  # choose the number of set of exercises
  
    cat("\n")
    repeat{
      cat("Please choose the set of training exercises listed below:",
          " 1.1 - Basic Intro & Sequence",
          " 1.2 - Vector & List",
          " 1.3 - Missing values",
          " 1.4 - Subsetting Vectors",
          " 1.5 - Matrices & Data Frame",
          " ", sep = "\n")
      no.set <- readline("Please enter the session number: ")
      # check1 <- regular expression
      # check2 <- no.set %in% c(seq(1.1,1.5,0.1),seq(2.1,2.7,0.1),seq(3.1,3.5,0.1))
      if (no.set %in% c(seq(1.1,1.5,0.1),seq(2.1,2.7,0.1),seq(3.1,3.5,0.1))){
        break
      }else{
        message("Just enter number of exercise set listed above please.")
        message("i.e.: enter '1.2' (without quotation mark) for the exercise of 'Vector & List'.")
        cat("\n")
      }
    }
    
    if (no.set == 1.1){
      #---------------#
      # Exercises 1.1 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 1.1 - Basic Intro & Sequence"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 5 questions below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat("Q1. Please enter the result of '3^2 + 17%%3'")
        ans <- readline("Your answer: ")
        ans.bis <- as.numeric(ans)
        if (ans.bis == 11){
          message("Correct! Let's move to the next question.")
          cat("\n")
          break
        }else{
          q1 <- q1 + 1
          
          if(q1>3){
            message("Oops, game over! Correct answer below:")
            message("3^2 + 17%%3 = 11")
            message("Let's move to the next question.")
            cat("\n")
            break
          }else{
            message("Bang! Wrong answer! Try again~")
            message("Just enter a number, without any other characters.")
            cat("\n")
          }
        }
      }
      
      # Question 2
      q2 <- 1
      repeat{
        cat("Q2. Follow the steps below:",
            "> apple <- 5",
            "> banana <- 6",
            "> pear <- 7",
            "> fruit <- apple + banana + pear",
            "What's the value of variable 'fruit'?",
            sep = "\n")
        ans <- readline("Your answer: ")
        ans.bis <- as.numeric(ans)
        if (ans.bis == 18){
          message("Hoorey~ Corret answer!")
          cat("\n")
          break
        }else{
          q2 <- q2 + 1
          
          if (q2>3){
            message("Stop and think, and try it again if you want after all exercises finished.")
            message("The value of variable 'fruit' is 18.")
            cat("\n")
            break
          }else{
            message("Bang-bang! Try again please")
            cat("\n")
          }
        }
      }
      
      # Question 3
      q3 <- 1
      repeat{
        cat("Q3. I want to create a vector with 10 elements:",
            "  4 8 12 16 20 24 28 32 36 40",
            "Which method below CANNOT help me to realize that?",
            "  1. c(4, 8, 12, 16, 20, 24, 28, 32, 36, 40)",
            "  2. seq(4, 40, 4)",
            "  3. seq(40, 4, 4)",
            "  4. 1:10 * 4",
            "  5. seq(4, 40, length = 10)",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        ans.bis <- as.numeric(ans)
        if (ans.bis == 3){
          message("Great! You still have 2 questions to answer!")
          cat("\n")
          break
        }else{
          q3 <- q3 + 1
          
          if (q3>3){
            message("You have run out of 3 trials...")
            message("The correct answer is '3. seq(40 ,4, 4)', that leads to error message.")
            cat("\n")
            break
          }else{
            message("Ah-oh~ try again please~")
            cat("\n")
          }
        }
      }
      
      # Question 4
      q4 <- 1
      repeat{
        cat("Q4. Which expression below is FALSE?",
            "  1. a <- 1:5; seq(along = a) gets the same result as seq_along(a)",
            "  2. pi:5 creates vector: 3.141593 4.141593",
            "  3. a <- 1:5; b <- seq(2,10,2); then b/a equals to 2",
            "  4. a <- 1:5; b <- 6:10; then b%*%a equals to 1*1 matrice",
            "  5. a <- 2:9; b <- 3:5; a+b leads to an error message",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        ans.bis <- as.numeric(ans.bis)
        if (ans.bis == 3){
          message("One question to go! Ganbade!")
          cat("\n")
          break
        }else{
          q4 <- q4 + 1
          
          if (q4>3){
            message("Oulala~no more chance for this question~")
            message("The correct answer is the 3rd option.")
            message("The division of 2 numeric vector (same length) leads to a vector of same length")
            cat("\n")
            break
          }else{
            message("Come on~ Try again!")
            cat("\n")
          }
        }
      }
      
      # Question 5
      q5 <- 1
      repeat{
        cat("Q5. Vector computation",
            "> a <- 1:5",
            "> b <- 3:7",
            "> c <- 6:10",
            "What is the result of 'c * 2 + b %/% a + sqrt(9) + 1:10'?",
            "  1. 19 21 23 26 29 24 26 28 31 34",
            "  2. 16 19 24 27 30 21 24 29 32 35",
            "  3. 19 21 23 26 29",
            "  4. 16 19 24 27 30",
            sep = "\n")
        ans <- readline("Your answer: ")
        ans.bis <- as.numeric(ans)
        
        if (ans.bis == 1){
          message("Congratulations! You have finished the exercise 1.1!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else{
          q5 <- q5 + 1
          
          if (q5>3){
            message("You have tried 3 times...")
            message("The correct answer is '1. 19 21 23 26 29 24 26 28 31 34'")
            message("You can enter 'r.ex()' again to do this exercise again.")
            message("See you around~")
            cat("\n")
            break
          }else{
            message("Try~~~~again~~~~~")
            cat("\n")
          }
        }
      }
      
      repeat{
        cho <- readline("Do you want to do this exercise again or try other exercise (Y/N)? ")
        cat("\n")
        if (toupper(cho) == "Y"){
          return(no.ex())
        }else if (toupper(cho) == "N"){
          message("c ya~")
          break
        }else{
          message("Please only enter 'Y' or 'N'")
        }
      }
      
    }else{
      message("Not developed yet... To be released after next session... :P")
    }
  }
  
  no.ex()
  
  # CREATION: EXERCISE 1.1 BASIC INTRO & SEQUENCE; 8/4/2014
  
}
