##########################
## R training exercises ##
##########################

r.ex <- function(){
  
  # DEVELOPED FOR BH MSU R TRAINING
  # EXERCISES ARE PROVIDED BELOW FOR PRACTICE
  # AUTHOR: MINGMIN
  
  message("Welcome to MSU R training exercise system!")
  cat("\n")
  
  message("Every training session comes up with corresponding exercises.")
  message("No score will be granted.")
  message("Just practice and make sure you fully understand the content of training.")
  message("You can press 'Esc' anywhere when you want to exit.")
  cat("\n")
  
  message("You can try 3 times for each exercise.") 
  message("If you failed all, correct answer will be printed.")
  message("Don't be shy, just try! :P")
  
  no.ex <- function(){
    cat("\n")
    repeat{
      cat("Please choose the set of training exercises listed below:",
          " 1.1 - Basic Intro & Sequence",
          " 1.2 - Vector & Factor",
          " 1.3 - Missing values",
          " 1.4 - Subsetting Vectors",
          " 1.5 - Matrices & Data Frame",
          " ",
          " 0. Exit this exercise program",
          " ", sep = "\n")
      no.set <- readline("Please enter the session number: ")
      # check1 <- regular expression
      # check2 <- no.set %in% c(seq(1.1,1.5,0.1),seq(2.1,2.7,0.1),seq(3.1,3.5,0.1))
      if (no.set %in% c(0, seq(1.1,1.5,0.1),seq(2.1,2.7,0.1),seq(3.1,3.5,0.1))){
        break
      }else{
        message("Just enter number of exercise set listed above please.")
        message("i.e.: enter '1.2' (without quotation mark) for the exercise of 'Vector & List'.")
        cat("\n")
      }
    }
    
    no.set <- as.numeric(no.set)
    if (no.set == 0){
      cat("\n")
      message("Bye-bye~~")
      cat("\n")
      # return(the.end())
    }else if (no.set == 1.1){
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
        cat("Q1. Please enter the result of '3^2 + 17%%3'",
            "",
            sep = "\n")
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
            "",
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
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "3"){
          message("Great! You still have 2 questions to answer!")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4", "5")){
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
        }else{
          cat("\n")
          message("Only numbers between 1 and 5 are acceptable!")
          cat("\n")
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
            "  5. a <- 2:9; b <- 3:5; a+b leads to an warning message",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "3"){
          message("One question to go! Ganbade!")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4", "5")){
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
        }else{
          cat("\n")
          message("Only numbers between 1 and 5 are acceptable!")
          cat("\n")
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
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "1"){
          message("Congratulations! You have finished the exercise 1.1!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else if(ans %in% c("2", "3", "4")){
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
        }else{
          cat("\n")
          message("Only numbers between 1 and 4 are acceptable!")
          cat("\n")
        }
      }
      
      cat("Exercise 1.1 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          paste("You made ", q5-1, " mistake", if(q5<3){""}else{"s"}, " for Q5;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4+q5-5 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
      }
      
      repeat{
        cho <- readline("Do you want to do this exercise again or try other exercise (Y/N)? ")
        cat("\n")
        if (toupper(cho) == "Y"){
          return(no.ex())
        }else if (toupper(cho) == "N"){
          message("c ya~")
          cat("\n")
          break
        }else{
          message("Please only enter 'Y' or 'N'")
        }
      }
      
    }else if(no.set == 1.2){
      #---------------#
      # Exercises 1.2 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 1.2 - Vector & Factor"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 5 questions below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat("Q1. > vect <- c(3, 1, 2, 4)",
            "which statement below is right?",
            "  1. the data type of 'vect' is integer",
            "  2. the data type of 'vect' is numeric",
            "  3. > vect1 <- c(vect, FALSE); then vect1 is a vector of 5 logical values",
            "  4. > vect2 <- c(vect, FALSE, 'A'); vect2 is still a numeric vector",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        # ans.bis <- as.numeric(ans)
        if (ans == "2"){
          message("Correct! Let's move to the next question.")
          cat("\n")
          break
        }else if (ans %in% c("1", "3", "4")){
          q1 <- q1 + 1
          
          if(q1>3){
            message("Oops, game over! Correct answer below:")
            message("The data type of 'vect' is numeric!")
            message("Let's move to the next question.")
            cat("\n")
            break
          }else{
            message("Bang! Wrong answer! Try again~")
            message("Just enter a number between 1 and 4, without any other characters.")
            cat("\n")
          }
        }else{
          cat("\n")
          message("Only numbers between 1 and 4 are acceptable!")
          cat("\n")
        }
      }
      
      # Question 2
      q2 <- 1
      repeat{
        cat("Q2. 2 vectors of daily spends are created:",
            "> XiaoJuan <- c(125, 130, 135, 242, 163)",
            "> XiaoFang <- c(132, 184, 162, 212, 182)",
            "",
            "Which of the following is right?",
            "  1. XiaoJuan > XiaoFang",
            "  2. sum(XiaoJuan) > sum(XiaoFang)",
            "  3. Total spends can be calculated by command: > sum(XiaoJuan, XiaoFang)",
            "  4. Total spends can be calculated by command: > XiaoJuan + XiaoFang",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "3"){
          message("Hoorey~ Corret answer!")
          cat("\n")
          break
        }else if (ans %in% c("1", "2", "4")){
          q2 <- q2 + 1
          
          if (q2>3){
            message("Stop and think, and try it again if you want after all exercises finished.")
            message("The only right answer is 3.")
            cat("\n")
            break
          }else{
            message("Bang-bang! Try again please")
            cat("\n")
          }
        }else{
          cat("\n")
          message("Only numbers between 1 and 4 are acceptable!")
          cat("\n")
        }
      }
      
      # Question 3
      q3 <- 1
      repeat{
        cat("Run the following commands:",
            '> random <- c(400, -80, -40, 200, -120)',
            '> names(random) <- c("Mon", "Tue", "Wed", "Thu", "Fri")',
            "",
            "Which option is right?",
            "  1. > summary(random); and we get:",
            "     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.",
            "     -120     -80     -40      72     200     400 ",
            "",
            "  2. > table(random); and we get:",
            "     random",
            "     Mon  Tue  Wed  Thu  Fri ",
            "     400  -80  -40  200 -120",
            "",
            "  3. > levels(random); and we get:",
            '     [1] "Mon" "Tue" "Wed" "Thu" "Fri"',
            "",
            "  4. > name(random); and we get:",
            '     [1] "Mon" "Tue" "Wed" "Thu" "Fri"',
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "1"){
          message("Great! You still have 2 questions to answer!")
          cat("\n")
          break
        }else if(ans %in% c("2", "3", "4")){
          q3 <- q3 + 1
          
          if (q3>3){
            message("You have run out of 3 trials...")
            message("The correct answer is 1.")
            message("  - Using table() on a vector gets only counts of each value;")
            message("  - levels() is used for factor instead of a vector with names;")
            message("  - There is no functoin name(), only names() is available. :P")
            cat("\n")
            break
          }else{
            message("Ah-oh~ try again please~")
            cat("\n")
          }
        }else{
          cat("\n")
          message("Only numbers between 1 and 4 are acceptable!")
          cat("\n")
        }
      }
      
      # Question 4
      q4 <- 1
      repeat{
        cat("Q4. Which expression below is FALSE?",
            "  1. if 'ordered' is not specified, levels adapt alphabetic order without priority;",
            "  2. comparing a ordinal factor with a nominal one leads to an error message;",
            "  3. in factor(), parameters 'order' and 'ordered' have different functionality;",
            "  4. changing names of levels without considering order may lead to wrong info.",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "3"){
          message("One question to go! Ganbade!")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4")){
          q4 <- q4 + 1
          
          if (q4>3){
            message("Oulala~no more chance for this question~")
            message("The correct answer is the 3rd option.")
            cat("\n")
            break
          }else{
            message("Come on~ Try again!")
            cat("\n")
          }
        }else{
          cat("\n")
          message("Only numbers between 1 and 4 are acceptable!")
          cat("\n")
        }
      }
      
      # Question 5
      q5 <- 1
      repeat{
        cat("Q5. Check the commands below:",
            '> animal <- c("puppy", "piggy", "donkey", "monkey", "kitty") # Line 1',
            "> zoo <- c(rep(c(1, 2), each = 3), 3)                        # Line 2",
            "> names(zoo) <- animal                                       # Line 3",
            "> summary(zoo)                                               # Line 4",
            "> f.zoo <- factor(zoo, order = T)                            # Line 5",
            "Which line will lead to error message (no. of line)?",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "3"){
          message("Congratulations! You have finished the exercise 1.2!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4")){
          q5 <- q5 + 1
          
          if (q5>3){
            message("You have tried 3 times...")
            message("The correct answer is 3. length of 'zoo' is longer than that of 'animal'.")
            message("You can enter 'r.ex()' again to do this exercise again.")
            message("See you around~")
            cat("\n")
            break
          }else{
            message("Try~~~~again~~~~~")
            cat("\n")
          }
        }else{
          cat("\n")
          message("Only numbers between 1 and 4 are acceptable!")
          cat("\n")
        }
      }
      
      cat("Exercise 1.2 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          paste("You made ", q5-1, " mistake", if(q5<3){""}else{"s"}, " for Q5;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4+q5-5 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
      }
      
      repeat{
        cho <- readline("Do you want to do this exercise again or try other exercise (Y/N)? ")
        cat("\n")
        if (toupper(cho) == "Y"){
          return(no.ex())
        }else if (toupper(cho) == "N"){
          message("c ya~")
          cat("\n")
          break
        }else{
          message("Please only enter 'Y' or 'N'")
        }
      }
      
      
      
    }else{
      message("Not developed yet... To be released after next session... :P")
    }
    
    #     # dummy function
    #     the.end <- function(){
    #       conf <- "I won't let you see I'm gonna go"
    #     }
    #     the.end()
  }
  
  no.ex()
  
  
  
  # CREATION: 8/4/2014. EXERCISE 1.1
  # UPDATE1: 8/4/2014. ADD "EXIT/QUIT" OPTION TO THE MAIN MENU; ADD EXERCISE STATISTICS
  # UPDATE2: 8/18/2014. EXERCISE 1.2
  # UPDATE3: 8/18/2014. RESOLVE THE BUG: WHEN A RANDOM VALUE (I.E. SPACE) ENTERED, PROGRAM STOPS WITH ERROR MESSAGE
  
}
