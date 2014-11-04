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
  na <- readline("Press 'ENTER' to continue...")
  cat("\n")
  message("You can try 3 times for each exercise.") 
  message("If you failed all, correct answer will be printed.")
  message("Don't be shy, just try! :P")
  
  # when users want to do extra exercises, check and adapt corresponding sourcing codes.
  call.extra <- function(){
    repeat{
      cat("Which system are you using?", 
          "  1. WINDOWS",
          "  2. OS X", "", sep="\n")
      syst <- as.numeric(readline("Your system? "))
      cat("\n")
      if(!syst %in% as.character(c(1,2))){
        message("Please enter valid number!")
      }else if(syst == 1){
        source("https://raw.githubusercontent.com/elliott828/boulot-test/master/extra.ex.R")
        break
      }else if(syst == 2){
        if(!"RCurl" %in% installed.packages()){install.packages("RCurl")}
        library(RCurl)
        ex <- getURL("https://raw.githubusercontent.com/elliott828/boulot-test/master/extra.ex.R",
                     ssl.verifypeer=0L, followlocation=1L)
        writeLines(ex, "temp.R")
        source("temp.R")
        break
      }
    }
  }
  
  no.ex <- function(){
    cat("\n")
    repeat{
      cat("Please choose the set of training exercises listed below:",
          "PART I - BASIC CONCEPT",
          "  1.1 - Basic Intro & Sequence",
          "  1.2 - Vector & Factor",
          "  1.3 - Matrix, Data Frame & Subsetting",
          "  1.4 - List & Missing Values",
          "",
          "PART II - ADVANCED OPERATION",
          "  2.1 - Control Flow & Self-defined Function",
          "  2.2 - Input & Output",
          "  2.3 - Fundamental Statistics",
          "  2.4 - Linear Regression Model",
          "",
          "PART III - PLOTTING & APPLICATION",
          "  3.1 - Basic Intro to Graphic Tools",
          "  3.2 - MSU Modeling Tool",
          "",
          "0. Exit this exercise program",
          " ", sep = "\n")
      no.set <- readline("Please enter the session number: ")
      # check1 <- regular expression
      # check2 <- no.set %in% c(seq(1.1,1.5,0.1),seq(2.1,2.7,0.1),seq(3.1,3.5,0.1))
      if (no.set %in% c(0, seq(1.1,1.4,0.1),seq(2.1,2.4,0.1),seq(3.1,3.2,0.1))){
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
            "Which line will lead to a problem (not necessarily an error or warning message)?",
            "",
            sep = "\n")
        ans <- readline("Your answer (no. of line): ")
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
          message("Only numbers between 1 and 5 are acceptable!")
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
      
    }else if (no.set == 1.3){
      
      #---------------#
      # Exercises 1.3 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 1.3 - Matrix, Data Frame & Subsetting"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 5 exercises and 2 optional ones below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat("Q1. Which command CANNOT get the matrix/data frame correctly named?",
            "  1. > a <- matrix(1:12, 3, 4,", 
            "                   dimnames = c('row1','row2','row3',",
            "                                'col1','col2','col3','col4'))",
            "",
            "  2. > a <- matrix(1:12, 3, 4)",
            "     > rownames(a) <- c('row1','row2','row3')",
            "     > colnames(a) <- c('col1','col2','col3','col4')",
            "",
            "  3. > b <- data.frame(matrix(1:12, 3 ,4))",
            "     > rownames(b) <- c('row1','row2','row3')",
            "     > colnames(b) <- c('col1','col2','col3','col4')",
            "",
            "  4. > b <- data.frame(matrix(1:12, 3 ,4))",
            "     > names(b) <- c('col1','col2','col3','col4')",
            "     > row.names(b) <- c('row1','row2','row3')",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        # ans.bis <- as.numeric(ans)
        if (ans == "1"){
          message("Correct! Let's move to the next question.")
          cat("\n")
          break
        }else if (ans %in% c("2", "3", "4")){
          q1 <- q1 + 1
          
          if(q1>3){
            message("Oops, game over! Correct answer below:")
            message("The value of argument 'dimnames' should be a 'list'!")
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
        cat("Q2. Which statements below is FALSE:",
            "  1. A matrix can only store one data type;",
            "  2. Data frame is often used for complicated data analysis because of",
            "     its capability of storing multiple data types;",
            "  3. Functions like colSums(), rowMeans(), etc. can only be used on matrix;",
            "  4. Functions data.frame() and as.data.frame() have same effect when we want",
            "     to coerce a matrix to data frame without considering column names.",
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
        cat("Run the following command and create a matrix 'matr':",
            '> matr <- matrix(11:22, byrow = T, 3, 4)',
            "",
            "Which command will get an logical value 'TRUE'?",
            "  1. > matr[5] == 15",
            "  2. > matr[2,3] == 17",
            "  3. > matr[1:3] == c(11,12,13)",
            "  4. > matr[,2] - matr[,1] == rep(3,3)",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "2"){
          message("Great! You still have 2 questions to answer!")
          cat("\n")
          break
        }else if(ans %in% c("1", "3", "4")){
          q3 <- q3 + 1
          
          if (q3>3){
            message("You have run out of 3 trials...")
            message("The correct answer is 2.")
            message("Pay attention on the direction of data filling!")
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
        cat("Q4. Which expression below is FALSE concerning a dataset 'df' with more than 10",
            "    observations and more than 5 numeric variables?",
            "  1. head(df) returns the first 5 rows of 'df';",
            "  2. str(df) returns the structure of 'df';",
            "  3. tail(df) == df[(nrow(df)-5):nrow(df),];",
            "  4. summary(df) returns summraries (min, max, mean, median, etc.) for each variable.",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "1"){
          message("One question to go! Ganbade!")
          cat("\n")
          break
        }else if(ans %in% c("2", "3", "4")){
          q4 <- q4 + 1
          
          if (q4>3){
            message("Oulala~no more chance for this question~")
            message("The correct answer is the 1st option.")
            message("head() returns the first 6 rows of a dataset by default.")
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
        cat("Q5. A data frame is created:",
            '> a <- c(1,5,2,3)',
            "> b <- c(T,F,T,F)",
            "> c <- LETTERS[4:7]",
            "> df <- data.frame(a,b,c)",
            "",
            "Which combination below is TRUE?",
            "1. > is.vector(df$a) & is.matrix(df)",
            "2. > is.character(df$c) & is.logical(df$b)",
            "3. > (is.data.frame(df) | is.character(df$c)) & sum(df$a)==11",
            "4. > is.numeric(as.matrix(df)[,1])",
            "",
            sep = "\n")
        ans <- readline("Your answer (no. of line): ")
        if (ans == "3"){
          message("Congratulations! You have finished the exercise 1.3!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4")){
          q5 <- q5 + 1
          
          if (q5>3){
            message("You have tried 3 times...")
            message("The correct answer is 3.")
            message("  - df$c is a factor, instead of a character vector;")
            message("  - after being turned to matrix, df$a becomes a character vector.")
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
      
      cat("Exercise 1.3 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          paste("You made ", q5-1, " mistake", if(q5<3){""}else{"s"}, " for Q5;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4+q5-5 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
        cat("\n")
      }
      
      # optional exercises 1.3
      repeat{
        opt <- readline("Do you want to do 2 more optional exercises (Y/N)? ")
        cat("\n")
        if (toupper(opt) == "Y"){
          message("Da~da~. You've earned yourself a chance of getting additional points!")
          message("Each correct answer will be granted 1 point.")
          cat("\n")
          message("The extra exercises can only be done when you quit r.ex().")
          message("Please read the instructions before you quit r.ex().")
          cat("\n")
          
          call.extra()
          
          cat("\n")
          na <- readline("Press 'ENTER' to continue...")
          cat("\n")
          
          # opt ex 1.3.1
          extra1.3.1 <- c("SESSION 1.3, EXTRA EX.1",
                          "PLEASE FOLLOW THE INSTRUCTION:",
                          "1. Call the built-in dataset: mtcars;",
                          "2. Type '?mtcars' to read the introduction of this dataset;",
                          "3. Use head(), tail(), str() to check its basic structure;",
                          "4. Sorting the DATASET by the variable mpg in DESCENDING order,",
                          "   (the largest value of miles/gallon is put in the 1st place),",
                          "   save the reordered dataset under data frame name 'mtcar1';",
                          "5. Type the 2 command lines as below (ignore the symbol '>'):",
                          "   > write.csv(mtcar1, 'mtcar1.csv')",
                          "   > extra.ex()")
          
          cat(extra1.3.1, "", sep = "\n")
          write.table(extra1.3.1, file="instruction_ex1-3-1.txt", eol="\n",
                      row.names = F, col.names = F, quote = F)
          message("You can also find this instruction in your working directory")
          message("under the name of 'instruction_ex1-3-1.txt'")
          cat("\n")
          message("You only have one chance to submit this exercise. :P")
          message("But don't be scared, no point will be substracted even you submit wrong answer.")
          cat("\n")
          
          na <- readline("If you are clear about the ex.1, press 'ENTER' to read ex.2...")
          cat("\n")
          
          # opt ex 1.3.2
          extra1.3.2 <- c("SESSION 1.3, EXTRA EX.2",
                          "PLEASE FOLLOW THE INSTRUCTION:",
                          "1. Call the built-in dataset: mtcars;",
                          "2. Subset a dataset from mtcars which meets conditions below:",
                          "   a) Allocate the top and bottom 3 cars by invesgating their horsepower;",
                          "   b) Keep 6 information in the subsetted dataset:",
                          "      * Milse/(US) gallon",
                          "      * Number of cylinders",
                          "      * Gross horsepower",
                          "      * Weight",
                          "      * 1/4 mile time",
                          "      * V/S",
                          "3. Save the subsetted dataset under data frame name 'mtcar2';",
                          "4. Type the 2 command lines as below (ignore the symbol '>'):",
                          "   > write.csv(mtcar2, 'mtcar2.csv')",
                          "   > extra.ex()")
          
          cat(extra1.3.2, "", sep = "\n")
          write.table(extra1.3.2, file="instruction_ex1-3-2.txt", eol="\n",
                      row.names = F, col.names = F, quote = F)
          message("You can also find this instruction in your working directory")
          message("under the name of 'instruction_ex1-3-2.txt'")
          cat("\n")
          message("You only have one chance to submit this exercise. :P")
          message("Don't mix up the order of variables in this dataset.")
          message("Don't type extra.ex() before you are 100% sure about your answer.")
          cat("\n")
          
          message("ENJOY THE EXERCISE LAH~")
          cat("\n")
          
          break
        }else if (toupper(opt) == "N"){
          break
        }else{
          message("Please only enter 'Y' or 'N'!")
        }
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
          message("Please only enter 'Y' or 'N'!")
        }
      }
      
    }else if (no.set == 1.4){
      
      #---------------#
      # Exercises 1.4 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 1.4 - List & Missing Values"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 4 exercises below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat("Q1. Which statement about 'list' is CORRECT:",
            "  1. A list can only store 1 data type;",
            "  2. Elements of list should be of same length;",
            "  3. Elements of list can be subsetted by both [[ ]] and [] operators;",
            "  4. A list cannot store another list.",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        # ans.bis <- as.numeric(ans)
        if (ans == "3"){
          message("Correct! Let's move to the next question.")
          cat("\n")
          break
        }else if (ans %in% c("1", "2", "4")){
          q1 <- q1 + 1
          
          if(q1>3){
            message("Oops, game over! Correct answer is 3!")
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
        cat("Q2. Create a list by code below:",
            "  > Intro <- 'The most popular soccer stars!'",
            "  > SoccerStars <- matrix(c('C.Ronaldo','Messi','Roben','Hazard',",
            "                             28, 26, 30, 23, 15, 12, 8, 6),",
            "                           nrow = 4, ncol = 3,",
            "                           dimnames = list(1:4,",
            "                                           c('Names','Age','Goals')))",
            "  > Nationality <- list('They are from:',",
            "                         c('Portugal','Argentina','Netherland','Belgium'))",
            "  > Soccer <- list(INTRO=Intro, STARS=SoccerStars, NATION=Nationality)",
            "",
            "  Which command can get the age of Roben?",
            "  1. > Soccer[2][3,2]",
            "  2. > Soccer[2]$Age[3]",
            "  3. > Soccer$STARS[3,2]",
            "  4. > Soccer[['STARS']][3,][,2]",
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
        cat("Which command below generates error message?",
            '  > matr <- matrix(rep(LETTERS[1:6],2), byrow = T, 3, 4)',
            '  > vect <- vector("a", b, 1, T)',
            '  > fact <- factor(c(LETTERS[3:1],T,1),ordered = T)',
            '  > list <- list("This is a test", rep(c("Rock","Roll"),3))',
            '',
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "2"){
          message("Great! One question to go!")
          cat("\n")
          break
        }else if(ans %in% c("1", "3", "4")){
          q3 <- q3 + 1
          
          if (q3>3){
            message("You have run out of 3 trials...")
            message("The correct answer is 2.")
            message("The 2nd element is not a valid value!")
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
        cat("Q4. There is a vector with missing value:",
            "  > Ohlala <- sample(c(rnorm(1000),rep(NA,1000)),20)",
            "",
            "  Which command CAN replace the NA by the average value of valid numbers?",
            "  1. > Ohlala[is.na(Ohlala)] <- mean(Ohlala, na.rm = T)",
            "  2. > Ohlala[!complete.cases(Ohlala)] <- mean(Ohlala)",
            "  3. > Ohlala[Ohlala == 'NA'] <- mean(Ohlala, na.rm = T)",
            "  4. > Ohlala[Ohlala == NA] <- mean(Ohlala)",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "1"){
          message("Excellent!")
          cat("\n")
          break
        }else if(ans %in% c("2", "3", "4")){
          q4 <- q4 + 1
          
          if (q4>3){
            message("Oulala~no more chance for this question~")
            message("The correct answer is the 1st option.")
            message("NA cannot be measured by logical operators!")
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
      
      cat("Exercise 1.4 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4-4 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
        cat("\n")
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
          message("Please only enter 'Y' or 'N'!")
        }
      }
      
    }else if (no.set == 2.1){
      
      #---------------#
      # Exercises 2.1 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 2.1 - Control Flow & Self-defined Function"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 5 questions below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat('Q1. which statement below is the functionality of "repeat" loops?',
            " 1. testing a condition;",
            " 2. execute a loop within a fixed number of times;",
            " 3. execute a loop while a condition is true;",
            " 4. execute an infinite loop until a 'break' clause shows up.",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        # ans.bis <- as.numeric(ans)
        if (ans == "4"){
          message("Correct! Let's move to the next question.")
          cat("\n")
          break
        }else if (ans %in% c("1", "3", "2")){
          q1 <- q1 + 1
          
          if(q1>3){
            message("Oops, game over! Correct answer below:")
            message("Repeat loops execute an infinite loop!")
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
        cat("Q2. Which loop below may not be able to execute an infinite loop theoretically?",
            "    PS: An infinite loop may not be expected and may cause problems.",
            " 1. while",
            " 2. for",
            " 3. repeat",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "2"){
          message("Great! ")
          cat("\n")
          break
        }else if(ans %in% c("1", "3")){
          q2 <- q2 + 1
          
          if (q2>3){
            message("You have run out of 3 trials...")
            message("The correct answer is 2.")
            message(' - "while" loops may go infinitely when the condition is always true;')
            message(' - "for" loops have a definition which says execute a loop a fixed number of times;')
            message(' - "repeat" loops have a definition which says execute an infinite loop.')
            cat("\n")
            break
          }else{
            message("Ah-oh~ try again please~")
            cat("\n")
          }
          
        }else{
          cat("\n")
          message("Only numbers between 1 and 3 are acceptable!")
          cat("\n")
        }
      }
      
      # Question 3
      q3 <- 1
      repeat{
        cat("Q3. Which expression below is TRUE?",
            ' 1. One "if" clause can only be followed by one "else" or "else if";',
            " 2. Different loops cannot be nested;",
            ' 3. "break" is the only way to stop a "repeat" loop;',
            " 4. R will always give warnings or messages when issues occur.",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "3"){
          message("Oh! Yeah! You still have 2 questions to answer!")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4")){
          q3 <- q3 + 1
          
          if (q3>3){
            message("Oulala~no more chance for this question~")
            message("The correct answer is the 3rd option.")
            message("Some situations which R doesn't think are errors may cause problems to us.")
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
      
      # Question 4
      q4 <- 1
      repeat{
        cat("Q4. A self-defined function is created:",
            "> above <- function(x, n = 10) {",
            "+             use <- x > n",
            "+             x[use]",
            "+ }",
            "",
            "Which of the following is right?",
            " 1. n = 10 means that n must equal to 10;",
            " 2. There are three parameters in this function;",
            " 3. The function's name is called 'above';",
            " 4. If length(x) = 10, then the length of output is also 10.",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "3"){
          message("Hoorey~ Corret answer! Only one question to go! Ganbade!")
          cat("\n")
          break
        }else if (ans %in% c("1", "2", "4")){
          q4 <- q4 + 1
          
          if (q4>3){
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
      
      # Question 5
      q5 <- 1
      repeat{
        cat("Q5. Check the commands below:",
            "> achieve <- function(mat, eng) { # input scores for Maths and English test",
            "+                    if(mat + eng > 185) {",
            '+                         ach <- "Excellent"',
            "+                    } else if(mat + eng > 175) {",
            '+                         ach <- "Great"',
            "+                    } else if(mat + eng > 155) {",
            '+                         ach <- "Good"',
            "+                    } else if((mat > 65) & (eng > 65)) {",
            '+                         ach <- "Medium"',
            "+                    } else if((mat < 60) | (eng < 60)) {",
            '+                         ach <- "Fail"',
            "+                    } else {",
            '+                         ach <- "Work Harder"',
            "+                    }",
            "+                    ach",
            "+            }",
            "",
            "Which of the following is right?",
            ' 1. achieve(80, 80) outputs "Medium"',
            " 2. This function can output all the students' achievements at a time",
            ' 3. achieve(95, 50) still outputs "Fail" regardless of high score 95',
            ' 4. achieve(95, 90) outputs "Excellent"',
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "3"){
          message("Congratulations! You have finished the exercise 1.5!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4")){
          q5 <- q5 + 1
          
          if (q5>3){
            message("You have tried 3 times...")
            message("The correct option is 3")
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
      cat("Exercise 2.1 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          paste("You made ", q5-1, " mistake", if(q5<3){""}else{"s"}, " for Q5;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4+q5-5 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
        cat("\n")
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
          message("Please only enter 'Y' or 'N'!")
        }
      }
      
      
    }else if (no.set == 2.2){
      
      #---------------#
      # Exercises 2.2 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 2.2 - Input & Output"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 5 exercises below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat("Q1. Suppose the working directory is folder 'R-training' in D Drive,",
            "    which command/operation CANNOT source 'r.ex.R' file in this folder?",
            "  1. R / RStudio Navigation Bar => Open File", 
            "     => Allocate 'r.ex.R' file in folder 'R-training'",
            "     => Select the 'r.ex.R' and click the button 'Open'",
            "",
            "  2. > source('D:\\R-training\\r.ex.R')",
            "",
            "  3. > source('D:\\\\R-training\\\\r.ex.R')",
            "",
            "  4. > source('D:/R-training/r.ex.R')",
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
            message("Single backslash is invalid for demonstrating the address in R!")
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
        cat("Q2. Which statements below is FALSE:",
            "  1. write.csv() can only export a '.csv' file;",
            "  2. read.xlsx() is a built-in function, we don't need to install packages;",
            "  3. A '.csv' file can also be exported by write.table();",
            "  4. read.table() cannot import a '.xlsx' file.",
            "", sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "2"){
          message("Hoorey~ Corret answer!")
          cat("\n")
          break
        }else if (ans %in% c("1", "3", "4")){
          q2 <- q2 + 1
          
          if (q2>3){
            message("Stop and think, and try it again if you want after all exercises finished.")
            message("The only right answer is 2. read.xlsx() is in package 'XLConnect'.")
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
        cat("Suppose we have 3 files in '.csv', '.xls', '.xlsx' with exact same content,",
            "(imagine they are exported by write.csv, write.xlsx with dataset 'mtcars')",
            "which command below leads to a different data frame than others", 
            "after import function runs?",
            "",
            "  1. > read.table('mtcars.csv')",
            "  2. > read.csv('mtcars.csv')",
            "  3. > read.xlsx('mtcars.xls',1)",
            "  4. > read.xlsx('mtcars.xlsx',1)",
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
            message("Reading '.csv' by read.table() need to specify header=T and sep =','.")
            message("Pay attention on the direction of data filling!")
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
        cat("Q4. Look at the paragraph below:",
            "",
            "    I figure life is a gift and I donnot intend on wasting it.",
            "    You never know what hand you are going to get dealt next.",
            "    You learn to take life as it comes at you.",
            "",
            "  Which option below can realize that?",
            "",
            "  1. > print('I figure life is a gift and I donnot intend on wasting it.')",
            "     > print('You never know what hand you are going to get dealt next.')",
            "     > print('You learn to take life as it comes at you.')",
            "",
            '  2. > message("I figure life is a gift and I donnot intend on wasting it.")',
            '     > message("You never know what hand youre going to get dealt next.")',
            '     > message("You learn to take life as it comes at you.")',
            "",
            "  3. > cat('I figure life is a gift and I donnot intend on wasting it.',",
            "     +     'You never know what hand you are going to get dealt next.',",
            "     +     'You learn to take life as it comes at you.', sep='\\n')",
            "",
            "  4. > first <- 'I figure life is a gift and I donnot intend on wasting it.'",
            "     > second <- 'You never know what hand you are going to get dealt next.'",
            "     > third <- 'You learn to take life as it comes at you.'",
            "     > warnings(first, second, third, sep = '\\n')",
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
        cat("Q5. Which statement below is TRUE:",
            "  1. library() downloads an uninstalled package;",
            "  2. warnings() prints last warning message(s);",
            "  3. cat() prints a character vector;",
            "  4. source() cannot read a R script from the Internet.",
            "", sep = "\n")
        ans <- readline("Your answer (no. of line): ")
        if (ans == "2"){
          message("Congratulations! You have finished the exercise 2.2!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else if(ans %in% c("1", "3", "4")){
          q5 <- q5 + 1
          
          if (q5>3){
            message("You have tried 3 times...")
            message("The correct answer is 2.")
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
      
      cat("Exercise 2.2 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          paste("You made ", q5-1, " mistake", if(q5<3){""}else{"s"}, " for Q5;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4+q5-5 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
        cat("\n")
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
          message("Please only enter 'Y' or 'N'!")
        }
      }
      
    }else if (no.set == 2.3){
      
      #---------------#
      # Exercises 2.3 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 2.3 - Fundamental Statistics"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 5 exercises below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat("Q1. Which statement below is correct regarding 95% Confidence Interval?",
            "  1. We expect 95% of the random samples (with same size)",
            "     to contain the true population mean",
            "  2. 95% confident that the sample mean in this interval",
            "  3. 95% of the time the true population mean will be in this interval",
            "  4. We expect 95% of the intervals to contain the true sample mean",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        # ans.bis <- as.numeric(ans)
        if (ans == "1"){
          message("Correct! Let's move to the next question.")
          cat("\n")
          break
        }else if (ans %in% c("1", "3", "4")){
          q1 <- q1 + 1
          
          if(q1>3){
            message("Oops, game over! Correct answer below:")
            message("The correct answer is option1!")
            message("Confidence interval also contains sample mean!")
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
        cat("Q2. Which clause can calculate the upper bound of a confidence interval?",
            "    Suppose: sample mean = 2, standard error = 1.5, confidence level = 90%",
            "  1. > coef <- pnorm(0.9)",
            "     > upper <- 2 + coef * 1.5",
            "  2. > coef <- abs(qnorm(-0.05))",
            "     > upper <- 2 + coef * 1.5",
            "  3. > coef <- abs(qnorm(0.1, lower.tail = F))",
            "     > upper <- 2 + coef * 1.5",
            "  4. > coef <- qnorm(-0.1)",
            "     > upper <- 2 + coef * 1.5",
            "", sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "2"){
          message("Hoorey~ Corret answer!")
          cat("\n")
          break
        }else if (ans %in% c("1", "3", "4")){
          q2 <- q2 + 1
          
          if (q2>3){
            message("Stop and think, and try it again if you want after all exercises finished.")
            message("The only right answer is 2.")
            message("Z-value cannot be calculated by pnorm();")
            message("Be careful while using argument 'lower.tail=FALSE'.")
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
        cat("Q3. Which command has absolutely INCORRECT answer?",
            "Suppose: x <- -10:10",
            "  1. > sample(x[x>8])",
            "       [1] 10 9",
            "  2. > sample(x[x>7], 10, replace = T)",
            "       [1] 10  9 10  9  9 10  8  9 10 10",
            "  3. > sample(x[x>9)])",
            "       [1] 10",
            "  4. > sample(x[x<(-5)], 5)",
            "       [1]  -8  -7 -10  -6  -9",
            "",
            sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "3"){
          message("Great! You still have 2 questions to answer!")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4")){
          q3 <- q3 + 1
          
          if (q3>3){
            message("You have run out of 3 trials...")
            message("Command no. 3 generates permutation of 1:10!")
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
        cat("Q4. Which statement below is TRUE:",
            "  1. Suppose mean = mu, standard error = se, the 2 lines have same result:",
            "     > prob <- pnorm(mu+se) - pnorm(mu-se)",
            "     > prob <- 1 - pnorm(mu+se, lower.tail = F) - pnorm(mu-se)",
            "",
            "  2. For standard normal distribution:",
            "     > qnorm(0.8,lower.tail=F) == qnorm(.3)",
            "",
            "  3. The command below generates error message:",
            "     > sample(1, 100, replace = T)",
            "",
            "  4. For standard normal distribution:", 
            "     > dnorm(0) == 1/sqrt(2*pi)*exp(-1/2)",
            "", sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "1"){
          message("One question to go! Ganbade!")
          cat("\n")
          break
        }else if(ans %in% c("2", "3", "4")){
          q4 <- q4 + 1
          
          if (q4>3){
            message("Oulala~no more chance for this question~")
            message("The correct answer is the 1st option.")
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
        cat("Q5. Which statement below is TRUE:",
            "  1. Higher Z value, higher significance level;",
            "  2. In normal distribution, smaller standard deviation means steeper",
            "     curve and narrower spread (regarding the bell curve);",
            "  3. Standard error of a sample shares the same formula of standard",
            "     deviation of population;",
            "  4. dnorm(x) reflects the probability of observing the value x.",
            "", sep = "\n")
        ans <- readline("Your answer (no. of line): ")
        if (ans == "2"){
          message("Congratulations! You have finished the exercise 2.3!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else if(ans %in% c("1", "3", "4")){
          q5 <- q5 + 1
          
          if (q5>3){
            message("You have tried 3 times...")
            message("The correct answer is 2.")
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
      
      cat("Exercise 2.3 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          paste("You made ", q5-1, " mistake", if(q5<3){""}else{"s"}, " for Q5;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4+q5-5 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
        cat("\n")
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
          message("Please only enter 'Y' or 'N'!")
        }
      }
      
    }else if (no.set == 2.4){
      
      #---------------#
      # Exercises 2.4 #
      #---------------#
      cat("\n")
      msm <- "Training Exercise 2.4 - Linear Regression Model"
      message(msm)
      message(rep("-", nchar(msm)))
      message("There are 5 exercises below for this training session.")
      cat("\n")
      
      # Question 1
      q1 <- 1 # initialize the count of answer times
      repeat{
        cat("Q1. Which statement below is NOT right?",
            "  1. Scatterplot can be used to describe the relationship ",
            "     between 2 categorical variables;",
            "  2. We can use mosaic plot to explore the relation between ",
            "     2 categorical variables;",
            "  3. Boxplot is mostly used to compare a numeric variable per",
            "     different aspect of a categorical variable;",
            "  4. Histogram put numeric data in adjacent intervals.cells of",
            "     same interval length.",
            "",
            sep = "\n")
        ans <- readline("Your answer: ")
        # ans.bis <- as.numeric(ans)
        if (ans == "1"){
          message("Correct! Let's move to the next question.")
          cat("\n")
          break
        }else if (ans %in% c("2", "3", "4")){
          q1 <- q1 + 1
          
          if(q1>3){
            message("Oops, game over! Correct answer below:")
            message("The answer is option1!")
            message("Scatterplot compares numeric variables!")
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
        attach(mtcars)
        plot(mpg ~ hp)
        detach(mtcars)
        cat("Q2. Looking at the plot, which of the following BEST describe the relationship",
            "    between the two variables?",
            "    (data source: mtcars; mpg: miles per gallon; hp: horsepower)",
            "  1. The relationship is positive, linear, and very weak. ",
            "     There are no outliers.",
            "  2. The relationship is positive, linear, and moderately strong. ",
            "     One of the potential outliers is a car with approximately 350 horsepower.",
            "  3. The relationship is negative, linear, and moderately strong. ",
            "     One of the potential outliers is a car with approximately 350 horsepower.",
            "  4. The relationship is negative, linear, and very weak. ",
            "     One of the potential outliers is a car with approximately 350 horsepower.",
            "", sep = "\n")
        ans <- readline("Your answer: ")
        if (ans == "3"){
          message("Hoorey~ Corret answer!")
          cat("\n")
          break
        }else if (ans %in% c("1", "2", "4")){
          q2 <- q2 + 1
          
          if (q2>3){
            message("Stop and think, and try it again if you want after all exercises finished.")
            message("The right answer is 3.")
            message("The relationship between the 2 variables are nearly negatively linear.")
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
        cat("Q3. Run the commands below to build a model and check its summary:",
            "  > fit <- lm(mpg ~ wt, data = mtcars)",
            "  > summary(fit)",
            "", sep = "\n")
        cat(paste(rep("-",50),collapse=""))
        fit <- lm(mpg ~ wt, data = mtcars)
        print(summary(fit))
        cat(paste(rep("-",50),collapse=""),"",sep="\n")
        cat("What does the slope tell us in the context of the relationship between",
            "fule economy (miles per gallon) and the weight of a car?",
            "",
            "  1. For each additional thousand pounds a car weights, the model predicts",
            "     5.34 less miles per gallon, on average.",
            "  2. For each additional thousand pounds a car weights, the model predicts",
            "     5.34 more miles per gallon, on average.",
            "  3. For each additional thousand pounds a car weights, the model predicts",
            "     37.29 less miles per gallon, on average.",
            "  4. For each additional thousand pounds a car weights, the model predicts",
            "     37.29 more miles per gallon, on average.",
            "", sep = "\n")
        
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "1"){
          message("Great! You still have 2 questions to answer!")
          cat("\n")
          break
        }else if(ans %in% c("2", "3", "4")){
          q3 <- q3 + 1
          
          if (q3>3){
            message("You have run out of 3 trials...")
            message("The correct answer is option 1!")
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
        plot(summary(fit)$residuals)
        abline(0,0)
        cat("Q4. Concerning the residual plot of previous model, which statement is FALSE?",
            "  1. The residuals appear to be randomly distributed around 0;",
            "  2. The residuals show a curved pattern;",
            "  3. The plot is indicative of a linear relationship between fuel economy",
            "     and car weight;",
            "  4. The cars with a very high residual compared to the others appears to be outliers.", 
            "", sep = "\n")
        ans <- readline("Your answer (enter the option number): ")
        if (ans == "2"){
          message("One question to go! Ganbade!")
          cat("\n")
          break
        }else if(ans %in% c("1", "3", "4")){
          q4 <- q4 + 1
          
          if (q4>3){
            message("Oulala~no more chance for this question~")
            message("The correct answer is the 2nd option.")
            message("There is no curve pattern regarding the residuals.")
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
        cat("Q5. Which statement below is TRUE:",
            "  1. Backward and forward stepwise regression based on same dataset will",
            "     eventually lead to a same model;",
            "  2. Backward stepwise regression always removes the variable which has the smallest",
            "     p-value of its t-test;",
            "  3. Forward stepwise regression always adds significant predictor which brings",
            "     the best R-square;",
            "  4. Forward stepwise regression always removes the variable which has the largest",
            "     p-value of its t-test",
            "", sep = "\n")
        ans <- readline("Your answer (no. of line): ")
        if (ans == "3"){
          message("Congratulations! You have finished the exercise 2.4!")
          message("See you in next set of exercises~")
          cat("\n")
          break
        }else if(ans %in% c("1", "2", "4")){
          q5 <- q5 + 1
          
          if (q5>3){
            message("You have tried 3 times...")
            message("The correct answer is 3.")
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
      
      cat("Exercise 2.4 statistics:",
          paste("You made ", q1-1, " mistake", if(q1<3){""}else{"s"}, " for Q1;", sep =""),
          paste("You made ", q2-1, " mistake", if(q2<3){""}else{"s"}, " for Q2;", sep =""),
          paste("You made ", q3-1, " mistake", if(q3<3){""}else{"s"}, " for Q3;", sep =""),
          paste("You made ", q4-1, " mistake", if(q4<3){""}else{"s"}, " for Q4;", sep =""),
          paste("You made ", q5-1, " mistake", if(q5<3){""}else{"s"}, " for Q5;", sep =""),
          "", sep = "\n")
      
      if(q1+q2+q3+q4+q5-5 == 0){
        message("You made no mistake at all in this set of exercise! Congratulations!")
        cat("\n")
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
          message("Please only enter 'Y' or 'N'!")
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
  
  
  
  # CREATION: 8/4/2014. EXERCISES 1.1
  # UPDATE1: 8/4/2014. ADD "EXIT/QUIT" OPTION TO THE MAIN MENU; 
  #                    ADD EXERCISE STATISTICS
  # UPDATE2: 8/18/2014. EXERCISES 1.2
  # UPDATE3: 8/18/2014. RESOLVE THE BUG: WHEN A RANDOM VALUE (I.E. SPACE) ENTERED, 
  #                     PROGRAM STOPS WITH ERROR MESSAGE
  # UPDATE4: 8/22/2014. EXERCISES 1.3
  # UPDATE5: 8/25/2014. OPTIONAL EXERCEISES FOR SESSION 1.3
  # UPDATE6: 10/22/2014. EXERCISES 1.4
  # UPDATE7: 10/29/2014. EXERCISES 2.1 BY KATHERINE.HOU
  # UPDATE8: 10/30/2014. EXERCISES 2.2
  # UPDATE9: 10/31/2014. EXERCISES 2.3
  # UPDATE10: 11/3/2014. EXERCISES 2.4
  # UPDATE11: 11/4/2014. EXERCISES STRUCTURE MODIFICATION;
  #                      BUG FIXED IN EXERCISES 2.4: Q1 LOOP PROBLEM.
}
