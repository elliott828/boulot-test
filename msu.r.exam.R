MSU.R.EXAM <- function(){
  message("\nHi MSUers, Dang!Dang!Dang!Dang~~~~!")
  message("Finally! The final exam comes!\n")
  message("There are 20 questions in this exam,")
  message("each of them has only one correct answer.")
  message("You have 2 chances to finish the exam within 2 days.\n")
  message("The exam is (supposed to be) delivered on Wed. 12/3/2014.")
  message("That means you have to finish it before 12/5/2014 11:59 A.M.\n")
  message("Have fun with it and good luck! :D\n")
  readline("Press <Enter> to continue...\n")
  
  # install package "mail" if it's not available.
  if (!"mail" %in% installed.packages()) install.packages("mail")
  require(mail)
  
  # copy user name & email address
  name <- readline("Please enter your full name (using dot for combining, i.e: john.doe): ")
  cat("\n")
  repeat{
    email1 <- readline("Please enter your working email address: ")
    email2 <- readline("Please confirm the email address: ")
    cat("\n")
    if (email1 != email2){
      message("The 2 email addresses are not the same, please enter again!\n")
    }else{
      break
    }
  }
  
  # send out the 1st message: when the exam starts
  # time <- gsub(":",".",gsub(" ",".",Sys.time()))
  time <- format(Sys.time(),"%H:%M, %m-%d-%Y")
  sendmail("zhu.mingmin@thebluehive.com", 
           subject = paste(name,": Exam starts at ",time,sep=""), 
           message = email1,
           password = "rmail")
  sendmail("katherine.hou@thebluehive.com", 
           subject = paste(name,": Exam starts at ",time,sep=""), 
           message = email1,
           password = "rmail")
  message("EXAM STARTS! TICKTACK!")
  message("(Embrace the storm!)\n")
  
  ans <- NA
  i <- 1
  
  # question 1
  # Content in Session 1.1
  repeat{
    cat("Q1. Follow the steps below:",
        "> x <- 5",
        "> y <- 6",
        "> z <- 7",
        "> num <- x^2 + y/2 + z%%3",
        "What's the value of variable 'num'?",
        " 1. 14",
        " 2. 15",
        " 3. 29",
        " 4. 30",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i+1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Let's move to the next question.\n")
  
  # question 2
  # Content in Session 1.1
  repeat{
    cat("Q2. Which expression below is FALSE?",
        "  1. pi:5 creates vector: 3.141593 4.141593",
        "  2. a <- 1:5; b <- seq(2,10,2); then b/a equals to 2",
        "  3. a <- 1:5; b <- 6:10; then b%*%a equals to 1*1 matrice",
        "  4. a <- 2:9; b <- 3:5; a+b generates a warning message",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Next question!\n")
  
  # question 3
  # Content in Session 1.3
  repeat{
    cat("Q3. Which expression below is FALSE concerning a dataset 'df' with more than 10",
        "    observations and more than 5 numeric variables?",
        "  1. head(df) returns the first 5 rows of 'df';",
        "  2. str(df) returns the structure of 'df';",
        "  3. tail(df) == df[(nrow(df)-5):nrow(df),];",
        "  4. summary(df) returns summraries (min, max, mean, median, etc.) for each variable.",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Next question~\n")
  
  # question 4
  # Content in Session 2.1
  repeat{
    cat("Q4. A self-defined function is created:",
        "> above <- function(x, n = 10) {",
        "+             use <- x > n",
        "+             x[use]",
        "+ }",
        "",
        "Which of the following is RIGHT?",
        " 1. n = 10 means that n must equal to 10;",
        " 2. There are three parameters in this function;",
        " 3. The function's name is called 'above';",
        " 4. If length(x) = 10, then the length of output must also be 10.",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Let's move to the next question.\n")
  
  # question 5
  # Content in Session 2.4
  repeat{
    cat("Q5. Run the commands below to build a model and check its summary:",
        "    (below: mpg - miles per gallon; hp - horse power)",
        "  > fit <- lm(mpg ~ hp, data = mtcars)",
        "  > summary(fit)",
        "", sep = "\n")
    cat(paste(rep("-",50),collapse=""))
    fit <- lm(mpg ~ hp, data = mtcars)
    print(summary(fit))
    cat(paste(rep("-",50),collapse=""),"",sep="\n")
    cat("What does the slope tell us in the context of the relationship between",
        "fuel economy (miles per gallon) and the weight of a car?",
        "",
        "  1. For each additional horse power a car has, the model predicts",
        "     0.068 less miles per gallon, on average.",
        "  2. For each additional horse power a car has, the model predicts",
        "     0.068 more miles per gallon, on average.",
        "  3. For each additional  horse power a car has, the model predicts",
        "     30.099 less miles per gallon, on average.",
        "  4. For each additional  horse power a car has, the model predicts",
        "     30.099 more miles per gallon, on average.",
        "", sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Next question lah~\n")
  
  # question 6
  # Content in Session 3.1
  repeat{
    cat("Q6. Which expression below is FALSE?",
        "  1. The x of plot(x,...) could be a model formula.",
        "  2. Boxplot usually applies on a categorical and a numeric data.",
        "  3. hist() can be used to plot non-numeric data.",
        "  4. Based on the graphic generated by plot(),",
        "     abline() adds a line to it.",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("N-E-X-T question!\n")
  
  # question 7
  # Content in Session 2.2
  repeat{
    cat("Q7. Suppose the working directory is folder 'R-training' in D Drive,",
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
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("NEXT QUESTION...\n")
  
  # question 8
  # Content in Session 1.2
  repeat{
    cat("Q8. Run the following commands:",
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
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Hayaku!\n")
  
  # question 9
  # Content in Session 2.3
  repeat{
    cat("Q9. Which command has absolutely INCORRECT answer?",
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
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Close to halfway~~~ let's move on!")
  
  # question 10
  # Content in Session 2.2
  repeat{
    cat("Q10. Look at the paragraph below:",
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
        "  3. > first <- 'I figure life is a gift and I donnot intend on wasting it.'",
        "     > second <- 'You never know what hand you are going to get dealt next.'",
        "     > third <- 'You learn to take life as it comes at you.'",
        "     > warnings(first, second, third, sep = '\\n')",
        "",
        "  4. > cat('I figure life is a gift and I donnot intend on wasting it.',",
        "     +     'You never know what hand you are going to get dealt next.',",
        "     +     'You learn to take life as it comes at you.', sep='\\n')",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("You have finished half of the exam! Keep going!\n")
  
  # question 11
  # Content in Session 2.4
  repeat{
    plot(summary(fit)$residuals)
    abline(0,0)
    cat("Q11. Please check the model below (between mpg and wt(weight)):\n")
    fit <- lm(mpg ~ wt, data = mtcars)
    print(summary(fit))
    cat("Concerning the residual plot of the model above, which statement is FALSE?",
        "  1. The residuals appear to be randomly distributed around 0;",
        "  2. The residuals show a curved pattern;",
        "  3. The plot is indicative of a linear relationship between fuel economy",
        "     and car weight;",
        "  4. The cars with a very high residual compared to the others appears to be outliers.", 
        "", sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Keep going!\n")
  
  # question 12
  # Content in Session 1.3
  repeat{
    cat("Q12. Which command CANNOT get the matrix/data frame correctly named?",
        "  1. > b <- data.frame(matrix(1:12, 3 ,4))",
        "     > rownames(b) <- c('row1','row2','row3')",
        "     > colnames(b) <- c('col1','col2','col3','col4')",
        "",
        "  2. > a <- matrix(1:12, 3, 4)",
        "     > rownames(a) <- c('row1','row2','row3')",
        "     > colnames(a) <- c('col1','col2','col3','col4')",
        "",
        "  3. > b <- data.frame(matrix(1:12, 3 ,4))",
        "     > names(b) <- c('col1','col2','col3','col4')",
        "     > row.names(b) <- c('row1','row2','row3')",
        "",
        "  4. > a <- matrix(1:12, 3, 4,", 
        "                   dimnames = c('row1','row2','row3',",
        "                                'col1','col2','col3','col4'))",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("8 questions left!\n")
  
  # question 13
  # Content in Session 3.2
  repeat{
    cat("Q13. Which statement about AutoReg() is NOT right?",
        "  1. The file you need to use should always be of .csv format when it is read by AutoReg()",
        "     from local drive;",
        "  2. If you are not satisfied with the performance of a transformed variable in the model, ",
        "     you always have chance to remove it in AutoReg();",
        "  3. If you choose a transformation method for a variable, you can set the name of the new",
        "     transformed variable by yourself;",
        "  4. Coefficients information of the final model can be exported to local drive.",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("7 questions left!\n")
  
  # question 14
  # Content in Session 1.2
  repeat{
    cat("Q14. > vect <- c(6, 1, 10, 4)",
        "which statement below is RIGHT?",
        "  1. > vect1 <- c(vect, FALSE); then vect1 is a vector of 5 logical values",
        "  2. > vect2 <- c(vect, FALSE, 'A'); vect2 is still a numeric vector",
        "  3. the data type of 'vect' is integer",
        "  4. the data type of 'vect' is numeric",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("6 questions left!\n")
  
  # question 15
  # Content in Session 1.4
  repeat{
    cat("Q15. Which command below might generate error message?",
        '  1. > matr <- matrix(rep(LETTERS[1:6],2), byrow = T, 3, 4)',
        '  2. > vect <- c("a", b, 1, T)',
        '  3. > fact <- factor(c(LETTERS[3:1],T,1),ordered = T)',
        '  4. > list <- list("This is a test", rep(c("Rock","Roll"),3))',
        '',
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("You have completed 3/4 of the exam!\n")
  
  # question 16
  # Content in Session 2.3
  repeat{
    cat("Q16. Which clause can calculate the upper bound of a confidence interval?",
        "     Suppose: sample mean = 2, standard error = 1.5, confidence level = 90%",
        "  1. > coef <- pnorm(0.9)",
        "     > upper <- 2 + coef * 1.5",
        "  2. > coef <- qnorm(-0.1)",
        "     > upper <- 2 + coef * 1.5",
        "  3. > coef <- abs(qnorm(0.1, lower.tail = F))",
        "     > upper <- 2 + coef * 1.5",
        "  4. > coef <- abs(qnorm(-0.05))",
        "     > upper <- 2 + coef * 1.5",
        "", sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("4 questions left!\n")
  
  # question 17
  # Content in Session 1.3
  repeat{
    cat("Q17. Which statements below is FALSE:",
        "  1. A matrix can only store one data type;",
        "  2. Data frame is often used for complicated data analysis because of",
        "     its capability of storing multiple data types;",
        "  3. Functions like colSums(), rowMeans(), etc. can only be used on matrix;",
        "  4. Functions data.frame() and as.data.frame() have same effect when we want",
        "     to coerce a matrix to data frame without considering column names.",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("The dark hour before dawn!\nCount down: 3!\n")
  
  # question 18
  # Content in Session 2.1
  repeat{
    cat("Q18. Check the commands below:",
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
        "Which of the following is RIGHT?",
        ' 1. achieve(80, 80) outputs "Medium"',
        " 2. This function can output all the students' achievements at a time",
        ' 3. achieve(95, 50) still outputs "Fail" regardless of high score 95',
        ' 4. achieve(95, 90) outputs "Excellent"',
        "",
        sep = "\n")
    
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("Don't give up! Count down: 2!\n")
  
  # question 19
  # Content in Session 1.4
  repeat{
    cat("Q19. Create a list by code below:",
        "  > Intro <- 'The most popular films!'",
        "  > Films <- matrix(c('Inception','Lost in Thailand',",
        "                      'Avatar','Titanic','3 Idiots',",
        "                      9.2, 8.4, 8.8, 9.5, 9.0,", 
        "                      0.6, 12.7, 13.8, 9.5, 0.5),",
        "                     nrow = 5, ncol = 3,",
        "                     dimnames = list(1:5,",
        "                                     c('Names','Rating','Box.Office(b)')))",
        "  > Nation <- list('They are from:',",
        "                   c('U.S.','China','U.S.','U.S.','India'))",
        "  > FILM <- list(INTRO=Intro, INFO=Films, NATION=Nation)",
        "",
        "  Which command can get the RATING of Titanic?",
        "  1. > FILM[2][4,2]",
        "  2. > FILM[2]$Rating[4]",
        "  3. > FILM$INFO[4,2]",
        "  4. > FILM[['INFO']][4,][,2]",
        "",
        sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  message("The first whitish rays have appeared above the horizon! Count down: 1!\n")
  
  # question 20
  # Content in Session 2.4
  repeat{
    cat("Q20. Which statement below is TRUE:",
        "  1. Backward and forward stepwise regression based on same dataset will eventually ",
        "     lead to a same model;",
        "  2. Backward stepwise regression always removes the variable which has the smallest",
        "     p-value of its t-test;",
        "  3. Forward stepwise regression always adds significant predictor which brings",
        "     the best R-square;",
        "  4. Forward stepwise regression always removes the variable which has the largest",
        "     p-value of its t-test",
        "", sep = "\n")
    temp <- readline("Your answer (enter the option number): ")
    cat("\n")
    if (temp %in% as.character(1:4)){
      ans[i] <- as.numeric(temp)
      i <- i + 1
      break
    } else {
      message("Only numbers between 1 and 4 are acceptable!\n")
    }
  }
  time <- format(Sys.time(),"%H:%M, %m-%d-%Y")
  sendmail("zhu.mingmin@thebluehive.com", 
           subject = paste(name,": Exam ends at ",time,sep=""), 
           message = paste(email1,paste(ans,collapse=","),sep = "\n"),
           password = "rmail")
  sendmail("katherine.hou@thebluehive.com", 
           subject = paste(name,": Exam ends at ",time,sep=""), 
           message = paste(email1,paste(ans,collapse=","),sep = "\n"),
           password = "rmail")
  
  message("Do you still feel cold...")
  message("Do you...")
  message("Don't be panic! There are 5 more questions waiting for you!!!")
  readline("Press <Enter> to continue...\n")
  cat(rep("\n", 10))
  message("Dang!Dang!Dang! Congratulations! You have finished all!")
  message("Your exam result will be granted as soon as we receive your message.")
  message("I suppose you don't want see me again...\n")
  message("So... See you... Don't miss me...")
}
