## 1. loop.output()
## 2. final.output()

loop.output <- function(resp, data, fit) {              		
  # Input: resp(name), data(modified), fit					
  # Output: only on memory and screen, nothing to files
  
  # Consists of 4 parts:  Part. I  Summary of Fit & MAPE
  #                       Part. II  Plots
  #                       Part. III  DW-test
  #                       Part. IV  Contribution Rates
  
  # Needed Packages: "zoo" (for library(lmtest)), "lmtest" (for dwtest())
  
  if(!"lmtest" %in% installed.packages()){
    install.packages("lmtest")
    require(lmtest)
  }
  
  readline('You can find the output of the model below.\nIn next 5 steps, please press <Enter> to continue...')
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. I  Summary of Fit
  #---------------------------------------------------------------
  
  readline("Part. I  Summary of Fit")
  print(summary(fit))	
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. II  MAPE
  #---------------------------------------------------------------
  
  
  readline("Part. II  MAPE")
  resp.temp <- data[[resp]]
  resp.temp[which(resp.temp == 0)] <- mean(data[[resp]]) 
  mape <<- mean(abs(fit$residuals/resp.temp))
  cat("MAPE of the model is ", round(mape, 4),"\n", sep = "")
  cat("\n")
  
  
  #---------------------------------------------------------------
  # Part. III  Plots
  #---------------------------------------------------------------
  
  # if(!is.null(dev.list())) invisible(dev.off()) # Clear the visible equipment
  op <- par(mar = rep(0, 4))   
  # plot.new()
  par(op)
  na <- readline("Part. III  Plots")
  message("Please look at the Plots area!")
  layout(matrix(c(1, 2,
                  3, 3), nr = 2, byrow = T))
  
  # 1. histogram for residuals
  hist(summary(fit)$residuals, main = "Histogram of Residuals", xlab = "Residuals")
  
  # 2. scatter points of residuals
  plot(summary(fit)$residuals, type = "p", pch = 21, bg = "black", 
       main = "Scatter of Residuals", ylab = "Residuals")
  abline(h = 0)
  par(new = FALSE)
  
  # 3. Actual Data vs. Modeled Data
  plot(data[, resp], type = "l", col = "blue", xlab = "", ylab = "")#,  axes = FALSE)
  
  # legend(2, 5, c("Actual", "Predicted"), fill = c("blue", "red"))
  # legend("topright",  c("Actual", "Predicted"), fill = c("blue", "red"), 
  #        border = "white", box.col = "white", box.lty = NULL)
  par(new = TRUE)
  
  # prediction <- (as.matrix(data[, names(coef(fit))[-1]]) 
  #    %*% as.vector(coef(fit)[-1]) + (coef(fit)[1]))
  plot(as.vector(fit$fitted.values), type = "l", col = "red", 
       xlab = "", ylab = "",  axes = FALSE, main = "Actual Data vs. Predicted Data")
  cat("\n")
  
  
  #---------------------------------------------------------------
  # Part. IV  DW-test
  #---------------------------------------------------------------
  
  na <- readline("Part. IV  DW-test")
  print(dwtest(fit))
  
  #---------------------------------------------------------------
  # Part. V  Contribution Rates
  #---------------------------------------------------------------    
  
  na <- readline("Part. V  Contribution Rates")
  cat("\n")
  
  # contri.d: draft    
  simulation <- cbind(coef(fit)[1], t(t(as.matrix(data[, names(coef(fit))[-1]])) * as.vector(coef(fit)[-1])))
  colnames(simulation) <- names(coef(fit))
  contri.d <- colSums(simulation)/sum(fit$fitted.values)
  contri <<- contri.d
  
  # contri.p: positive
  contri.p <- contri.d[which(contri.d >= 0)]
  contri.p <- as.matrix(contri.p[order(contri.p, decreasing = T)])
  
  contri.top10 <- as.matrix(head(contri.p[which(rownames(contri.p) != "(Intercept)")], 10))
  colnames(contri.top10) <- "Top POSITIVE Contributors"
  rn.p <- rownames(contri.p)[which(rownames(contri.p) != "(Intercept)")]
  rownames(contri.top10) <- if (length(rn.p)>10) rn.p[1:10] else rn.p
  
  
  # contri.n: negative
  contri.n <- contri.d[which(contri.d < 0)]
  contri.n <- as.matrix(contri.n[order(contri.n, decreasing = F)])
  
  contri.bot5 <- as.matrix(head(contri.n[which(rownames(contri.n) != "(Intercept)")], 5))
  colnames(contri.bot5) <- "Top NEGATIVE Contributors"
  rn.n <- rownames(contri.n)[which(rownames(contri.n) != "(Intercept)")]
  rownames(contri.bot5) <- if (length(rn.n)>5) rn.n[1:5] else rn.n
  
  
  # message('Voila the top contributor(s) to the variable "', resp, '":', sep ="")
  if(nrow(contri.top10) > 0){
    print(contri.top10)
    cat("\n")
  }
  if(nrow(contri.bot5) > 0){
    print(contri.bot5)
    cat("\n")
  }
  cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
  
  # percent <- function(x, digits = 4, format = "f") {
  #     paste0(formatC(100 * x, format = format, digits = digits), "%")
  # }
  # contri.top10 <- as.matrix(percent(head(contri.rate, 10)))
  # rownames(contri.top10) <- rownames(contri.rate)[1:10]
  
  #---------------------------------------------------------------
  # Part. VI  VIF (Updated on Friday 12/19/2014)
  #---------------------------------------------------------------  
  
  if(length(coef(fit)) > 2) {
    vif <- as.matrix(vif(fit))
    colnames(vif) <- "VIF"
    na <- readline("Part. VI  VIF")
    cat("\n")
    print(vif)
    cat("\n")
    
    if(any(vif >= 10)){
      message(paste("VIF of", paste(rownames(vif)[which(vif >= 10)], collapse = ", "), 
              if(length(which(vif >= 10)) > 1) "are" else "is", 
              "bigger than 10! ", sep = " "))
      cat("\n")
    }
  }
  
}	# end of function loop.output()					


final.output <- function(resp, data, fit, prmt, contri, aic = FALSE) {
  # prmt is a dataframe including 8 columns: 
  #       variable, trans.meth, co.r, sc.1, sc.2, pc.r, oth, status
  # data = df1 # Transformed Data
  
  # Needed Packages: "MASS" (for stepAIC()), "car" (for vif())   
  
  if(!"MASS" %in% installed.packages()){
    install.packages("MASS")
    require(MASS)
  }
  
  if(!"car" %in% installed.packages()){
    install.packages("car")
    require(car)
  }
  
  # 1.Transformation (trans.meth, co.r, sc.1, sc.2, pc.r, oth)
  # directly output as a csv file
  
  if(aic) aic.ind <- "aic." else aic.ind <- NULL
  
  write.csv(prmt, paste(aic.ind, "prmt.csv", sep = ""))
  message(paste('Variable parameters history is exported to "',
                paste(aic.ind, "prmt.csv", sep = ""),
                '" under default working directory',sep=""))
  cat(paste(rep("-+-",20),collapse=""))
  
  # --------------------------------------------------------------------------
  # put 2, 3, 4 into one data frame, and then output into a csv file
  
  # 2. Residuals
  resid <- cbind(data[, resp], fit$fitted.values, summary(fit)$residuals)
  rownames(resid) <- NULL
  colnames(resid) <- c(resp, "Prediction", "Residuals")
  
  write.csv(resid, paste(aic.ind, "residuals.csv", sep = ""))
  message(paste('Value of response variable, prediction and residuals are exported to',
                paste('the file "',paste(aic.ind, "residuals.csv", sep = ""),
                      '" under default working directory', sep=""), sep="\n"))
  cat(paste(rep("-+-",20),collapse=""))
  
  # 3. Coefficients
  coef <- coef(summary(fit))     # Estimate  Std. Error    t value   Pr(>|t|)
  
  # 4. VIF
  if(length(coef(fit)) > 2) {
    vif <- vif(fit)
  } else {
    vif <- NA
  }
  
  # Merge model information into one data frame
  # Output csv
  prmt.alive <- prmt[which(prmt[[8]] == "alive"), ][, -8]
  
  if(rownames(coef)[1] == "(Intercept)"){
    prmt.alive <- rbind(NA, prmt.alive)
    prmt.alive$pred.i <- c("(Intercept)", as.vector(prmt.alive$pred.i)[-1]) 
    # column "pred" is a factor, need to be changed to a vector for further modification
    vif <- rbind(NA, as.matrix(vif))
  }
  model <- as.data.frame(cbind(prmt.alive, coef, contri, vif), stringsAsFactors = F)
  rownames(model) <- NULL
  
  write.csv(model, paste(aic.ind, "model.results.csv", sep = ""))
  message(paste('The modeling result is exported to "', 
                paste(aic.ind, "model.results.csv", sep = ""),'".',sep=""))
  message('You could find the file under default working directory.')
  cat(paste(rep("-+-",20),collapse=""),"\n\n")
  
  # output transformed data to local file
  # write.csv(data[, c(resp, as.character(prmt.alive[, 1]))], "transformed.data.csv")
  write.csv(data, "transformed.data.csv")
  
  # print(summary(stepAIC(fit, direction = "both", trace = 0)))        
  
} # end of function final.output()
