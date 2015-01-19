loop.output <- function(resp, data, fit) {    					
    # Input: resp(name), data(modified), fit					
    # Output: only on memory and screen, nothing to files
    
    # Consists of 4 parts:  Part. I  Summary of Fit & MAPE
    #                       Part. II  Plots
    #                       Part. III  DW-test
    #                       Part. IV  Contribution Rates
    
    # Needed Packages: "lmtest" (for dwtest()), "zoo" (for library(lmtest))
    
    
    #---------------------------------------------------------------
    # Part. I  Summary of Fit & MAPE
    #---------------------------------------------------------------
    
    cat("\n")
    readline("Part. I  Summary of Fit & MAPE")
    print(summary(fit))	
    mape <<- mean(abs(as.vector(fit$residuals))/data[, resp])
    cat("MAPE of the model is ", mape, "\n")
    cat("\n")
    
    
    #---------------------------------------------------------------
    # Part. II  Plots
    #---------------------------------------------------------------
    
    if(!is.null(dev.list())) invisible(dev.off()) # Clear the visible equipment
    na <- readline("Part. II  Plots")
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
    plot(data[, resp], type = "l", col = "blue", xlab = "", ylab = "",  axes = FALSE)

    # legend(2, 5, c("Actual", "Predicted"), fill = c("blue", "red"))
    # legend("topright",  c("Actual", "Predicted"), fill = c("blue", "red"), 
    #        border = "white", box.col = "white", box.lty = NULL)
    par(new = TRUE)
    
    # prediction <- (as.matrix(data[, names(coef(fit))[-1]]) 
    #    %*% as.vector(coef(fit)[-1]) + (coef(fit)[1]))
    plot(as.vector(fit$fitted.values), type = "l", col = "red", 
        xlab = "", ylab = "", main = "Actual Data vs. Predicted Data")
    cat("\n")
    
    
    #---------------------------------------------------------------
    # Part. III  DW-test
    #---------------------------------------------------------------
	
    na <- readline("Part. III  DW-test")
    print(dwtest(fit))
    cat("\n")
    
    					
    #---------------------------------------------------------------
    # Part. IV  Contribution Rates
    #---------------------------------------------------------------    

    na <- readline("Part. IV  Contribution Rates")
    
    simulation <- cbind(coef(fit)[1], as.matrix(data[, names(coef(fit))[-1]]) * as.vector(coef(fit)[-1]))
    colnames(simulation)[1] <- "(Intercept)"
    contri <<- sapply(as.data.frame(simulation), sum)/sum(fit$fitted.values)
    contri <- as.matrix(contri[order(contri, decreasing = T)])

    contri.top10 <- as.matrix(head(contri[ - which(rownames(contri)=="(Intercept)")], 10))
    rownames(contri.top10) <- rownames(contri)[ - which(rownames(contri)=="(Intercept)")]
    contri.top10
    
    # percent <- function(x, digits = 4, format = "f") {
    #     paste0(formatC(100 * x, format = format, digits = digits), "%")
    # }
    # contri.top10 <- as.matrix(percent(head(contri.rate, 10)))
    # rownames(contri.top10) <- rownames(contri.rate)[1:10]
    			
}						
