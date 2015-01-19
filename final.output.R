final.output <- fucntion(resp, data, fit, prmt, contri) {    		
    # prmt is a dataframe including 8 columns: 
    #       variable, trans.meth, co.r, sc.1, sc.2, pc.r, oth, status
    
    # Output: 
    
    
    # Needed Packages: "MASS" (for stepAIC()), "car" (for vif())             
    
    # 1.Transformation (trans.meth, co.r, sc.1, sc.2, pc.r, oth)
    write.csv(prmt, paste(getwd(), "/prmt.history.csv", sep = ""))
    message('Variable parameters history is exported to "prmt.csv" under default working directory')
    cat(paste(rep("-+-",20),collapse=""))
    
    # 2. Residuals
    resid <- cbind(data[, resp], fit$fitted.values, summary(fit)$residuals)
    rownames(resid) <- NULL
    colnames(resid) <- c(resp, "Prediction", "Residuals")
    
    write.csv(resid, paste(getwd(), "/residuals.csv", sep = ""))
    message('Value of Response Variable, Prediction and Residuals
            is exported to "residuals.csv" under default working directory')
    cat(paste(rep("-+-",20),collapse=""))
    
    # 3. Coefficients
    coef <- coef(summary(fit))     # Estimate  Std. Error    t value   Pr(>|t|)
    
    # 4. VIF
    if(length(coef(fit)) > 1) {
        vif <- vif(fit)
    }
    
    # Merge model information into one data frame
    # Output csv
    prmt.alive <- prmt[which(prmt$status == "alive"), ][, -8]
    
    if(rownames(coef)[1] == "(Intercept)"){
        prmt.alive <- rbind(NA, prmt.alive)
        vif <- rbind(NA, as.matrix(vif))
    }
    model <- as.data.frame(cbind(prmt.alive, coef, contri, vif))
    rownames(model) <- NULL
    colnames(model)[, -(1:11)] <- c("contri.rate", "VIF")

    write.csv(model, paste(getwd(), "/model.results.csv", sep = "")))
    message('Information of the Model 
            is exported to "model.results.csv" under default working directory')
    cat(paste(rep("-+-",20),collapse=""))
    
    # print(summary(stepAIC(fit, direction = "both", trace = 0)))        
    
}
			
