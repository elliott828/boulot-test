rebuild <- function(resp, data = df0, prmt.name) {
    
    # resp and data is already in the global environment
    # data is raw without modification
    # source(modif)
    
    # need one step to confirm the model, then 
    # fit <- fit.temp; df <- df.temp
    
    prmt.history <<- read.csv(paste(getwd(), "/", prmt.name, sep = ""))
    prmt.alive <- prmt.history[prmt.history$status == "alive"]
    
    for(i in 1:nrow(prmt.alive)) {
        
        pred <- prmt.alive[[1]][i]
        type <- prmt.alive[[2]][i]
        co.r <- prmt.alive[[3]][i]
        sc.1 <- prmt.alive[[4]][i]
        sc.2 <- prmt.alive[[5]][i]
        pc.r <- prmt.alive[[6]][i]
        object <- prmt.alive[[7]][i]
        
        df.history <<- modif(pred, type, data, co.r, sc.1, sc.2, pc.r, object)
        # type: character
    }
    
    fit.history <<- lm(as.formula(paste(c(resp, paste(pred, collapse = " + ")), collapse = " ~ ")), data = df.temp)
    
    
}