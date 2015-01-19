trial <- function(data, resp, fit = NULL, action = 2, pred = NULL) {    				
    # Build the model				
    # based on NULL or fit1, add/remove a predictor/the intercept, output summary				
    
    # action: -1 means delete the Intercept
    #          1 means add the Intercept
    #          2 means add a predictor
    #         -2 means delete a predictor
    
    if(action == -1 | action == 1) pred <- "(Intercept)"
        
    if(is.null(fit)){ # if(exists("fit", mode = "list"))
        if(action == 1) {
            lm(as.formula(sprintf('%s ~ 1', resp)), data = data, na.action = na.exclude)
        } else if(action == 2) {
            lm(as.formula(sprintf('%s ~ %s + 0', resp, pred)), data = data, na.action = na.exclude)
        } else if(action == -1) {
            lm(as.formula(sprintf('%s ~ -1', resp)), data = data, na.action = na.exclude)
        } else { # if(action == -2)
            warning("There's no existed model to let you delete ", pred, " from!")
        }
            
    } else { #if(!is.null(fit))
        if(pred %in% names(coef(fit))) {
            if(action == -1) {
                update(fit, ~. -1)
            } else if(action == -2) {
                update(fit, as.formula(sprintf('~. - %s', pred)))
            } else {
                warning(if(action == 1)"Intercept"else pred, " is already in the model!")
            }
        } else {
            if(action == 1){
                update(fit, ~. +1)
            } else if(action == 2){
                update(fit, as.formula(sprintf('~. + %s', pred)))
            } else {
                warning(if(action == -1)"Intercept"else pred, " isn't in the model!")
            }
        }    
    }    
}            
            
            
