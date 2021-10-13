#This is a file to takes a linear regression and outputs all the information needed in the BYU-Idaho class MATH 425: Applied Linear Regression


# Ideas List:
  # rounded how many decimals as an option in the parameters? Or an if statement for if it's blank
  # put in the information by hand or feed in an lm already performed?
  # all the diagnosis plots needed
  # the boxCox plot that tells you what to do?!
  # predict function?
  # make the psuedo code to make a graph of SSE, SSR, and SSTO




linear_regression_info <- function(x, y, data){
  
  mylm <- lm(y ~ x, data)
  
  #results to output
  paste0(
    #Residuals
    residuals <- mylm[]
    #SSE
    
    #SSR
    
    #SSTO
    
    #R2
    
  )
  
}





mylm2$coefficients[1] %>% round(2) # y-intercept estimate. Textbook Definition: "This is the estimate of the y-intercept, ??0. It is called b0. It is the average y-value when X is zero."
mylm2$coefficients[2] %>% round(2) # slope estimate. Textbook Definition: "This is the estimate of the slope, ??1. It is called b1. It is the change in the average y-value as X is increased by 1 unit."
mylm2$residuals[1] #? Indexing the residuals for each data point?
mylm2$residuals # all residuals?

mylm2$effects # all effects? What is an effect?

mylm2$rank #?

mylm2$fitted.values # all fitted values
mylm2$assign #?
mylm2$qr #??
mylm2$df.residual[1] # only one of them?
mylm2$xlevels #?
mylm2$call
mylm2$terms
mylm2$model # this just shows the values of the x and y variables. Not sure why it's useful