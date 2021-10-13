library(car)
library(tidyverse)
View(Davis) 
colnames(Davis)


# Question 1 --------------------------------------------------------------



mylm <- lm(height ~ weight, data = Davis)
summary(mylm)

mylm$coefficients[1] %>% round(2)
mylm$coefficients[2] %>% round(2)


par(mfrow=c(1,1))
plot(height ~ weight, data = Davis)
abline(mylm)

par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)


SSR <- sum( (mylm$fit - mean(Davis$height))^2 )

SSTO <- sum( (Davis$height - mean(Davis$height))^2 )
             
R2 <- (SSR/SSTO) 
R2 %>% round(2)



# * Part E ------------------------------------------------------------------

View(Davis)

Davis2 <- Davis %>% 
  filter(weight != 166 & height != 57 & repwt != 56 & repht != 163)


mylm2 <- lm(height ~ weight, data = Davis2)
summary(mylm2)

mylm2$coefficients[1] %>% round(2) # y-intercept estimate. Textbook Definition: "This is the estimate of the y-intercept, ??0. It is called b0. It is the average y-value when X is zero."
mylm2$coefficients[2] %>% round(2) # slope estimate. Textbook Definition: "This is the estimate of the slope, ??1. It is called b1. It is the change in the average y-value as X is increased by 1 unit."

mylm <- lm(height ~ weight, data = Davis)
mylm2 <- lm(height ~ weight, data = Davis2)

par(mfrow=c(1,1))
plot(height ~ weight, data = Davis)
abline(mylm)
abline(mylm2)


paste("Fitted Regression (with outlier", 
       bty='n', 
       cex=0.8, 
       y.intersp=1.3) 

par(mfrow=c(1,3))
plot(mylm2,which=1:2)
plot(mylm2$residuals)


SSR <- sum( (mylm2$fit - mean(Davis2$height))^2 )

SSTO <- sum( (Davis2$height - mean(Davis2$height))^2 )

R2 <- (SSR/SSTO) %>% round(2)
R2




# Question 2 ---------------------------------------------------------------


# * Part A ----------------------------------------------------------------

View(Prestige)

par(mfrow=c(1,1))
mylm <- lm(income ~ prestige, data = Prestige)
plot(income ~ prestige, data = Prestige)
abline(mylm)

SSR <- sum( (mylm$fit - mean(Davis$height))^2 )
SSTO <- sum( (Davis$height - mean(Davis$height))^2 )
R2 <- (SSR/SSTO) 
R2 %>% round(2)


mylm$coefficients[1] %>% round(2)
mylm$coefficients[2] %>% round(2)


# residual standard error 
summary(mylm)


par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)

  


 
# Question 3 --------------------------------------------------------------
View(Burt)
?Burt

# * Part A ------------------------------------------------------------------
plot(IQbio ~ IQfoster, data = Burt) 
mylm <- lm(IQbio ~ IQfoster, data = Burt) 
abline(mylm)
 
# * Part B ------------------------------------------------------------------
par(mfrow=c(1,))
plot(mylm,which=1:2)
plot(mylm$residuals)


# Question 4 ------------------------------------------------------------------


# * Part A ----------------------------------------------------------------
mylm <- lm(mpg ~ disp, mtcars)
summary(mylm)

# plot(mylm)
plot(mpg ~ disp, mtcars)
abline(mylm)

SSR <- sum( (mylm$fit - mean(mtcars$mpg))^2 )
SSTO <- sum( (mtcars$mpg - mean(mtcars$mpg))^2 )
R2 <- (SSR/SSTO) 
R2 %>% round(2)

par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)


# Question 5 -------------------------------------------------------------


mylm <- lm(circumference ~ age, data = Orange)
summary(mylm)

plot(circumference ~ age, data = Orange)
abline(mylm)


set.seed(15)
N <- 300
X <- runif(N, 5, 50)
Y <- 25 + 3.5*X + rnorm(N, 0, 20)

Yd <- Y^2         #sqrt(Y) Lam =  0.5

plot(Yd ~ X, main=expression(paste("Use ", lambda == 0.5)), ylab="Y in Original Units", pch=16, col="gray45", cex=0.9, yaxt='n', xaxt='n', xlab="X in Original Units")
b <- coef(lm(sqrt(Yd) ~ X))
curve((b[1] + b[2]*x)^2, add=TRUE, col="green", lwd=2)



Xd <- X^2         #sqrt(X) Lam =  0.5

# par(mfrow=c(2,3), mai=c(.4,.4,.3,.2), mgp=c(0.5,0.5,0))

plot(Y ~ Xd, main=expression(paste("Use ", X*minute == sqrt(X))), ylab="Y in Original Units", pch=16, col="gray45", cex=0.9, yaxt='n', xaxt='n', xlab="X in Original Units")
b <- coef(lm(Y ~ sqrt(Xd)))
curve(b[1] + b[2]*sqrt(x), add=TRUE, col="green", lwd=2)

boxCox(lm(Yd ~ X))
mtext(side=3, text=expression(paste("Use ", lambda == 0.5)), line=.5)

boxCox(mylm)

plot(sqrt(circumference) ~ age, data = Orange)
mylm.t <- lm(sqrt(circumference) ~ age, data = Orange)
summary(mylm.t)
abline(mylm.t)
plot(circumference ~ age, data = Orange)




