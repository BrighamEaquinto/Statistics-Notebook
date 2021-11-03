# Part 1 Weighted Regressions 
# We're going to be learning about giving different weights to variables

X <- c(1, 4,  7,   8,  10, 20)

Y <- c(3, 5, 18, 13, 12,   1)

w <- c(1, .5, .2, 1, 1, .1)
w <- c(1, 1, 0, 0, 0, 0, 0)
w <- c(0, 0, 1, 0, 0, 1)
w <- c(1, 1, 0.5, 1, 1, 0.2)

mylm <- lm(Y ~ X, weights=w)
summary(mylm)

plot(Y ~ X, pch=21, bg=rgb(1-w,1-w,1-w), col="orange")

abline(mylm)


# Systematic way of trusting data: Lowess Curve
# Lowess: Local weighted scatterplot smoothing
# Break the dataset into regions, run regressions on those 
# You can control the number of nebiors in lowess
# baseR's and Gplot's lowess curves are different


library(mosaic)
library(tidyverse)

plot(gasbill ~ month, Utilities, pch = 16, col = "orange")
lines(lowess(Utilities$month,Utilities$gasbill), col = "red")

quad.lm <- lm(gasbill ~ month + I(month^2), Utilities)
summary(quad.lm)
b <- coef(quad.lm)
curve(b[1] + b[2] * x + b[3] * x^2, add = TRUE, col = "skyblue")


ggplot(Utilities, aes(month, gasbill))+
  geom_point()+
  geom_smooth(se = FALSE, color = "red")+
  stat_function(fun = function(x) b[1] + b[2]*X + b[3]*x ^2, color = "skyblue")


# in those other doc, only x values are sued ad neghibors 
# looks like this can be overfitted in an extreme case 

# transformations, lowess, 

