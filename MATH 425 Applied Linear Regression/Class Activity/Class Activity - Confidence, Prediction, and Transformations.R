#Class Activity - Confidence, Prediction, and Transformations
# https://byui.instructure.com/courses/152366/quizzes/2408281/take

library(tidyverse)
library(mosaic)
library(car)

mylm <- lm(gasbill~temp, data = Utilities)
summary(mylm)
hist(mylm$residuals)
plot(gasbill~temp, data = Utilities)
abline(mylm)

boxCox(mylm, lambda = seq(0,1,.1))
mylmt <- lm(sqrt(sqrt(gasbill)) ~ temp, data = Utilities)

b <- coef(mylm)
bt <- coef(mylmt)


mypreds <- predict(mylm, data.frame(temp = 30), interval = "prediction")
mypreds[1] #fit
mypreds[2] #lower
mypreds[3] #upper

mytpreds <- predict(mylmt, data.frame(temp = 30), interval = "prediction")^4
mytpreds[1] #fit
mytpreds[2] #lower
mytpreds[3] #upper


mypreds2 <- predict(mylm, data.frame(temp = 60), interval = "prediction")
mypreds2[1] #fit
mypreds2[2] #lower
mypreds2[3] #upper

mytpreds2 <- predict(mylmt, data.frame(temp = 60), interval = "prediction")^4
mytpreds2[1] #fit
mytpreds2[2] #lower
mytpreds2[3] #upper


ggplot(Utilities, aes(temp, gasbill))+
  geom_point()+
  stat_function(fun = function(x) b[1] + b[2]*x, color = "hotpink")+
  stat_function(fun = function(x) (bt[1] + bt[2]*x)^4, color = "skyblue")+
  geom_segment(aes(x = 30, xend = 30,
                   y = mytpreds[2], yend = mytpreds[3]), 
               size = 4, color = "skyblue", alpha = 0.01)+
  geom_segment(aes(x = 30, xend = 30,
                   y = mypreds[2], yend = mypreds[3]), 
               size = 4, color = "hotpink")+
  geom_segment(aes(x = 60, xend = 60,
                   y = mypreds2[2], yend = mypreds2[3]), 
               size = 4, color = "hotpink", alpha = 0.01)+
  geom_segment(aes(x = 60, xend = 60,
                   y = mytpreds2[2], yend = mytpreds2[3]), 
               size = 4, color = "skyblue")



plot(mylm, which = 1)
plot(gasbill~temp, data = Utilities)


