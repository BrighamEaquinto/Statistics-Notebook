log(5000)
exp(8.5171932)

log10(5000)
exp(1)

library(tidyverse)
ggplot(islands, aes(x = log(islands)))+
  geom_histogram()

exp(4) %>% round(3)
log(10000) %>% round(3)



library(mosaicData)
View(Utilities)
?Utilities

mylm <- lm(gasbill ~ temp, data = Utilities)
summary(mylm)
plot(gasbill ~ temp, data = Utilities)
abline(mylm) 
# this is probably what the car data will be like! Get from brand new cars to 200000+ miles in the data

plot(mylm, which = 1)
plot(mylm, which = 2)
plot(mylm, which = 3)

# linearity first, variance second in matters to address before anything else

plot(log(gasbill) ~ temp, data = Utilities)
mylm.log <- lm(log(gasbill) ~ temp, data = Utilities)
plot(mylm.log, which = 1)
plot(mylm.log, which = 2)
plot(mylm.log, which = 3)

# look at each plot and decide which is beneficial for what reason. Outliers

plot(mylm.log)
b <- coef(mylm.log)
b


plot(gasbill ~ temp, data = Utilities)
curve(exp(b[1] +b[2]*x), add = TRUE)


ggplot(Utilities, aes(x = temp, y = gasbill))+
  geom_point()+
  stat_function(fun = function(x) expb[1] + b[2]) 


abline(b[1], b[2])
summary(mylm.log)
abline(b[1]+ 0.3668, b[2], lty = 2)
abline(mylm)
abline(mylm.log)

