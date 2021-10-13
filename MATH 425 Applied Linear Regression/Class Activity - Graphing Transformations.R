# Base Graphic

plot(circumference ~ age, data=Orange, pch=16, col="orangered", main="Growth of Orange Trees", xlab="Age of Tree in Days", ylab="Circumference of Tree (mm)")

# ggplot Graphic
library(tidyverse)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw( )

mylm <- lm(circumference ~ age, data = Orange)
lm.log <- lm(log(circumference) ~ age, data = Orange)
lm.sqrt <- lm(sqrt(circumference) ~ age, data = Orange)
lm.1o <- lm(1/(circumference) ~ age, data = Orange)
lm.Y <- lm((circumference)^2 ~ age, data = Orange)
lm.Y2 <- lm((circumference)^2 ~ age, data = Orange)
lm.neg2 <- lm((circumference)^-2 ~ age, data = Orange)

b.log <- coef(lm.log)
b.sqrt <- coef(lm.sqrt)
b.1o <- coef(lm.1o)
b.Y <- coef(lm.Y)
b.Y2 <- coef(lm.Y2)
b.neg2 <- coef(lm.neg2)


ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)")+ 
  stat_function(fun = function(x) exp())+
  stat_function(fun = function(x) (b.sqrt[1] + b.sqrt[2]*x)^2)+
  stat_function(fun = function(x) 1/(b.1o[1] + ))+
  stat_function(fun = function(x) )+
  stat_function(fun = function(x) sqrt(b.Y2[1] + b.Y2[2]*x), color = "deeppink")+
  stat_function(fun = function(x) (b.neg2[1] + b.neg2[2]*x)^-0.5)+
  theme_bw()




