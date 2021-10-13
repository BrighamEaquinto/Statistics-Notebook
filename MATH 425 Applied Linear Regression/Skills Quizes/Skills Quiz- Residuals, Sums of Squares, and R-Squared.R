library(tidyverse)
# install.packages("gridExtra")
library(gridExtra)
View(Orange) 

# Problem 2 ---------------------------------------------------------------


lm.orange <- lm(circumference~age, data = Orange)
summary(lm.orange)

ggplot(Orange, aes(age, circumference))+
  geom_point(color = "red")+
  geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "brown")+
  theme_bw()

B_0 <- lm.orange$coefficients[1] %>% round(2)
B_1 <- lm.orange$coefficients[2] %>% round(2) 

SSE <- sum(lm.orange$res^2) %>% round(2)
SSR <- sum((lm.orange$fit - mean(Orange$circumference))^2)
SSTO <- sum((Orange$circumference - mean(Orange$circumference))^2)
R2 <- SSR / SSTO 
R2%>% round(2)
Correlation <- cor(Orange$circumference, Orange$age)
Correlation %>% round(2)

predict(lm.orange, data.frame(age = 365*3)) %>% round(2)


# Problem 3 ---------------------------------------------------------------
colnames(mtcars)


# wt
lm.wt <- lm(mpg~wt, data = mtcars) 
summary(lm.wt)
ggplot(mtcars, aes(wt, mpg))+
  geom_point(color = "red")+
  geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "brown")+
  theme_bw()
SSE <- sum(lm.wt$res^2) %>% round(2)
SSR <- sum((lm.wt$fit - mean(mtcars$mpg))^2) %>% round(2)
SSTO <- sum((mtcars$mpg - mean(mtcars$mpg))^2) %>% round(2)
R2 <- SSR / SSTO 
R2%>% round(2) 
Correlation <- cor(lm.wt$wt, lm.wt$mpg)
Correlation %>% round(2)

par(mfrow=c(1,3))
plot(lm.wt,which=1:2)
plot(lm.wt$residuals)


# cyl
lm.cyl <- lm(mpg~cyl, data = mtcars) 
summary(lm.cyl)
ggplot(mtcars, aes(cyl, mpg))+
  geom_point(color = "red")+
  geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "brown")+
  theme_bw()
SSE <- sum(lm.cyl$res^2) %>% round(2)
SSR <- sum((lm.cyl$fit - mean(mtcars$mpg))^2) %>% round(2)
SSTO <- sum((mtcars$mpg - mean(mtcars$mpg))^2)
R2 <- SSR / SSTO 
R2%>% round(2)
Correlation <- cor(Orange$circumference, Orange$age)
Correlation %>% round(2)

par(mfrow=c(1,3))
plot(lm.cyl,which=1:2)
plot(lm.cyl$residuals)


# hp
lm.hp <- lm(mpg~hp, data = mtcars) 
summary(lm.hp)
ggplot(mtcars, aes(hp, mpg))+
  geom_point(color = "red")+
  geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "brown")+
  theme_bw()
SSE <- sum(lm.hp$res^2) %>% round(2)
SSR <- sum((lm.hp$fit - mean(mtcars$mpg))^2) %>% round(2)
SSTO <- sum((mtcars$mpg - mean(mtcars$mpg))^2)
R2 <- SSR / SSTO 
R2%>% round(2)
Correlation <- cor(Orange$circumference, Orange$age)
Correlation %>% round(2)

par(mfrow=c(1,3))
plot(lm.hp,which=1:2)
plot(lm.hp$residuals)

ggplot(mtcars, aes(hp))+
  geom_histogram(bins = 10)
