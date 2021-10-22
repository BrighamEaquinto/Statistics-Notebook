# 1
mylm <- lm(Wind ~ Solar.R , data = airquality)
summary(mylm)

MSE<- 3.515^2

SSE <- sum( mylm$res^2 )


MSE <- sum( mylm$res^2 ) / (n - 2)
MSE


# 2
library(mosaic)
library(car)
?RailTrail
View(RailTrail)
mylm <- lm(volume ~ lowtemp , data = RailTrail)
summary(mylm)

confint(mylm, level = 0.90)

predict(mylm, data.frame(lowtemp=38), interval="confidence")
predict(mylm, data.frame(lowtemp=22), interval="confidence")
predict(mylm, data.frame(lowtemp=62), interval="confidence")
predict(mylm, data.frame(lowtemp=45), interval="confidence")

predict(mylm, data.frame(lowtemp=38), interval="prediction")
predict(mylm, data.frame(lowtemp=22), interval="prediction")
predict(mylm, data.frame(lowtemp=62), interval="prediction")
predict(mylm, data.frame(lowtemp=45), interval="prediction")


# 3
?airquality

#Original
mylm <- lm(Ozone ~ Temp, data=airquality)
summary(mylm) # 0.4877

boxCox(mylm)


mylm <- lm(Ozone^-1 ~ Temp, data=airquality)
summary(mylm) #0.1682

mylm <- lm(Ozone ~ Temp, data=airquality)
summary(mylm) # 0.4877

mylm <- lm(sqrt(Ozone) ~ Temp, data=airquality)
summary(mylm) # 0.5527

mylm <- lm(log(Ozone) ~ Temp, data=airquality)
summary(mylm) # 0.5473





plot(Ozone ~ Temp, data=airquality)
abline(mylm)

SSR <- sum((mylm$fit - mean(airquality$Ozone))^2)
SSTO <- sum((airquality$Ozone - mean(airquality$Ozone))^2)

SSR/SSTO

# 4

# 5
(-7.1)^2 + 13^2 + (-7.4)^2 + 4.1^2 + (-2.7)^2

#6
mylm <- lm(dist, speed, data = cars)
summary(mylm)



# 8
cars
mylm <- lm(dist~speed, data = cars)
mylm.sqrt <- lm(sqrt(dist)~speed, data = cars)

summary(mylm)
summary(mylm.sqrt)


# plot(dist~speed, data = cars)
# abline(sqrt(dist)~speed, data = cars)
# 
# b <- coef(lm(sqrt(dist) ~ speed))
# curve((b[1] + b[2]*x)^2, add=TRUE, col="green", lwd=2)
# 

predict(mylm, data.frame(speed=20), interval="prediction")
predict(mylm.sqrt, data.frame(speed=20), interval="prediction")

# 9
mylm <- lm(log(mpg)~wt, data = mtcars)
plot(mpg~wt, data = mtcars)
summary(mylm)
boxCox(mylm)

?mtcars
predict(mylm, data.frame(wt=4), interval="confidence")


# 10



# 11

mylm <- lm(width ~ length, data = KidsFeet)
summary(mylm)
predict(mylm, data.frame(length = 25), interval="confidence")

# 13
mylm <- lm(Temp~Wind, data = airquality)
summary(mylm)

# 14
mylm <- lm(dist~speed, cars)
summary(mylm)
rez <- abs(mylm$residuals) %>% 
  sort()
rez
cars[24,]
plot(dist~speed, cars)

residuals
#15 either a or b
389.075/374.182
-6.171/51.379
tvalue <- -109.736/374.404
9.468/51.428

pt(-abs(tvalue), 31)
pt(-abs(tvalue), 41)
pt(-abs(tvalue), 51)
pt(-abs(tvalue), 61)
pt(-abs(tvalue), 71)
pt(-abs(tvalue), 81)
pt(-abs(tvalue), 91)
pt(-abs(tvalue), 101)


# 16
mylm <- lm(weight ~ Time , ChickWeight)
summary(mylm)
?ChickWeight
par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)

 
# 20
mylm <- lm(height~age, Loblolly)
plot(height~age, Loblolly)
abline(mylm)
par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)
summary(mylm)

dat <- Loblolly %>% 
  group_by(age) %>% 
  summarise(mean= mean(height))
dat
plot(mean~age,dat)
mylm <- lm(mean~age,dat)
summary(mylm)


# 21
mylm <- lm(circumference ~ age , Orange)
summary(mylm)

par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)

# 22
?mpg

View(mpg)

plot(hwy ~ cty, data = mpg)

mpg.lm <- lm(hwy ~ cty, data=mpg) 
summary(mpg.lm)

# 23
mylm <- lm(sqrt(dist)~speed, cars)
mylm <- lm(sqrt(dist)~speed, cars)

summary(mylm)
boxCox(mylm)

plot(dist~speed, cars)
abline(mylm)




mylm <- lm(dist~speed, cars)
summary(mylm)

mylm.sqrt <- lm(sqrt(dist)~speed, cars)
summary(mylm.sqrt)

#coef just grabs things from the lm summary

b <- coef(mylm)
b.sqrt <- coef(mylm.sqrt)

ggplot(cars, aes(speed, dist))+
  geom_point()+
  stat_function(fun = function(x) b[1] + b[2]* x, color = "red")+
  stat_function(fun = function(x) (b.sqrt[1] + b.sqrt[2]* x)^2, color = "blue")



# Unknown questions <- c(2, 6, 7, 15, 18)

