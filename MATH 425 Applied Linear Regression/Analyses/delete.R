#https://byui.instructure.com/courses/152366/quizzes/2408371/take

library(mosaic)

View(Utilities)

mylm <- lm(elecbill ~ kwh, data = Utilities)
summary(mylm)


ggplot(Utilities, aes(kwh, elecbill))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = FALSE)+
  theme_bw()

#Line is average y value for any given x value


colnames(Utilities)
mylm <- lm(data = utilities)
predict(mylm, data.frame())




library(car)
View(Davis)
?Davis

dat <- Davis %>% 
  filter(sex == "M")
mylm <- lm(weight ~ height, data = dat)
predict(mylm, data.frame(height = 180))


View(USArrests)
?USArrests

mylm <- lm(Murder~Assault, data = USArrests)
summary(mylm)


ggplot(USArrests, aes(Assault, Murder))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = FALSE)+ 
  # geom_vline(xintercept = 72, linetype = "longdash")+
  # geom_hline(yintercept = 10.96, linetype = "longdash")+
  # geom_point(x = 72, y = 10.96, size = 5, color = "firebrick")+
  theme_bw()
