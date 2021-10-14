# https://byui.instructure.com/courses/152366/quizzes/2408405/take


mylm <- lm(waiting~eruption, faithful)
summary(mylm)



ggplot(data= faithful, aes(x=eruptions, y= waiting))+
  geom_point(color="skyblue", alpha= 0.5)+
  geom_smooth(method = "lm", formula=y~x, se=FALSE, size=1)


faithfullm <- lm(waiting~eruptions, data=faithful)
summary(faithfullm) 

confint(faithfullm)

predict(faithfullm, data.frame(eruptions=2), interval= "prediction")
#when we predict, we are predicting a dot. It is very difference from predicting a confidence interval.


predict(faithfullm, data.frame(eruptions=2), interval= "confidence")


ggplot(data= faithful, aes(x=eruptions, y= waiting))+
  geom_point()+
  geom_smooth(method = "lm")
#confidence is pretty accurate
#prediction is very wide