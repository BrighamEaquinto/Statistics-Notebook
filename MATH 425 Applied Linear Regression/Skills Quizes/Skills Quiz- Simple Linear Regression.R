
# Problem 1 ---------------------------------------------------------------

View(airquality)

mylm <- lm(Wind ~ Temp, airquality)
summary(mylm)

ggplot(airquality, aes(Wind))+
  geom_histogram()

# > Part (a) The Regression ----
# What is the y intercept?
B_0 <- mylm$coefficients[1] %>% round(3)
B_1 <- mylm$coefficients[2] %>% round(3) 

# > Part (d) Plotting the Regression ----
# Ggplot
ggplot(airquality, aes(Temp, Wind))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = FALSE)+ 
  geom_vline(xintercept = 72, linetype = "longdash")+
  geom_hline(yintercept = 10.96, linetype = "longdash")+
  geom_point(x = 72, y = 10.96, size = 5, color = "firebrick")+
  theme_bw()
#BaseR
plot(Temp ~ Wind, data = airquality)
abline(mylm) #I'm having trouble with the getting the abline to appear
abline(mylm,lty=1,lwd=1,col="blue")

# > Part (e) Predict ----
predict(mylm, data.frame(Temp = 72)) %>% round(2)

# > Part (f) 
# Select the correct interpretation of the slope of your model
paste("An increase of 1 degree F in the daily maximum Temperature results in a 0.17 mph decrease in the average daily average Wind speed.")
# Select the correct interpretation of the intercept of your model.
paste("When the daily maximum Temperature is 0 degrees F, the average daily average Wind speed is estimated to be 23.234 mph.") 

# Is the slope meaningful for this data?
paste("This is where the p-value comes in, I think. This one is very close to zero, so yes the slope is significant. I didn't check any of the assumptions though so that may change this.")

# Is the intercept meaningful for this data?
paste("The answer is that it's meaningful, but I don't know why!")



# Problem 2 ---------------------------------------------------------------

View(mtcars)

mylm <- lm(mpg ~ wt, mtcars)
summary(mylm)

B_0 <- mylm$coefficients[1] %>% round(3)
B_1 <- mylm$coefficients[2] %>% round(3) 

# > Part (d) Graph ----
ggplot(mtcars, aes(wt, mpg))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = FALSE)+ 
  # geom_vline(xintercept = 72, linetype = "longdash")+
  # geom_hline(yintercept = 10.96, linetype = "longdash")+
  # geom_point(x = 72, y = 10.96, size = 5, color = "firebrick")+
  theme_bw()

# > Part (e) Predict ----
# We are predicting y after given an x value.
# wt is in thousands in this dataset
predict(mylm, data.frame(wt = 3)) %>% round(2)

# WHy is the slope not meaningful???



# Problem 3 ---------------------------------------------------------------

par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)

# Problem 4 ---------------------------------------------------------------

par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)

