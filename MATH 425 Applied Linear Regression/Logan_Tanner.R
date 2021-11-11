logan_data <- read.csv("C:/Users/brigh/OneDrive - BYU-Idaho/8th Semester- Fall 2021/Math 425 Applied Linear Regression (Statistics-Notebook)/MATH 425 Applied Linear Regression/Week 8 Model Selection & Regression Battleship/data_logan.csv")
View(logan_data)

logan_lm <- lm(y ~ x2, data = logan_data)
summary(logan_lm)


pairs(logan_data, panel = panel(smooth)

      
      
      
      
tanner_data <- read.csv("C:/Users/brigh/OneDrive - BYU-Idaho/8th Semester- Fall 2021/Math 425 Applied Linear Regression (Statistics-Notebook)/MATH 425 Applied Linear Regression/Week 8 Model Selection & Regression Battleship/data_tanner.csv")
View(tanner_data)      
pairs(tanner_data)
tanner_lm <- lm(y ~ x2, data = tanner_data)
summary(tanner_lm)

b <- coef(tanner_lm)
plot(y ~ x2, data = tanner_data, col = as.factor(x4))
x4=0
curve(b[1] + b[2]*x4, add = TRUE)
curve(b[1] + b[2]*x + 
      b[3] + b[4]*x, add = TRUE)
