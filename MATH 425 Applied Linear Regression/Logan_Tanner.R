# Logan's Data ------------------------------------------------------------

logan_data <- read.csv("C:/Users/brigh/OneDrive - BYU-Idaho/8th Semester- Fall 2021/Math 425 Applied Linear Regression (Statistics-Notebook)/MATH 425 Applied Linear Regression/Week 9/data_logan.csv")
# View(logan_data)

pairs(logan_data)
# , panel = panel(smooth)

logan_lm <- lm(y ~ 1/((x2)^2), data = logan_data)
summary(logan_lm)

      

# Tanner's Data -----------------------------------------------------------

tanner_data <- read.csv("C:/Users/brigh/OneDrive - BYU-Idaho/8th Semester- Fall 2021/Math 425 Applied Linear Regression (Statistics-Notebook)/MATH 425 Applied Linear Regression/Week 9/data_tanner.csv")
# View(tanner_data)

pairs(tanner_data %>% select(y, x1, x2, x3, x4, x5))

#testing
tanner_lm <- lm(y ~ I(x2)^3 * . , data = tanner_data)
summary(tanner_lm)


#best current results
tanner_lm <- lm(y ~ x2 + I(x2^2):x2 + I(x2^2):x4, data = tanner_data)
summary(tanner_lm)

b <- coef(tanner_lm)
plot(y ~ x2, data = tanner_data)

x2=0
x4=0
curve(b[1] + b[2]*x + b[3]*(x2^2)*x2 + b[4]*(x2^2)*x4, add = TRUE)

x2=1
x4=0
curve(b[1] + b[2]*x + b[3]*(x2^2)*x2 + b[4]*(x2^2)*x4, add = TRUE)

x2=0
x4=1
curve(b[1] + b[2]*x + b[3]*(x2^2)*x2 + b[4]*(x2^2)*x4, add = TRUE)

x2=1
x4=1
curve(b[1] + b[2]*x + b[3]*(x2^2)*x2 + b[4]*(x2^2)*x4, add = TRUE)
