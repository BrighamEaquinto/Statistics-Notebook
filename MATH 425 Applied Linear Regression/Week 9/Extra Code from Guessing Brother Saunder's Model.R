# Extra code from guessing brother Suander's model






```{r}

# take out least significant first


pairs(rbdata)
pairs(rbdata, col = interaction(x4, x5))

plot(y~x10, data = rbdata, col = interaction(x4, x5, x9))



mylm <- lm(y ~ x10 * . * sqrt(.) , data = rbdata) # x10 * all other columns as interactions
summary(mylm)


mylm <- lm(y~x10 + x4 + x5 + x9 + x10:x4 + x10:x5 + x10:x9, data = rbdata)
summary(mylm)

mylm <- lm(y ~ I(x10^3) * ., data = rbdata)
summary(mylm)

# most significant
mylm <- lm(y~x10 + x4 + x5 + x9 + x10:x4 + x10:x5 + I(x10^3):x5 + I(x10^3):x9, data = rbdata)
summary(mylm)


mylm <- lm(y ~ I(x10^2) * ., data = rbdata)
summary(mylm)




mylm <- lm(y~x10 + x4 
           + x5 + x9 + x10:x5 + I(x10^2):x4 + I(x10^2):x5 + I(x10^3):x9, data = rbdata)
summary(mylm)

b <- coef(mylm)
plot(y ~ x10, data = rbdata)
x4 = 0
x5 = 0
x9 = 0
curve(b[1] + b[2]*x, add = TRUE) 
x4 = 1
curve(b[1] + b[2]*x + 
        b[3]*x4 + b[7]*x4*x^2, add = TRUE)

# plot(y~x10, data = rbdata, col = as.factor(x10))




```

```{r} 
rbdata %>% count(x4, x5, x9)


curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5])


```

```{r}
mylm <- lm(y ~ x10^2, data = rbdata)
summary(mylm)

```


```{r}
mylm <- lm(y~x10 + x5 + x9 + x10:x4 + x10:x5 + I(x10^2):x4 + I(x10^3):x4 + I(x10^3):x5 + I(x10^3):x9, data = rbdata)
summary(mylm)

b <- coef(mylm)
interactions <- interaction(as.factor(rbdata$x4), 
                            as.factor(rbdata$x5), 
                            as.factor(rbdata$x9)
)

plot(y ~ x10, data = rbdata, col = interactions)


# curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5]*x*x4 + b[6]*x*x5 + b[7]*I(x^2)*x4 + b[8]*I(x^3)*x4 + b[9]*I(x^3)*x5 + b[10]*I(x^3)*x9, add = TRUE)


x4 = 0 # 0 or 1
x5 = 0 # 0 or 1
x9 = 0 # 0, 1, 2, or 3

curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5]*x*x4 + b[6]*x*x5 + b[7]*I(x^2)*x4 + b[8]*I(x^3)*x4 + b[9]*I(x^3)*x5 + b[10]*I(x^3)*x9, add = TRUE)

x4 = 0 
x5 = 1 
x9 = 0 

curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5]*x*x4 + b[6]*x*x5 + b[7]*I(x^2)*x4 + b[8]*I(x^3)*x4 + b[9]*I(x^3)*x5 + b[10]*I(x^3)*x9, add = TRUE)

x4 = 0 
x5 = 1
x9 = 1 

curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5]*x*x4 + b[6]*x*x5 + b[7]*I(x^2)*x4 + b[8]*I(x^3)*x4 + b[9]*I(x^3)*x5 + b[10]*I(x^3)*x9, add = TRUE)

------------------
  x4 = 0 
x5 = 1 
x9 = 2 

curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5]*x*x4 + b[6]*x*x5 + b[7]*I(x^2)*x4 + b[8]*I(x^3)*x4 + b[9]*I(x^3)*x5 + b[10]*I(x^3)*x9, add = TRUE)
------------------
  
  x4 = 0 
x5 = 1 
x9 = 3 

curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5]*x*x4 + b[6]*x*x5 + b[7]*I(x^2)*x4 + b[8]*I(x^3)*x4 + b[9]*I(x^3)*x5 + b[10]*I(x^3)*x9, add = TRUE)

x4 = 1 
x5 = 0 
x9 = 0 

curve(b[1] + b[2]*x + b[3]*x5 + b[4]*x9 + b[5]*x*x4 + b[6]*x*x5 + b[7]*I(x^2)*x4 + b[8]*I(x^3)*x4 + b[9]*I(x^3)*x5 + b[10]*I(x^3)*x9, add = TRUE)


```


