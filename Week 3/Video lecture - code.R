### Script of the commands from the video lecture ###
# Here is notes for the video lecture
# https://www.notion.so/fjellestad/L3-Simple-regression-9f07794a7b354c238ff86070f144c290?pvs=4 

#----- Effect of class size on teaching outcome -----
require(dplyr)

# load data after setting your working directory to the folder with the file [use setwd()]
load("School.Rdata")

# inspect data
# summary(School) # use this on your machine
str(School)

# estimate beta.1
y_bar <- mean(School$testscr)
x_bar <- mean(School$str)

beta_hat_1 <- sum((School$str - x_bar)*(School$testscr - y_bar)) / sum((School$str - x_bar)^2)

# estimate beta.2
beta_hat_0 <- mean(School$testscr) - beta_hat_1 * x_bar

# display
beta_hat_0
beta_hat_1

# estimate regression 
fit <- lm(testscr ~ str, data = School)

# display result
summary(fit)

# Examination of variation of the variables
summary(School$str)
sd(School$str)
summary(School$testscr)
sd(School$testscr)

# predicted value for str = 16
y_x16 <- fit$coefficients[1] + fit$coefficients[2]*16
# predicted value for str = 19
y_x19 <- fit$coefficients[1] + fit$coefficients[2]*19
# difference
as.numeric(y_x19 - y_x16)

# predicted value for str = 20
y_x20 <- fit$coefficients[1] + fit$coefficients[2]*20
# predicted value for str = 23
y_x23 <- fit$coefficients[1] + fit$coefficients[2]*23
# difference
as.numeric(y_x23 - y_x20)

# Simple way
fit$coefficients[2]*3

# Is this a big effect?
# std x variables
sd(School$str)
# effect on y if x changes by one std
sd(School$str)*fit$coefficients[2]
# std y var
sd(School$testscr)
# effect on y if x changes by one std expressed 
# in std of the dependent var
sd(School$str)*fit$coefficients[2] / sd(School$testscr)

### Binary regressor
# define small class size
School <- mutate(School, small = str < 20)
# School$small <- School$str < 20 # alternative code

# estimate regression
fit.bin <- lm(testscr ~ small, data = School)

# display result
fit.bin$coefficients


### Goodness of fit
# extract from lm output
summary(fit)$r.squared

# compute yourself
var(fit$fitted.values) / var(fit$residuals + fit$fitted.values)


