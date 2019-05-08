#Installing packages
install.packages('ggplot2')
library('ggplot2')

#Setting up data table
data = read.csv("Highway1.csv", head = TRUE, sep = ",")
rate = as.numeric(data[,2])
signs = as.numeric(data[,6])

#Part 1a

#Part 1b
cor(signs, rate)

#Part 2a
model = lm(rate ~ signs)
summary(model)

#Part 2c
confint(model, 'signs', level=0.95)

#Part 3a
ggplot(data, aes(x=signs, y=rate)) + geom_point() + stat_smooth(method=lm)

#Part 3b
plot(signs, resid(model), xlab = "Signs", ylab = "Rate Residuals")
abline(0,0)

#Part 3c
qqnorm(resid(model), xlab = "Signs", ylab = "Standardized Residuals")
qqline(resid(model))

#Part 4a
predict(model, data.frame(signs=2), interval="prediction", level=0.95)

#Part 4b
predict(model, data.frame(signs=2), interval="prediction", level=0.99)
