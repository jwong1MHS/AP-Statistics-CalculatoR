out <- as.data.frame(mvrnorm(10, mu = c(0,0), 
                             Sigma = matrix(c(1,-0.26,-0.26,1),, ncol = 2), 
                             empirical = TRUE))

x <- (out$V1 - min(out$V1))*1000+10
y <- (out$V2 - min(out$V2))*200+30
data <- data.frame(x, y)

cor(x, y)
ggplot(data, aes(x=x, y=y)) + geom_point() + stat_smooth(method=lm)

x <- -x
y <- y + 12

cor(x, y)
ggplot(data, aes(x=x, y=y)) + geom_point() + stat_smooth(method=lm)
