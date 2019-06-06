mean <- 6
popsd <- 2.5
n <- 20
sd <- popsd/sqrt(n)
error <- qnorm(0.975)*sd
lb <- a-error
ub <- a+error

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="IQ Values", ylab="",
     main="Normal Distribution", axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(mean-3*sd, mean+3*sd, sd), pos=0)
