#binompdf
dbinom(4, size=12, prob=0.2)

#binomcdf
pbinom(4, size=12, prob=0.2)

lb = 1
ub = 4

plot(x, hx, type="n", xlab="IQ Values", ylab="",
     main="Binomial Distribution", axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 

area <- pbinom(4, size=12, prob=0.2) - 1
result <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(mean-3*sd, mean+3*sd, sd), pos=0)