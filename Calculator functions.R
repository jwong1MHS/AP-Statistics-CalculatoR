data = read.csv("Highway1.csv", head = TRUE, sep = ",")
rate = as.numeric(data[,2])
signs = as.numeric(data[,6])


#1 Var Stats
#3nput list
#1,2,3.4.5
#5 num summary, mean, stddev, sum(x), sum(x^2), pop stdddev
#1, 1.5, 3, 4.5, 5, 3, 1.58, 15, 55, 1.41
x <- c(1, 2, 3, 4, 5)
summary <- summary(x, verbose = show.info)


#2 Var Stats
#1, 2, 3, 4, 5 and 6, 7, 8, 9, 10
#min1, min2, max1, max2, mean1, mean2, stddev1, stddev2, sum(x)1, sum(x)2, sum(x^2)1, sum(x^2)2, popstddev(x)1, popstd(y)2, sum(xy)
#1, 6, 5, 10, 3, 8, 1.58, 1.58, 15, 40, 55, 330, 1.41, 1.41, 130


#LinReg
#input values, and generates a linear model, gives you a coefficieent, b coefficent, r, and r^2
cor(signs, rate)
model = lm(rate ~ signs)
summary(model)


#invNorm - z-value
#area, mu, sigma
qnorm(0.8, mean = 15, sd = 4)


#Normalcdf - area
#lower, upper, mu, sigma, outputs probability
#0, 15, 8, 6, output 0.787
pnorm(15, mean = 8, sd = 6) - pnorm(0, mean = 8, sd = 6)


#invT - t-value
#area, df
qt(0.8, df = 13)


#Tcdf
#lower, upper, df
pt(15, df = 7) - pt(0, df = 7)


#Binompdf
#x-value, n, p
x <- 14
n <- 20
p <- 0.3
pbinom(x, n, p) - pbinom(x - 1, n, p)


#Binomcdf
#x-value, n, p
pbinom(3, 8, 0.6)


#Geometpdf
#p, x
x <- 14
p <- 0.3
pgeom(x - 1, p) - pgeom(x - 2, p)


#Geometcdf
pgeom(x - 1, p)


#T-test
# m: the sample mean
# s: the sample standard deviation
# n: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m,s,n)
{
  se <- sqrt(s^2/n)
  df <- n - 1

  t <- m1/se 
  dat <- c(m1, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Std Error", "t", "p-value")
  return(dat) 
}
x1 = rnorm(100)
x2 = rnorm(200) 
# you'll find this output agrees with that of t.test when you input x1,x2
t.test2( mean(x1), mean(x2), sd(x1), sd(x2), 100, 200)


#2-SampTTest
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}
x1 = rnorm(100)
x2 = rnorm(200) 
# you'll find this output agrees with that of t.test when you input x1,x2
t.test2( mean(x1), mean(x2), sd(x1), sd(x2), 100, 200)


#1-PropZtest
#p0, x, n, inequality sign
#0.7, 59, 100, two-sided
prop.test(x = 59, n = 100, p = 0.7)
prop.test(x = 59, n = 100, p = 0.7)$p.value
z.test(59/100, y = NULL, alternative = "two.sided", mu = 0.7, sigma.x = sqrt(0.7*(1-0.7)),
       sigma.y = NULL, conf.level = 0.95)


#2-PropZtest
prop.test(x = c(490, 400), n = c(500, 500))
prop.test(x = c(490, 400), n = c(500, 500))$p.value


#ZInterval
prop.test(x = 59, n = 100, p = 0.7)$conf.int


#Tinterval


#2-SampTint



#2-PropZint
prop.test(x = c(490, 400), n = c(500, 500))$conf.int


#Chi squared test
chisq.test


#Chi squared GOF test
obs <- c(81, 50, 27)
exp <- c(1/2, 1/3, 1/6)
res <- chisq.test(obs, p = exp)
chisq.test(obs, p = exp)$expected


#LinReg T test
model = lm(rate ~ signs)
summary(model)


#LinReg T Int
predict(model, data.frame(signs=2), interval = "confidence", level = 0.95)
predict(model, data.frame(signs=2), interval = "prediction", level = 0.95)