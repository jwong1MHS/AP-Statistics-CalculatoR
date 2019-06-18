data = read.csv("Highway1.csv", head = TRUE, sep = ",")
rate = as.numeric(data[,2])
signs = as.numeric(data[,6])


#1 Var Stats
#3nput list
#1,2,3.4.5
#5 num summary, mean, stddev, sum(x), sum(x^2), pop stdddev
#1, 1.5, 3, 4.5, 5, 3, 1.58, 15, 55, 1.41
L1 <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
n <- length(L1)
result <- c(mean(L1), sum(L1), sum(L1^2), sd(L1), sqrt((n-1)/n) * sd(L1), n, quantile(L1))    
names(result) <- c("x-bar", "Sum of x", "Sum of x^2", "Sample SE of x", "Pop SE of x", "n", "minX", "Q1", "Med", "Q3", "maxX")
print(result)


#2 Var Stats
#1, 2, 3, 4, 5 and 6, 7, 8, 9, 10
#min1, min2, max1, max2, mean1, mean2, stddev1, stddev2, sum(x)1, sum(x)2, sum(x^2)1, sum(x^2)2, popstddev(x)1, popstd(y)2, sum(xy)
#1, 6, 5, 10, 3, 8, 1.58, 1.58, 15, 40, 55, 330, 1.41, 1.41, 130
L1 <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
L2 <- c(52, 36, 71, 21, 57, 79, 63, 55, 31, 70)
n <- length(L1 & L2)
result <- c(mean(L1), sum(L1), sum(L1^2), sd(L1), sqrt((n-1)/n) * sd(L1), n, mean(L2), sum(L2), sum(L2^2), sd(L2), 
         sqrt((n-1)/n) * sd(L2), sum(L1 * L2), min(L1), max(L1), min(L2), max(L2))
names(result) <- c("x-bar", "Sum of x", "Sum of x^2", "Sample SE of x", "Pop SE of x", "n", "y-bar", "Sum of y", "Sum of y^2", 
                   "Sample SE of y", "Pop SE of y", "Sum of xy", "minX", "maxX", "minY", "maxY")
print(result)


#LinReg
#input values, and generates a linear model, gives you a coefficieent, b coefficent, r, and r^2
explanatory <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
response <- c(52, 36, 71, 21, 57, 79, 63, 55, 31, 70)
model <- lm(response ~ explanatory)
a <- summary(model)$coefficients["(Intercept)", "Estimate"]
b <- summary(model)$coefficients["explanatory", "Estimate"]
cat(paste0("y = a + bx\na = ", a, "\nb = ", b))


#invNorm - z-value
#area, mu, sigma
area <- 0.84
mean <- 15
sd <- 4
qnorm(area, mean, sd)


#Normalcdf - area
#lower, upper, mu, sigma, outputs probability
#0, 15, 8, 6, output 0.787
lower <- 0
upper <- 15
mean <- 8
sd <- 6
pnorm(upper, mean, sd) - pnorm(lower, mean, sd)


#invT - t-value
#area, df
area <- 0.8
df <- 13
qt(area, df)


#Tcdf
#lower, upper, df
lower <- 0
upper <- 15
df <- 7
pt(upper, df) - pt(lower, df)


#Binompdf
#n, p, x trials
trials <- 20
p <- 0.3
x_value <- 14
pbinom(x_value, trials, p) - pbinom(x_value - 1, trials, p)


#Binomcdf
#n, p, x trials
trials <- 20
p <- 0.3
x_value <- 14
pbinom(x_value, trials, p)


#Geometpdf
#p, x
p <- 0.3
x <- 14
pgeom(x - 1, p) - pgeom(x - 2, p)


#Geometcdf
p <- 0.3
x <- 14
pgeom(x - 1, p)

#--------------------------------------------------------DO NOT ENTER----------------------------------------------------------------

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
  if(equal.variance == FALSE) 
  {
    se <- sqrt(s1^2/n1 + s2^2/n2)
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
t.test2(mean(x1), mean(x2), sd(x1), sd(x2), 100, 200)


#1-PropZtest
#p0, x, n, inequality sign
#0.7, 59, 100, two-sided
prop.test(x = 59, n = 100, p = 0.7)
prop.test(x = 59, n = 100, p = 0.7)$p.value
z.test(59/100, y = NULL, alternative = "two.sided", mu = 0.7, sigma.x = sqrt(0.7*(1-0.7)),
       sigma.y = NULL, conf.level = 0.95)


#2-PropZtest
#x1, n1, x2, n2, inequality sign
prop.test(x = c(490, 400), n = c(500, 500))
prop.test(x = c(490, 400), n = c(500, 500))$p.value


#ZInterval
#sigma, x-bar, n, confidence level
prop.test(x = 59, n = 100, p = 0.7)$conf.int


#Tinterval
#x-bar, sample std error, n, confidence level


#2-SampTint
# x1, x2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# pooled: whether or not to assume equal variance. Default is FALSE. 
#inequality sign


#2-PropZint
#x1, n1, x2, n2, inequality sign
prop.test(x = c(490, 400), n = c(500, 500))$conf.int

#----------------------------------------------------------FINISHED------------------------------------------------------

#Chi squared test
obs <- c(30, 10, 15, 25, 15, 5)
newobs <- matrix(obs, nrow = 3, ncol = 2, byrow=TRUE)
exp <- c(24, 16, 24, 16, 12, 8)
newexp <- matrix(exp/sum(exp), nrow = 3, ncol = 2, byrow=TRUE)
res <- chisq.test(newobs, p = newexp)
chi_squared <- chisq.test(newobs, p = newexp)$statistic
p <- chisq.test(newobs, p = newexp)$p.value
df <- chisq.test(newobs, p = newexp)$parameter
cat(paste0("X2-Test\nX2 = ", chi_squared, "\np = ", p, "\ndf = ", df))


#Chi squared GOF test
obs <- c(4, 13, 7)
exp <- c(8, 8, 8)
exp2 <- c(exp/sum(exp))
res <- chisq.test(obs, p = exp2)
chi_squared <- chisq.test(obs, p = exp2)$statistic
p <- chisq.test(obs, p = exp2)$p.value
df <- chisq.test(obs, p = exp2)$parameter
CNTRB <- (obs - exp)^2/exp
cat(paste0("X2GOF-Test\nX2 = ", chi_squared, "\np = ", p, "\ndf = ", df, "\nCNTRB = {", paste(CNTRB, collapse = ", "), "}"))


#LinReg T test
explanatory <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
response <- c(52, 36, 71, 21, 57, 79, 63, 55, 31, 70)
model <- lm(response ~ explanatory)
df <- model$df.residual
a <- summary(model)$coefficients["(Intercept)", "Estimate"]
b <- summary(model)$coefficients["explanatory", "Estimate"]
s <- summary(model)$sigma
t <- summary(model)$coefficients[6]
p <- summary(model)$coefficients[8]
r_squared <- summary(model)$r.squared
r <- cor(response, explanatory)
cat(paste0("LinRegTTest\ny = a + bx\nβ ≠ 0\nt = ", t, "\np = ", p, "\ndf = ", df, "\na = ", a, "\nb = ", b, "\ns = ", s, "\nr^2 = ", r_squared, "\nr = ", r))


#LinReg T Int
explanatory <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
response <- c(52, 36, 71, 21, 57, 79, 63, 55, 31, 70)
model <- lm(response ~ explanatory)
df <- model$df.residual
a <- summary(model)$coefficients["(Intercept)", "Estimate"]
b <- summary(model)$coefficients["explanatory", "Estimate"]
s <- summary(model)$sigma
r_squared <- summary(model)$r.squared
r <- cor(response, explanatory)
interval <- confint(model, 'explanatory', level=0.95)
lower <- interval[1]
upper <- interval[2]
cat(paste0("LinRegTInt\ny = a + bx\n(", lower, ", ", upper, ")\nb = ", b, "\ndf = ", df, "\ns = ", s, "\na = ", a, "\nr^2 = ", r_squared, "\nr = ", r))

