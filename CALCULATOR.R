decision <- readline(prompt = paste0("What type of processes would you like to do today? \nType \n[B] for binomials \n[G] for geometric sequences \n[S] for descriptive statistics \n[N] for normal distributions or z distributions \n[C] for Chi Squared functions \n[R] for Regressions: "))

if (decision == "B"){
  
  type <- readline(prompt="Binomial: [P]df or [C]df: ")
  
  if (type == "P"){
    
    print("binomPdf")
    
    trials <- as.numeric(readline(prompt = "How many trials?: "))
    p <- as.numeric(readline(prompt = "What is the probability of success?: "))
    x_value <- as.numeric(readline(prompt = "What is the desired x?: "))
    
    cat("Probability: ", pbinom(x_value, trials, p) - pbinom(x_value - 1, trials, p))
    
  } else {
    
    print("binomCdf")
    
    trials <- as.numeric(readline(prompt = "How many trials?: "))
    p <- as.numeric(readline(prompt = "What is the probability of success?: "))
    x_value <- as.numeric(readline(prompt = "What is the desired x?: "))
    
    cat("Probability: ", pbinom(x_value, trials, p))
    
  }
  
} else if (decision == "G"){               
  
  type <- readline(prompt="Geometric:[P]df or [C]df: ")
  
  if (type == "P"){
    
    print("geometPdf")
    
    p <- as.numeric(readline(prompt = "What is the probability of success?: "))
    x_value <- as.numeric(readline(prompt = "What is the desired x?: "))
    
    cat("Probability: ", pgeom(x - 1, p) - pgeom(x - 2, p))
    
  } else {
    
    print("geometCdf")
    
    x_value <- as.numeric(readline(prompt = "What is the desired x?: "))
    p <- as.numeric(readline(prompt = "What is the probability of success?: "))
    
    cat("Probability: ", pgeom(x - 1, p))
    
  }
  
} else if (decision == "N"){
  
  type <- readline(prompt = "[N]ormal, [Z] functions or [T] functions?: ")
  
  if (type == "N"){
    
    type <- readline(prompt = "[C]df or [I]nverse Norm?")
    
    if (type == "C") {
      
      print("Normal Cdf")
      
      lower <- as.numeric(readline(prompt = "What is your lower bound?: "))
      upper <- as.numeric(readline(prompt = "What is your upper bound?: "))
      mean <- as.numeric(readline(prompt = "What is the mean of the distribution?: "))
      sd <- as.numeric(readline(prompt = "What is the standard distribution?: "))
      
      print(pnorm(upper, mean, sd) - pnorm(lower, mean, sd))
      
    } else {
      
      print("Inverse Norm")
      
      area <- as.numeric(readline(prompt = "What is your desired area?: "))
      mean <- as.numeric(readline(prompt = "What is the mean of the distribution?: "))
      sd <- as.numeric(readline(prompt = "What is the standard distribution?: "))
      
      print(qnorm(area, mean, sd))
      
    }
    
  } else if (type == "Z") {
    
    type <- readline(prompt = "[T]ests or [I]nterval?: ")
    
    if (type == "I") {
      
      type <- readline(prompt = writeLines("What type of interval, \n[Z] Interval \n[1] Prop Z interval \n[2] Prop Z interval?: "))
      
      
      if (type == "Z") {
        
        sigma <- as.numeric(readline(prompt = "What is the population standard deviation?: "))
        x_bar <- as.numeric(readline(prompt = "What is x-bar?: "))
        n <- as.numeric(readline(prompt = "What is n?: "))
        c <- as.numeric(readline(prompt = "What is the confidence level?: "))
        
        #sigma <- 21
        #x_bar <- 430
        #n <- 500
        #c <- 0.95
        
        z <- qnorm(c + (1 - c)/2)
        se <- sigma/sqrt(n)
        lower <- x_bar - z * se
        upper <- x_bar + z * se
        
        cat(paste0("Z-Interval \n(", lower, ", ", upper, ") \nx-bar = ", x_bar, " \nn = ", n))
        
        #Graph
        x <- seq(-4,4,length=100)*se + x_bar
        hx <- dnorm(x,x_bar,se)
        
        plot(x, hx, type="n", xlab="Means", ylab="",
             main="Normal Distribution", axes=FALSE)
        
        i <- x >= lower & x <= upper
        lines(x, hx)
        polygon(c(lower,x[i],upper), c(0,hx[i],0), col="red") 
        
        area <- pnorm(upper, x_bar, se) - pnorm(lower, x_bar, se)
        result <- paste("P(",lower,"< mean <",upper,") =",
                        signif(area, digits=3))
        mtext(result,3)
        axis(1, at=seq(x_bar-3*se, x_bar+3*se, se), pos=0)
        
      } else if (type == "1") {
        
        print("1 Prop Z interval")
        
        x <- as.numeric(readline(prompt = "What is x?: "))
        n <- as.numeric(readline(prompt = "What is n?: "))
        c <- as.numeric(readline(prompt = "What is the confidence level?: "))
        
        #x <- 430
        #n <- 500
        #c <- 0.95
        
        p <- x/n
        z <- qnorm(c + (1 - c)/2)
        se <- sqrt(p * (1 - p) / n)
        lower <- p - z * se
        upper <- p + z * se
        
        cat(paste0("1-PropZInt \n(", lower, ", ", upper, ") \np-hat = ", p, " \nn = ", n))
        
        #Graph
        x <- seq(-4,4,length=100)*se + p
        hx <- dnorm(x,p,se)
        
        plot(x, hx, type="n", xlab="Proportions", ylab="",
             main="Normal Distribution", axes=FALSE)
        
        i <- x >= lower & x <= upper
        lines(x, hx)
        polygon(c(lower,x[i],upper), c(0,hx[i],0), col="red") 
        
        area <- pnorm(upper, p, se) - pnorm(lower, p, se)
        result <- paste("P(",lower,"< p <",upper,") =",
                        signif(area, digits=3))
        mtext(result,3)
        axis(1, at=seq(p-3*se, p+3*se, se), pos=0)
        
      } else {
        
        print("2 Prop Z interval")
        
        x1 <- as.numeric(readline(prompt = "What is x1?: "))
        n1 <- as.numeric(readline(prompt = "What is n1?: "))
        x2 <- as.numeric(readline(prompt = "What is x2?: "))
        n2 <- as.numeric(readline(prompt = "What is n2?: "))
        c <- as.numeric(readline(prompt = "What is the confidence level?: "))
        
        #x1 <- 400
        #n1 <- 500
        #x2 <- 490
        #n2 <- 500
        #c <- 0.95
        
        p1 <- x1/n1
        p2 <- x2/n2
        
        z <- qnorm(c + (1 - c)/2)
        se <- sqrt(p1 * (1 - p1) / n1 + p2 *(1 - p2) / n2)
        p <- p1 - p2
        lower <- p - z * se
        upper <- p + z * se
        
        cat(paste0("2-PropZInt \n(", lower, ", ", upper, ") \np1 = ", p1, " \np2 = ", p2, " \nn1 = ", n1, "\nn2 = ", n2))
        
        #Graph
        x <- seq(-4,4,length=100)*se + p
        hx <- dnorm(x,p,se)
        
        plot(x, hx, type="n", xlab="Proportions", ylab="",
             main="Normal Distribution", axes=FALSE)
        
        i <- x >= lower & x <= upper
        lines(x, hx)
        polygon(c(lower,x[i],upper), c(0,hx[i],0), col="red") 
        
        area <- pnorm(upper, p, se) - pnorm(lower, p, se)
        result <- paste("P(",lower,"< p <",upper,") =",
                        signif(area, digits=3))
        mtext(result,3)
        axis(1, at=seq(p-3*se, p+3*se, se), pos=0)
        
      }
      
    } else {
      
      
      ##-----------------------------------------THE Z TEST FUNCTIONS NEED THEIR INPUTS-----------------------      
      
      type <- readline(prompt = writeLines("What type of test, [1] Prop Z test \n[2] Prop Z test?: "))
      if (type == "1") {
        
        print("1 Prop Z Test")
        
        p0 <- as.numeric(readline(prompt = "What is p0?: "))
        x <- as.numeric(readline(prompt = "What is x?: "))
        n <- as.numeric(readline(prompt = "What is n?: "))
        
        #p0 <- 0.7
        #x <- 59
        #n <- 100
        
        p <- x/n
        z <- (p - p0)/sqrt(p0 * (1 - p0) / n)
        pval = 2 * pnorm(-abs(z))
        cat(paste0("1-PropZTest \nmu1 != mu2 \nz = ", z, " \np-value = ", pval, " \np-hat = ", p, " \nn = ", n))
        
      } else {
        
        print("2 Prop Z Test")
        
        prop.test(x = c(490, 400), n = c(500, 500))
        prop.test(x = c(490, 400), n = c(500, 500))$p.value
        
        x1 <- as.numeric(readline(prompt = "What is x1?: "))
        n1 <- as.numeric(readline(prompt = "What is n1?: "))
        x2 <- as.numeric(readline(prompt = "What is x2?: "))
        n2 <- as.numeric(readline(prompt = "What is n2?: "))
        
        #x1 <- 400
        #n1 <- 500
        #x2 <- 490
        #n2 <- 500
        
        p1 <- x1/n1
        p2 <- x2/n2
        p <- (x1 + x2)/(n1 + n2)
        z <- (p1 - p2)/sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
        pval = 2 * pnorm(-abs(z))
        print(pval)
        cat(paste0("1-PropZTest \nmu1 != mu2 \nz = ", z, " \np-value = ", pval, " \np-hat = ", p, " \np-hat 1 = ", p1, " \np-hat 2 = ", p2, 
                   " \np-hat = ", p, " \nn1 = ", n1, "\nn2 = ", n2))
        
      }
      
    }
    
  } else {
    
    ##----------------------------------------THE T TEST FUNCTIONS NEED THEIR INPUTS-------------------------    
    print("T Functions")
    type <- readline(prompt = "[T]ests or [O]ther?: ")
    if (type == "T") {
      type <- readline(prompt = writeLines("\n [1] T test \n[2] 2 Sample T Test \n"))
      if (type == "1") {
        
        print("T Test")
        
        mu <- as.numeric(readline(prompt = "What is mu?: "))
        x <- as.numeric(readline(prompt = "What is x?: "))
        s <- as.numeric(readline(prompt = "What is the sample standard deviation?: "))
        n <- as.numeric(readline(prompt = "What is n?: "))
        
        #mu <- 1.6
        #x <- 1.3
        #s <- 0.09
        #n <- 24
        
        df <- n - 1
        t <- (x - mu) / sqrt(s^2/n)
        p <- 2 * pt(-abs(t),df)
        cat(paste0("1-SampTTest \nmu != ", mu, " \nt = ", t, " \np-value = ", p, "\nx = ", x, "\ns = ", s, " \nn = ", n))
        
      } else {
        
        print("2 Sample T Test")
        
        x1 <- as.numeric(readline(prompt = "What is x1?: "))
        s1 <- as.numeric(readline(prompt = "What is s1?: "))
        n1 <- as.numeric(readline(prompt = "What is n1?: "))
        x2 <- as.numeric(readline(prompt = "What is x2?: "))
        s2 <- as.numeric(readline(prompt = "What is s2?: "))
        n2 <- as.numeric(readline(prompt = "What is n2?: "))
        pooled <- as.logical(readline(prompt = "Is this is pooled sample? (boolean): "))
        
        #x1 <- 0.06816336
        #s1 <- 1.218701
        #n1 <- 100
        #x2 <- -0.07430414
        #s2 <- 1.044537
        #n2 <- 200
        #pooled <- FALSE
        
        if(pooled == FALSE) 
        {
          se <- sqrt(s1^2/n1 + s2^2/n2)
          # welch-satterthwaite df
          df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
        } else {
          # pooled standard deviation, scaled by the sample sizes
          se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
          df <- n1 + n2 - 2
        }
        
        t <- (x1 - x2) / se 
        p <- 2 * pt(-abs(t),df)
        cat(paste0("2-SampTTest \nmu1 != mu2 \nt = ", t, " \np-value = ", p, " \ndf = ", df, " \nx1 = ", x1, " \nx2 = ", x2, 
                   " \ns1 = ", s1, " \ns2 = ", s2, " \nn1 = ", n1, " \nn2 = ", n2))
        
      }
      
    } else {
      
      type <- readline(prompt = writeLines("\n [1] T Interval \n[2] InvT \n [3] Tcdf"))
      
      if (type == "1") {
        
        print("T Interval")
        
        sd <- as.numeric(readline(prompt = "What is the sample standard deviation?: "))
        x_bar <- as.numeric(readline(prompt = "What is x-bar?: "))
        n <- as.numeric(readline(prompt = "What is n?: "))
        c <- as.numeric(readline(prompt = "What is the confidence level?: "))
        
        sd <- 0.45
        x_bar <- 9
        n <- 13
        c <- 0.95
        
        t <- qt(c + (1 - c)/2, n - 1)
        se <- sd/sqrt(n)
        lower <- x_bar - t * se
        upper <- x_bar + t * se
        
        cat(paste0("T-Interval \n(", lower, ", ", upper, ") \nx-bar = ", x_bar, " \nn = ", n))
        
      } else if (type == "2") {
        
        print("Inverse T")
        
        area <- as.numeric(readline(prompt = "What is the desired area?: "))
        df <- as.numeric(readline(prompt = "What are the degrees of freedom?" ))
        
        print(qt(area, df))
        
      } else {
        
        print("TCdf")
        
        lower <- as.numeric(readline(prompt = "What is your lower bound?: "))
        upper <- as.numeric(readline(prompt = "What is your upper bound?: "))
        df <- as.numeric(readline(prompt = "What are the degrees of freedom?" ))
        
        print(pt(upper, df) - pt(lower, df))
        
      }
      
    }
    
  }
  
} else if (decision == "C") {
  
  type <- readline(prompt = "[G]oodness of Fit or [T]est?: ")
  if (type == "G") {
    
    print("Chi Squared Goodness of Fit")
    
    prompt <- "Enter the observed numbers (space-separated list) \n"
    obs <- as.integer(strsplit(readline(prompt), " ")[[1]])
    #obs <- c(4, 13, 7)
    
    prompt <- "Enter the expected numbers (space-separated list) \n"
    exp <- as.integer(strsplit(readline(prompt), " ")[[1]])
    #exp <- c(8, 8, 8)
    exp2 <- c(exp/sum(exp))
    
    res <- chisq.test(obs, p = exp2)
    chi_squared <- chisq.test(obs, p = exp2)$statistic
    p-value <- chisq.test(obs, p = exp2)$p.value
    df <- chisq.test(obs, p = exp2)$parameter
    CNTRB <- (obs - exp)^2/exp
    
    cat(paste0("X2GOF-Test\nX2 = ", chi_squared, "\np-value = ", p-value, "\ndf = ", df, "\nCNTRB = {", paste(CNTRB, collapse = ", "), "}"))
    
  } else {
    
    print("Chi Squared Test")
    
    row <- as.numeric(readline(prompt = "How many rows?: "))
    col <- as.numeric(readline(prompt = "How many columns?: "))
    
    prompt <- "Enter the observed numbers working horizontally (space-separated list) \n"
    obs <- as.integer(strsplit(readline(prompt), " ")[[1]])
    #obs <- c(30, 10, 15, 25, 15, 5)
    newobs <- matrix(obs, nrow = row, ncol = col, byrow=TRUE)
    
    prompt <- "Enter the observed numbers working horizontally (space-separated list) \n"
    exp <- as.integer(strsplit(readline(prompt), " ")[[1]])
    #exp <- c(24, 16, 24, 16, 12, 8)
    newexp <- matrix(exp/sum(exp), nrow = row, ncol = col, byrow=TRUE)
    
    res <- chisq.test(newobs, p = newexp)
    chi_squared <- chisq.test(newobs, p = newexp)$statistic
    p-value <- chisq.test(newobs, p = newexp)$p.value
    df <- chisq.test(newobs, p = newexp)$parameter
    
    cat(paste0("X2-Test\nX2 = ", chi_squared, "\np-value = ", p-value, "\ndf = ", df))
    
  }
  
} else if (decision == "R") {
  
  type <- readline(prompt = "[R]egression or [T]ests?: ")
  
  if (type == "R") {
    
    print("Linear Regression")
    
    prompt <- "Enter the explanatory numbers (space-separated list) \n"
    explanatory <- as.integer(strsplit(readline(prompt), " ")[[1]])
    #explanatory <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
    
    prompt <- "Enter the response numbers (space-separated list) \n"
    response <- as.integer(strsplit(readline(prompt), " ")[[1]])
    #response <- c(52, 36, 71, 21, 57, 79, 63, 55, 31, 70)
    
    model <- lm(response ~ explanatory)
    a <- summary(model)$coefficients["(Intercept)", "Estimate"]
    b <- summary(model)$coefficients["explanatory", "Estimate"]
    cat(paste0("y = a + bx\na = ", a, "\nb = ", b))
    
  } else {
    
    type <- readline(prompt = "Linear Regression T [T]est or T [I]nterval?: ")
    
    if (type == "T") {
      
      print("LinReg T Test")
      
      prompt <- "Enter the explanatory numbers (space-separated list) \n"
      explanatory <- as.integer(strsplit(readline(prompt), " ")[[1]])
      #explanatory <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
      
      prompt <- "Enter the response numbers (space-separated list) \n"
      response <- as.integer(strsplit(readline(prompt), " ")[[1]])
      #response <- c(52, 36, 71, 21, 57, 79, 63, 55, 31, 70)
      
      model <- lm(response ~ explanatory)
      df <- model$df.residual
      a <- summary(model)$coefficients["(Intercept)", "Estimate"]
      b <- summary(model)$coefficients["explanatory", "Estimate"]
      s <- summary(model)$sigma
      t <- summary(model)$coefficients[6]
      p-value <- summary(model)$coefficients[8]
      r_squared <- summary(model)$r.squared
      r <- cor(response, explanatory)
      
      cat(paste0("LinRegTTest \ny = a + bx \nbeta != 0 \nt = ", t, " \np-value = ", p-value, " \ndf = ", df, " \na = ", a, " \nb = ", b, " \ns = ", s, " \nr^2 = ", r_squared, " \nr = ", r))
      
      
    } else {
      
      print("LinReg T Int")
      
      prompt <- "Enter the explanatory numbers (space-separated list) \n"
      explanatory <- as.integer(strsplit(readline(prompt), " ")[[1]])
      #explanatory <- c(2, 47, 4, 75, 35, 77, 49, 14, 24, 21)
      
      prompt <- "Enter the response numbers (space-separated list) \n"
      response <- as.integer(strsplit(readline(prompt), " ")[[1]])
      #response <- c(52, 36, 71, 21, 57, 79, 63, 55, 31, 70)
      
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
      
      cat(paste0("LinRegTInt \ny = a + bx \n(", lower, ", ", upper, ") \nb = ", b, " \ndf = ", df, " \ns = ", s, " \na = ", a, " \nr^2 = ", r_squared, " \nr = ", r))
      
    }
    
  }
  
} else {
  
  type <- readline(prompt = "Would you like [1] Variable or [2] Variable descriptive statistics?: ")
  
  if (type == "1"){
    
    print("1 Variable")
    
#    a <- 0
    L1 <- c()
    
#    my.size <- as.numeric(readline(prompt = "1 Variable, how long is your x list?: "))
#    while (a < my.size){
#      
#      i <- as.numeric(readline(prompt = "Type in a number: "))
#      L1 <- append(L1, i)
#      a <- a + 1
#      
#    }
#
    prompt <- "Enter the numbers for L1 (space-separated list) \n"
    L1 <- as.integer(strsplit(readline(prompt), " ")[[1]])
    n <- length(L1)
    
    result <- c(mean(L1), sum(L1), sum(L1^2), sd(L1), sqrt((n-1)/n) * sd(L1), n, quantile(L1))    
    names(result) <- c("x-bar", "Sum of x", "Sum of x^2", "Sample SE of x", "Pop SE of x", "n", "minX", "Q1", "Med", "Q3", "maxX")
    
    print(result)
    
  } else {
    
#    my.size <- as.numeric(readline(prompt = "2 Variable, how long is your x list?: "))
#    my.size2 <- as.numeric(readline(prompt = "2 Variable, how long is your y list?: "))
#    
#    a <- 0
    L1 <- c()
    
    prompt <- "Enter the numbers for L1 (space-separated list) \n"
    L1 <- as.integer(strsplit(readline(prompt), " ")[[1]])
    
#    while (a < my.size) {
#      i <- as.numeric(readline(prompt = "Type in a number for your X list: "))
#      L1 <- append(L1, i)
#      a <- a + 1
#    }
#    
#    b <- 0
    L2 <- c()
    
    prompt <- "Enter the numbers for L2 (space-separated list) \n"
    L2 <- as.integer(strsplit(readline(prompt), " ")[[1]])
    
#    while (b < my.size2) {
#      i <- as.numeric(readline(prompt = "Input a number for your Y list: "))
#      L2 <- append(L2, i)
#      b <- b + 1
#    }
#    
    n <- length(L1 & L2)
    
    result <- c(mean(L1), sum(L1), sum(L1^2), sd(L1), sqrt((n-1)/n) * sd(L1), n, mean(L2), sum(L2), sum(L2^2), sd(L2), 
                sqrt((n-1)/n) * sd(L2), sum(L1 * L2), min(L1), max(L1), min(L2), max(L2))
    names(result) <- c("x-bar", "Sum of x", "Sum of x^2", "Sample SE of x", "Pop SE of x", "n", "y-bar", "Sum of y", "Sum of y^2", 
                       "Sample SE of y", "Pop SE of y", "Sum of xy", "minX", "maxX", "minY", "maxY")
    
    print(result)
    
  }
}