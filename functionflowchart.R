decision <- readline(prompt=writeLines("What type of processes would you like to do today? \nType \n[B] for binomials \n[G] for geometric sequences \n[S] for descriptive statistics \n[N] for normal distributions or z distributions \n[C] for Chi Squared functions \n[R] for Regressions: "))

if (decision == "B"){
  type <- readline(prompt="Binomial: [P]df or [C]df: ")
  if (type == "P"){
    print("binomPdf")
    trials <- readline(prompt = "How many trials?: ")
    p <- readline(prompt = "What is the probability of success?: ")
    x_value <- readline(prompt = "What is the desired x?: ")
  } else {
    print("binomCdf")
    trials <- readline(prompt = "How many trials?: ")
    p <- readline(prompt = "What is the probability of success?: ")
    x_value <- readline(prompt = "What is the desired x?: ")
  }
} else if (decision == "G"){
  type <- readline(prompt="Geometric:[P]df or [C]df: ")
  if (type == "P"){
    print("geometPdf")
    p <- readline(prompt = "What is the probability of success?: ")
    x_value <- readline(prompt = "What is the desired x?: ")
  } else {
    print("geometCdf")
    x_value <- readline(prompt = "What is the desired x?: ")
    p <- readline(prompt = "What is the probability of success?: ")
  }
} else if (decision == "N"){
  type <- readline(prompt = "[N]ormal, [Z] functions or [T] functions?: ")
  if (type == "N"){
    type <- readline(prompt = "[C]df or [I]nverse Norm?")
    if (type == "C") {
      print("Normal Cdf")
      lower <- readline(prompt = "What is your lower bound?: ")
      upper <- readline(prompt = "What is your upper bound?: ")
      mean <- readline(prompt = "What is the mean of the distribution?: ")
      sd <- readline(prompt = "What is the standard distribution?: ")
    } else {
      print("Inverse Norm")
      area <- readline(prompt = "What is your desired area?: ")
      mean <- readline(prompt = "What is the mean of the distribution?: ")
      sd <- readline(prompt = "What is the standard distribution?: ")
    }
  } else if (type == "Z") {
    type <- readline(prompt = "[T]ests or [I]nterval?: ")
    if (type == "I") {
      type <- readline(prompt = writelines("What type of interval, [1] Prop Z interval \n[2] Prop Z interval?: "))
      if (type == "1") {
        print("1 Prop Z interval")
        se <- readline(prompt = "What is the standard error?: ")
        x-bar <- readline(prompt = "What is x-bar?: ")
        
      } else {
        print("2 Prop Z interval")
        x1 <- readline(prompt = "What is x1?: ")
        n1 <- readline(prompt = "What is n1?: ")
        x2 <- readline(prompt = "What is x2?: ")
        n2 <- readline(prompt = "What is n2?: ")
        c <- readline(prompt = "What is the confidence level?: ")
      }
    } else {
      
      
      ##-----------------------------------------THE Z TEST FUNCTIONS NEED THEIR INPUTS-----------------------      
      
      type <- readline(prompt = writeLines("What type of test, [1] Prop Z test \n[2] Prop Z test?: "))
      if (type == "1") {
        print("1 Prop Z Test")
      } else {
        print("2 Prop Z Test")
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
      } else {
        print("2 Sample T Test")
      }
    } else {
      type <- readline(prompt = writeLines("\n [1] T Interval \n[2] InvT \n [3] Tcdf"))
      if (type == "1") {
        print("T Interval")
      } else if (type == "2") {
        print("InvT")
        area <- readline(prompt = "What is the desired area?: ")
        df <- readline(prompt = "What are the degrees of freedom?" )
      } else {
        print("TCdf")
        lower <- readline(prompt = "What is your lower bound?: ")
        upper <- readline(prompt = "What is your upper bound?: ")
        df <- readline(prompt = "What are the degrees of freedom?" )
      }
    }
  }
} else if (decision == "C") {
  
  ##-----------------------------------CHI SQUARED FUNCTIONS NEED INPUTS----------------------------------
  
  type <- readline(prompt = "[G]oodness of Fit or [T]est?: ")
  if (type == "G") {
    print("Chi Squared Goodness of Fit")
  } else {
    print("Chi Squared Test")
  }
} else if (decision == "R") {
  type <- readline(prompt = "[R]egression or [T]ests?: ")
  if (type == "R") {
    print("Linear Regression")
  } else {
    type <- readline(prompt = "Linear Regression T [T]est or T [I]?: ")
    if (type == "T") {
      print("LinReg T Test")
    } else {
      print("LinReg T Int")
    }
  }
}else {
  type <- readline(prompt = "Would you like [1] Variable or [2] Variable descriptive statistics?: ")
  if (type == "1"){
    print("1 Variable")
    my.size <- readline(prompt="1 Variable, how long is your x list?: ")
    a <- 0
    my.xlist <- c()
    while (a < my.size){
      i <- readline(prompt="Type in a number: ")
      my.xlist <- append(my.xlist, i)
      a <- a+1
    }
  } else {
    my.size <- readline(prompt="2 Variable, how long is your x list?: ")
    my.size2 <- readline(prompt="2 Variable, how long is your y list?: ")
    a <- 0
    my.xlist <- c()
    while (a < my.size){
      i <- readline(prompt="Type in a number: ")
      my.xlist <- append(my.xlist, i)
      a <- a+1
    }
    b <- 0
    my.ylist <- c()
    while (b < my.size2){
      i <- readline(prompt="Input a number for your Y list: ")
      my.ylist <- append(my.ylist, i)
      b <- b+1
    }
  }
}