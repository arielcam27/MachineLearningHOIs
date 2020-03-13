require(deSolve)
library(magrittr)
library(dplyr)
library(progress)

NoHOIsamplesNoise <- function(globPars, SD) {
  ODE = function(t, y, params) {
    x1 = y[1]
    x2 = y[2]
    x3 = y[3]
    
    r1 = params["r1"]
    r2 = params["r2"]
    r3 = params["r3"]
    
    K1 = params["K1"]
    K2 = params["K2"]
    K3 = params["K3"]
    
    a12 = params["a12"]
    a13 = params["a13"]
    a21 = params["a21"]
    a23 = params["a23"]
    a31 = params["a31"]
    a32 = params["a32"]
    
    dx1 = r1*x1*(1 - x1/K1 + a12*x2/K2 + a13*x3/K3 + b123*x2*x3/(K2*K3))
    dx2 = r2*x2*(1 - x2/K2 + a23*x3/K3 + a21*x1/K1 + b231*x3*x1/(K3*K1))
    dx3 = r3*x3*(1 - x3/K3 + a31*x1/K1 + a32*x2/K2 + b312*x1*x2/(K1*K2))
    
    res = c(dx1, dx2, dx3)
    
    list(res)
  }
  
  r1 = globPars[1]
  K1 = globPars[2]
  
  r2 = globPars[3]
  K2 = globPars[4]
  
  r3 = globPars[5]
  K3 = globPars[6]
  
  a12 = globPars[7]
  a21 = globPars[8]
  
  a13 = globPars[9]
  a31 = globPars[10]
  
  a23 = globPars[11]
  a32 = globPars[12]
  
  b123 = 0
  b231 = 0
  b312 = 0
  
  maxAllowed <- 2000
  
  times = seq(0, 8, by=1/100)
  listpars <- read.table("pars-ICs.txt")
  nrow(listpars)
  
  # x1: 7, x2: 7, x3: 7, no-hoi
  dataOut2 <- data.frame(0, 0, 0, 0, 0, 0, 0, 
                         0, 0, 0, 0, 0, 0, 0, 
                         0, 0, 0, 0, 0, 0, 0, 
                         "no-hoi")
  
  colnames(dataOut2) <- c("x11", "x12", "x13", "x14", "x15", "x16", "x17", 
                          "x21", "x22", "x23", "x24", "x25", "x26", "x27", 
                          "x31", "x32", "x33", "x34", "x35", "x36", "x37", "isHOI")
  
  index <- 1
  pb <- progress_bar$new(total = nrow(listpars))

    
    x10 <- 10
    x20 <- 10
    x30 <- 10
    start = c(x1=x10, x2=x20, x3=x30)
    #print(b312)
    
    params = c(r1=r1, r2=r2, r3=r3, 
               K1=K1, K2=K2, K3=K3, 
               a12=a12, a13=a13, a21=a21, a23=a23, a31=a31, a32=a32,
               b123=b123, b231=b231, b312=b312)
    
    tryCatch(
      {
        # Just to highlight: if you want to use more than one 
        # R expression in the "try" part then you'll have to 
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression 
        # in case the "try" part was completed successfully
        
        out <- ode(y=start, times=times, func=ODE, parms=params)
        out <- as.data.frame(out)
        out <- round(out, 4)
        
        
        
        #plot(x=out$time, y=out$x1, ylab="Number", xlab="Time", type="l", ylim = c(min(out$x1, out$x2, out$x3), max(out$x1, out$x2, out$x3)))
        #lines(x=out$time, y=out$x2, col="red")
        #lines(x=out$time, y=out$x3, col="green")
        
        out2 <- out %>% filter(out$time %in% c(1, 2, 3, 4, 5, 6, 7))
      
      },
      error=function(cond) {
        #message("ERROR: Possibly NaN")
      }
    )

for (bpars in 1:nrow(listpars)) {
  pb$tick()
  dataOut2[index, 1:7] <- rlnorm(7, mean=log(out2$x1), sd=SD)
  dataOut2[index, 8:14] <- rlnorm(7, mean=log(out2$x2), sd=SD)
  dataOut2[index, 15:21] <- rlnorm(7, mean=log(out2$x3), sd=SD)
  dataOut2[index, 22] <- "no-hoi"
  index <- index + 1
  #write.table(out2, paste("./samplesIC/mydataIC", bpars, ".csv", sep=""), row.names = FALSE, sep = "\t")
  
  #rowSums(out2[,2:4] > MaxAllowed)
  # The return value of `readLines()` is the actual value 
  # that will be returned in case there is no condition 
  # (e.g. warning or error). 
  # You don't need to state the return value via `return()` as code 
  # in the "try" part is not wrapped insided a function (unlike that
  # for the condition handlers for warnings and error below)
  #return(out)
        
  }
  return(dataOut2)
}