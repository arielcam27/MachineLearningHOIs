require(deSolve)
library(magrittr)
library(dplyr)
library(progress)

NONHOIsamples <- function(number, parSingle, parPair, parSingle_sd, parPair_sd) {
ODE = function(t, y, params) {
  x1 = y[1]
  x2 = y[2]
  x3 = y[3]
  
  dx1 = r1*x1*(1 - x1/K1 + a12*x2/K2 + a13*x3/K3)
  dx2 = r2*x2*(1 - x2/K2 + a23*x3/K3 + a21*x1/K1)
  dx3 = r3*x3*(1 - x3/K3 + a31*x1/K1 + a32*x2/K2)
  
  res = c(dx1, dx2, dx3)
  
  list(res)
}

times = seq(0, 8, by=1/100)
start = c(x1=10, x2=10, x3=10)

# x1: 7, x2: 7, x3: 7, no-hoi
dataOut2 <- data.frame(0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                      "non-hoi")

colnames(dataOut2) <- c("x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", 
                       "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", 
                       "x30", "x31", "x32", "x33", "x34", "x35", "x36", "x37", 
                       "isHOI")

index <- 1

r1 = parSingle[1]
K1 = parSingle[2]

r2 = parSingle[3]
K2 = parSingle[4]

r3 = parSingle[5]
K3 = parSingle[6]

a12_mean = parPair[1]
a21_mean = parPair[2]
a12_sd = parPair_sd[1]
a21_sd = parPair_sd[2]

a13_mean = parPair[3]
a31_mean = parPair[4]
a13_sd = parPair_sd[3]
a31_sd = parPair_sd[4]

a23_mean = parPair[5]
a32_mean = parPair[6]
a23_sd = parPair_sd[5]
a32_sd = parPair_sd[6]

for (bpars in 1:number) {
  a12 = rnorm(1, mean=a12_mean, sd=a12_sd)
  a21 = rnorm(1, mean=a21_mean, sd=a21_sd)
  
  a13 = rnorm(1, mean=a13_mean, sd=a13_sd)
  a31 = rnorm(1, mean=a31_mean, sd=a31_sd)
  
  a23 = rnorm(1, mean=a23_mean, sd=a23_sd)
  a32 = rnorm(1, mean=a32_mean, sd=a32_sd)
  
  params = c(r1=r1, r2=r2, r3=r3, 
             K1=K1, K2=K2, K3=K3, 
             a12=a12, a13=a13, a21=a21, a23=a23, a31=a31, a32=a32)
  
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
      
      tail(round(out, 3))
      
      #plot(x=out$time, y=out$x1, ylab="Number", xlab="Time", type="l", ylim = c(min(out$x1, out$x2, out$x3), max(out$x1, out$x2, out$x3)))
      #lines(x=out$time, y=out$x2, col="red")
      #lines(x=out$time, y=out$x3, col="green")
      
      out2 <- out %>% filter(out$time %in% c(0, 1, 2, 3, 4, 5, 6, 7))
      dataOut2[index, 1:8] <- out2$x1
      dataOut2[index, 9:16] <- out2$x2
      dataOut2[index, 17:24] <- out2$x3
      dataOut2[index, 25] <- "non-hoi"
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
    },
    error=function(cond) {
      #message("ERROR: Possibly NaN")
    }
  )    
}
return(dataOut2)
}