require(deSolve)
library(magrittr)
library(dplyr)
library(progress)

HOIsamples <- function(number, parSingle, parPair, b_min, b_max) {
  
# define model
ODE = function(t, y, params) {
  x1 = y[1]
  x2 = y[2]
  x3 = y[3]

  dx1 = r1*x1*(1 - x1/K1 + a12*x2/K2 + a13*x3/K3 + b123*x2*x3/(K2*K3))
  dx2 = r2*x2*(1 - x2/K2 + a23*x3/K3 + a21*x1/K1 + b231*x3*x1/(K3*K1))
  dx3 = r3*x3*(1 - x3/K3 + a31*x1/K1 + a32*x2/K2 + b312*x1*x2/(K1*K2))
  
  res = c(dx1, dx2, dx3)
  
  list(res)
}

times = seq(0, 8, by=1/100)
start = c(x1=10, x2=10, x3=10)

# x1: 7, x2: 7, x3: 7, hoi
dataOut <- data.frame(0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      "hoi")

colnames(dataOut) <- c("x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", 
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

a12 = parPair[1]
a21 = parPair[2]

a13 = parPair[3]
a31 = parPair[4]

a23 = parPair[5]
a32 = parPair[6]

while (index < number) {
  if (index %% 1000 == 0) { cat("Progress:", index) }
  
  j1 <- runif(1, min=b_min, max=b_max)
  j2 <- runif(1, min=b_min, max=b_max)
  j3 <- runif(1, min=b_min, max=b_max)
  
  flag1 = abs(j1) > 0.00001
  flag2 = abs(j2) > 0.00001
  flag3 = abs(j3) > 0.00001
  
  if (flag1 | flag2 | flag3) {
    b123 <- j1
    b231 <- j2
    b312 <- j3
    
    params = c(r1=r1, r2=r2, r3=r3, 
               K1=K1, K2=K2, K3=K3, 
               a12=a12, a13=a13, a21=a21, a23=a23, a31=a31, a32=a32,
               b123=b123, b231=b231, b312=b312)
    
    tryCatch(
      {
        out <- ode(y=start, times=times, func=ODE, parms=params, atol = 1e-3)
        out <- as.data.frame(out)
        out <- round(out, 4)
        
        out2 <- out %>% filter(out$time %in% c(0, 1, 2, 3, 4, 5, 6, 7))
        
        dataOut[index, 1:8]   <- out2$x1
        dataOut[index, 9:16]  <- out2$x2
        dataOut[index, 17:24] <- out2$x3
        dataOut[index, 25]    <- "hoi"
        
        index <- index + 1
      },
      error=function(cond) {
        #message("ERROR: Possibly NaN")
      }
    )
  

  }
}
return(dataOut)
}