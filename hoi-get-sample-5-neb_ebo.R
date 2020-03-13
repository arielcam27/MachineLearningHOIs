library("FME")
require(ggpubr)
require(sets)
require(growthrates)

get_5_neb_ebo <- function(globPars)  {
  # globPars - r1, K1, r2, K2
  # considering time 0
  dataOut <-read.table("./data/5-neb_ebo0.csv",sep=",")
  
  colnames(dataOut) <- c("time", "replicate", "x1", "x2")
  
  #dataOut
  
  splitted.data <- multisplit(dataOut, c("replicate"))
  
  ggerrorplot(dataOut, x = "time", y = "x1", 
              desc_stat = "mean_sd",
              error.plot = "errorbar",
              add = "mean")
  
  ggerrorplot(dataOut, x = "time", y = "x2", 
              desc_stat = "mean_sd",
              error.plot = "errorbar",
              add = "mean")
  
  ## initial "guess"
  parms <- c(a12 = 0.1, a21 = 0.1)
  
  ## numeric solution
  model <- function(t, x, parms) {
    with(as.list(parms), {
      r1 = globPars[1]
      K1 = globPars[2]
      r2 = globPars[3]
      K2 = globPars[4]
      
      # c is the concentration of species
      
      # derivatives dc/dt are computed below
      r=rep(0,length(c))
      r[1] = r1*x[1]*(1 - x[1]/K1 + a12*x[2]/K2)
      r[2] = r2*x[2]*(1 - x[2]/K2 + a21*x[1]/K1)
      return(list(r))})}
  
  ## model cost,
  ModelCost2 <- function(P) {
    out <- ode(y = c(x1=15,x2=15), 
               func = model, 
               parms = P, 
               times = 0:7)
    return(modCost(out, Data))  # object of class modCost
  }
  
  df_pars <- data.frame("in. week" = 0,
                        "replicate" = 1,
                        "a12" = 0, 
                        "a21" = 0, 
                        stringsAsFactors = FALSE)
  df_pars <- df_pars[-1,]
  for (ii in 1:3) {
    Data <- splitted.data[[ii]]
    Data <- Data[,-2]
    Fit <- modFit(f = ModelCost2, 
                  p = parms, 
                  lower = c(-10,-10),
                  upper = c(10, 10))
    
    df_pars[nrow(df_pars) +1,] = c(0,ii, Fit[[1]][[1]], Fit[[1]][[2]]) 
  }
  
  
  
  
  
  # considering time 1
  dataOut <-read.table("./data/5-neb_ebo.csv",sep=",")
  
  colnames(dataOut) <- c("time", "replicate", "x1", "x2")
  
  splitted.data <- multisplit(dataOut, c("replicate"))
  
  ggerrorplot(dataOut, x = "time", y = "x1", 
              desc_stat = "mean_sd",
              error.plot = "errorbar",
              add = "mean")
  
  ggerrorplot(dataOut, x = "time", y = "x2", 
              desc_stat = "mean_sd",
              error.plot = "errorbar",
              add = "mean")
  
  for (ii in 1:3) {
    Data <- splitted.data[[ii]]
    Data <- Data[,-2]
    Fit <- modFit(f = ModelCost2, 
                  p = parms, 
                  lower = c(-10,-10),
                  upper = c(10, 10))
    
    df_pars[nrow(df_pars) +1,] = c(1,ii, Fit[[1]][[1]], Fit[[1]][[2]]) 
  }
  
  
  return(df_pars)
  # summary(as.numeric(df_pars$a12))
  # summary(as.numeric(df_pars$a21))
  
  
  
  
  
  
  
  # 
  # out <- ode(y = c(x1=15,x2=15), func = model, parms = Fit$par,
  #           times = times)
  # out <- as.data.frame(out)
  # plot(Data$x2)
  # lines(out$x2, col = "red", lty = 2)
  # legend("right", c("data", "original", "fitted analytical", "fitted numerical"),
  #       lty = c(NA, 1, 1, 2), lwd = c(NA, 2, 2, 1),
  #       col = c("red", "green", "blue", "red"), pch = c(16, NA, NA, NA))
  # summary(Fit)
}