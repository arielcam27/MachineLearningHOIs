library("FME")
require(ggpubr)
require(sets)
require(growthrates)

get_pairwise_week0 <- function(file, pars_x1, pars_x2)  {
  # file = "./data/4-neb_sim0.csv"
  # file = "./data/5-neb_ebo0.csv"
  # file = "./data/6-sim_ebo0.csv"
  
  # pars_xj = (rj, Kj)
  
  dataOut <-read.table(file,sep=",")
  colnames(dataOut) <- c("time", "replicate", "x1", "x2")
  
  splitted.data <- multisplit(dataOut, c("replicate"))
  
  # initial "guess"
  parms <- c(a12 = 0.1, a21 = 0.1)
  
  # numeric solution
  model <- function(t, x, parms) {
    with(as.list(parms), {
      r1 = pars_x1[1]
      K1 = pars_x1[2]
      r2 = pars_x2[1]
      K2 = pars_x2[2]
      
      # c is the concentration of species
      
      # derivatives dc/dt are computed below
      r=rep(0,length(c))
      r[1] = r1*x[1]*(1 - x[1]/K1 + a12*x[2]/K2)
      r[2] = r2*x[2]*(1 - x[2]/K2 + a21*x[1]/K1)
      return(list(r))}
    )
  }
  
  # model cost,
  ModelCost2 <- function(P) {
    out <- ode(y = c(x1=15, x2=15), 
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
  
  return(df_pars)
}