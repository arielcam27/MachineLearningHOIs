require(ggpubr)
require(sets)
require(growthrates)

get_single_week0 <- function(file)  {
  
  #file = "./data/1-neb0.csv"
  #file = "./data/2-sim0.csv"
  #file = "./data/3-ebo0.csv"
  
  # considering week 0
  dataOut <-read.table(file, sep=",")
  colnames(dataOut) <- c("week", "replicate", "x1")
  
  # check data
  # ggerrorplot(dataOut, x = "week", y = "x1", 
  #             desc_stat = "mean_sd",
  #             error.plot = "errorbar",
  #             add = "mean")
  
  # separate data by replicate
  splitted.data <- multisplit(dataOut, c("replicate"))
  
  # set a priori min/max bounds for parameters
  # logistic
  lower1 <- c(y0 = 28,  mumax = 0.1, K = 100)
  p1     <- c(y0 = 29,  mumax = 0.5,  K = 500)
  upper1 <- c(y0 = 30, mumax = 3,   K = 1000)
  
  # two-step growth
  lower2 <- c(yi = 28,  ya = 0,  kw = 0,   mumax = 0,   K = 100)
  p2     <- c(yi = 29,  ya = 2,  kw = 0.1, mumax = 0.5, K = 500)
  upper2 <- c(yi = 30, ya = 10, kw = 3,  mumax = 10,   K = 1000)
  
  # create dataframe for parameter estimation results
  df_pars <- data.frame("in. week" = 0,
                        "replicate" = 1,
                        "model" = '1', 
                        "y0" = 0, 
                        "r" = as.numeric(0), 
                        "K" = 0, stringsAsFactors = FALSE)
  
  # perform a fit for each replicate
  for (ii in as.set(dataOut$replicate)) {
    dat <- splitted.data[[ii]]
    
    # estimate logistic parameters
    try(
      fit1 <- fit_growthmodel(FUN = grow_logistic, 
                              p = p1, 
                              dat$week, 
                              dat$x1,
                              lower = lower1, 
                              upper = upper1))
    
    # save parameters y0, mumax, K
    df_pars[nrow(df_pars) + 1,] = c(0,
                                    ii,
                                    "logistic", 
                                    coef(fit1)[[1]], 
                                    coef(fit1)[[2]], 
                                    coef(fit1)[[3]]) 
    
    # estimate two-step growth parameters
    try(
      fit2 <- fit_growthmodel(FUN = grow_twostep, 
                              p = p2, 
                              time = dat$week, 
                              y = dat$x1,
                              lower = lower2, 
                              upper = upper2))
    
    # save parameters yi, ya, kw, mumax, K
    df_pars[nrow(df_pars) +1,] = c(0, 
                                   ii,
                                   "twostep", 
                                   coef(fit2)[[1]], 
                                   coef(fit2)[[4]], 
                                   coef(fit2)[[5]]) 
    
    # 3.5.3 from "A Primer in Ecology with R", M Henry H Stevens
    n <- nrow(dat)
    N.change <- dat$x1[-1]/dat$x1[-n]
    interval <- diff(dat$week)
    pgr <- log(N.change)/interval
    Nt <- dat$x1[-n]
    mod1 <- lm(pgr ~ Nt)
    r <- coef(mod1)[[1]]
    K <- -1/coef(mod1)[[2]]
    
    if (K > 0 & K < 2000 & r > 0.1 & r < 3) {
      df_pars[nrow(df_pars) +1,] = c(0,ii,"timeind", NA, r, K) 
    } else {
      df_pars[nrow(df_pars) +1,] = c(0,ii,"timeind", NA, NA, NA) 
    }
  }
  df_pars <- df_pars[-1,]
  return(df_pars)
}

#-------------------------------------
  
get_single_week1 <- function(file)  {
  
  #file = "./data/1-neb.csv"
  #file = "./data/2-sim.csv"
  #file = "./data/3-ebo.csv"
  
  # without considering week 0
  dataOut <-read.table(file,sep=",")
  colnames(dataOut) <- c("week", "replicate", "x1")

  splitted.data <- multisplit(dataOut, c("replicate"))
  
  # set a priori min/max bounds for parameters
  # logistic
  lower1 <- c(y0 = 1,  mumax = 0.1, K = 100)
  p1     <- c(y0 = 2,  mumax = 0.5,  K = 500)
  upper1 <- c(y0 = 30, mumax = 3,   K = 1000)
  
  # two-step growth
  lower2 <- c(yi = 1,  ya = 1,  kw = 0,   mumax = 0.1,   K = 100)
  p2     <- c(yi = 2,  ya = 2,  kw = 0.1, mumax = 0.5, K = 500)
  upper2 <- c(yi = 30, ya = 30, kw = 10,  mumax = 3,   K = 1000)
  
  # create dataframe for parameter estimation results
  df_pars <- data.frame("in. week" = 1,
                        "replicate" = 1,
                        "model" = '1', 
                        "y0" = 0, 
                        "r" = as.numeric(0), 
                        "K" = 0, stringsAsFactors = FALSE)

  for (ii in as.set(dataOut$replicate)) {
    dat <- splitted.data[[ii]]
    dat

    try(
      fit1 <- fit_growthmodel(FUN = grow_logistic, p = p1, dat$week, dat$x1,
                              lower = lower1, upper = upper1))

    # y0, mumax, K
    df_pars[nrow(df_pars) + 1,] = c(1,ii,"logistic", coef(fit1)[[1]], coef(fit1)[[2]], coef(fit1)[[3]])

    try(
      fit2 <- fit_growthmodel(FUN = grow_twostep, p = p2, time = dat$week, y = dat$x1,
                              lower = lower2, upper = upper2))

    # yi, ya, kw, mumax, K
    df_pars[nrow(df_pars) +1,] = c(1,ii,"twostep", coef(fit2)[[1]], coef(fit2)[[4]], coef(fit2)[[5]])

    # 3.5.3 from "A Primer in Ecology with R", M Henry H Stevens
    n <- nrow(dat)
    N.change <- dat$x1[-1]/dat$x1[-n]
    interval <- diff(dat$week)
    pgr <- log(N.change)/interval
    pgr
    Nt <- dat$x1[-n]
    #plot(pgr ~ Nt)
    mod1 <- lm(pgr ~ Nt)
    #summary(mod1)
    r <- coef(mod1)[[1]]
    K <- -1/coef(mod1)[[2]]
    
    if (K > 0 & K < 2000 & r > 0.1 & r < 3) {
      df_pars[nrow(df_pars) +1,] = c(1,ii,"timeind", NA, r, K)
    } else {
      df_pars[nrow(df_pars) +1,] = c(1,ii,"timeind", NA, NA, NA)
    }
  }
  
  df_pars <- df_pars[-1,]
  return(df_pars)
}
