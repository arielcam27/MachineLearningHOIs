require(ggpubr)
require(sets)
require(growthrates)

get_2_sim <- function()  {
  # considering week 0
  dataOut <-read.table("./data/2-sim0.csv",sep=",")
colnames(dataOut) <- c("week", "replicate", "x1")

ggerrorplot(dataOut, x = "week", y = "x1", 
            desc_stat = "mean_sd",
            error.plot = "errorbar",
            add = "mean")

splitted.data <- multisplit(dataOut, c("replicate"))

lower1 <- c(y0 = 2,  mumax = 0.01, K = 100)
p1     <- c(y0 = 5,  mumax = 0.5,  K = 500)
upper1 <- c(y0 = 30, mumax = 10,   K = 2000)

lower2 <- c(yi = 2,  ya = 0,  kw = 0,   mumax = 0,   K = 100)
p2     <- c(yi = 5,  ya = 2,  kw = 0.1, mumax = 0.5, K = 500)
upper2 <- c(yi = 30, ya = 10, kw = 10,  mumax = 10,   K = 2000)

df_pars <- data.frame("in. week" = 0,
                      "replicate" = 1,
                      "model" = '1', 
                      "y0" = 0, 
                      "r" = 0, 
                      "K" = 0, 
                      stringsAsFactors = FALSE)

for (ii in as.set(dataOut$replicate)) {
  dat <- splitted.data[[ii]]
  dat
  
  try(
    fit1 <- fit_growthmodel(FUN = grow_logistic, p = p1, dat$week, dat$x1,
                            lower = lower1, upper = upper1))
  
  # y0, mumax, K
  df_pars[nrow(df_pars) + 1,] = c(0,ii,"logistic", coef(fit1)[[1]], coef(fit1)[[2]], coef(fit1)[[3]]) 
  
  try(
    fit2 <- fit_growthmodel(FUN = grow_twostep, p = p2, time = dat$week, y = dat$x1,
                            lower = lower2, upper = upper2))
  
  # yi, ya, kw, mumax, K
  df_pars[nrow(df_pars) +1,] = c(0,ii,"twostep", coef(fit2)[[1]], coef(fit2)[[4]], coef(fit2)[[5]]) 
  
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
  if (K > 0 & K < 2000) {
    df_pars[nrow(df_pars) +1,] = c(0,ii,"timeind", NA, r, K) 
  } else {
    df_pars[nrow(df_pars) +1,] = c(0,ii,"timeind", NA, NA, NA) 
  }
}

# without considering week 0
dataOut <-read.table("./data/2-sim.csv",sep=",")
colnames(dataOut) <- c("week", "replicate", "x1")

ggerrorplot(dataOut, x = "week", y = "x1", 
            desc_stat = "mean_sd",
            error.plot = "errorbar",
            add = "mean")

splitted.data <- multisplit(dataOut, c("replicate"))

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
  if (K > 0 & K < 2000) {
    df_pars[nrow(df_pars) +1,] = c(0,ii,"timeind", NA, r, K) 
  } else {
    df_pars[nrow(df_pars) +1,] = c(0,ii,"timeind", NA, NA, NA) 
  }
}

df_pars <- df_pars[-1,]
return(df_pars)
}
