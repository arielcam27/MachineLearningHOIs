require(ggpubr)
require(growthrates)
# x1: 7, x2: 7, x3: 7, no-hoi

#help(read.table)
dataOut2 <-read.table("droso.csv",sep=",")
colnames(dataOut2) <- c("week", "replicate", "x1", "x2", "x3")

dataOut2
plot(dataOut2$week,dataOut2$x3)
#lines(1:7,dataOut2[2,1:7])
#lines(1:7,dataOut2[2,15:21])

ggerrorplot(dataOut2, x = "week", y = "x3", 
            desc_stat = "mean_sd",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)

#data(bactgrowth)
#tail(bactgrowth)
#library(lattice)
#data(bactgrowth)
#xyplot(value ~ time|strain+as.factor(conc), data = bactgrowth,
#       groups = replicate, pch = 16, cex = 0.1)

#splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
#dat <- splitted.data[[4]]
#head(dat)
#plot(dat$time, dat$value)
#fit <- fit_easylinear(dat$time, dat$value)
#summary(fit)

# par(mfrow = c(1, 2))
# plot(fit, log = "y")
# plot(fit)
# fitx <- fit_easylinear(dat$time, dat$value, h = 8, quota = 0.95)
# plot(fitx)
# lines(fitx, pch = "+", col = "blue")

splitted.data <- multisplit(dataOut2, c("replicate"))
dat <- splitted.data[[2]]
dat
fit <- fit_easylinear(dat$week, dat$x2)
summary(fit)
plot(fit)
fitx <- fit_easylinear(dat$week, dat$x2, h = 6, quota=0.9)
plot(fitx)
fit

p     <- c(y0 = 5, mumax = 0.5, K = 500)
lower <- c(y0 = 1, mumax = 0.01,   K = 100)
upper <- c(y0 = 10, mumax = 10,   K = 1000)

fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, dat$week, dat$x2,
                        lower = lower, upper = upper)

plot(fit1)
coef(fit1)

lower <- c(yi = 1,  ya = 0,  kw = 0,   mumax = 0,   K = 400)
p     <- c(yi = 5,  ya = 2,  kw = 0.1, mumax = 0.2, K = 500)
upper <- c(yi = 10, ya = 10, kw = 10,  mumax = 5,   K = 1000)

fit2 <- fit_growthmodel(FUN = grow_twostep, p = p, time = dat$week, y = dat$x2,
                        lower = lower, upper = upper)
plot(fit2)
coef(fit2)


