source("hoi-get-sample-1-neb.r")
source("hoi-get-sample-2-sim.r")
source("hoi-get-sample-3-ebo.r")
source("hoi-STEP1.r")
source("hoi-get-sample-4-neb_sim.R")
source("hoi-get-sample-5-neb_ebo.R")
source("hoi-get-sample-6-sim_ebo.R")
source("hoi-STEP2.r")
source("hoi-get-sample-7-neb_sim_ebo.R")
source("hoi-parms.r")
source("hoi-parms-ICs.r")
source("hoi-generate.r")
source("hoi-generate-ICs.r")
source("hoi-generate-noise.r")
source("hoi-generate-noise2.r")
source("machine-hoi2.r")
library(progress)

message("Estimating r, K from single species data...")
df_pars1 <- get_1_neb()
df_pars2 <- get_2_sim()
df_pars3 <- get_3_ebo()

globPars1 <- get_rK_single(df_pars1, df_pars2, FUN=mean)
globPars2 <- get_rK_single(df_pars1, df_pars3, FUN=mean)
globPars3 <- get_rK_single(df_pars2, df_pars3, FUN=mean)

message("Estimating aij from two-species data...")
df_pars4 <- get_4_neb_sim(globPars1)
df_pars5 <- get_5_neb_ebo(globPars2)
df_pars6 <- get_6_sim_ebo(globPars3)

globPars4 <- get_aij_double(df_pars4, df_pars5, df_pars6, FUN=mean)
globPars <-c(globPars1[1], globPars1[2], 
             globPars2[1], globPars2[2],
             globPars3[1], globPars3[2], 
             globPars4)

message("Generating HOI samples...")
HOIgenerate(-10, 10, 15)
df_hoi <- HOIsamples(globPars)
df_hoi <- na.omit(df_hoi)

message("Generating NO-HOI samples with different ICs...")
root <- as.integer(nrow(df_hoi)^(1/3))
NoHOIgenerate(1, 30, root)
df_NOhoi <- NoHOIsamples(globPars)
df_NOhoi <- na.omit(df_NOhoi)

message("Generating NO-HOI samples with log-normal noise...")
SD = 2.0
df_NOhoiNoise1 <- NoHOIsamplesNoise(globPars, SD)
df_NOhoiNoise1 <- na.omit(df_NOhoiNoise1)

df_NOhoiNoise2 <- NoHOIsamplesNoise2(globPars, SD)
df_NOhoiNoise2 <- na.omit(df_NOhoiNoise2)

message("Predicting HOI for sample...")
sample1 <- getSampleHOI(1)
sample2 <- getSampleHOI(2)
sample3 <- getSampleHOI(3)

list1  <- machineHOI(df_hoi, df_NOhoi, sample1, sample2, sample3)
list2  <- machineHOI(df_hoi, df_NOhoiNoise1, sample1, sample2, sample3)
list3  <- machineHOI(df_hoi, df_NOhoiNoise2, sample1, sample2, sample3)