getSampleHOI <- function(ii) {
  dataSample <-read.table("./data/7-neb_sim_ebo.csv",sep=",")
  colnames(dataSample) <- c("week", "replicate", "x1", "x2", "x3")
  splitted.data <- multisplit(dataSample, c("replicate"))
  dataSample1 <- splitted.data[[ii]]
  dataSample2 <- data.frame(0, 0, 0, 0, 0, 0, 0, 
                            0, 0, 0, 0, 0, 0, 0, 
                            0, 0, 0, 0, 0, 0, 0)
  colnames(dataSample2) <- c("x11", "x12", "x13", "x14", "x15", "x16", "x17", 
                             "x21", "x22", "x23", "x24", "x25", "x26", "x27", 
                             "x31", "x32", "x33", "x34", "x35", "x36", "x37")
  dataSample2[1, 1:7] <- dataSample1$x1
  dataSample2[1, 8:14] <- dataSample1$x2
  dataSample2[1, 15:21] <- dataSample1$x3
  return(dataSample2)
}