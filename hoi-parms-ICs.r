require(pracma)

NoHOIgenerate <- function(lowerIC, upperIC, number) {
  df <- data.frame(1, 2, 3)
  names(df) <- c("10", "20", "30")
  
  i <- 1
for (j1 in linspace(lowerIC, upperIC, number)) {
  for (j2 in linspace(lowerIC, upperIC, number)) {
    for (j3 in linspace(lowerIC, upperIC, number)) {
      df[i, ] <- c(j1, j2, j3)
      i <- i+1
    }
  }
}

write.table(df, file="./pars-ICs.txt")
}
