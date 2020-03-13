require(pracma)


HOIgenerate <- function(lower, upper, number) {
df <- data.frame(1, 2, 3)
names(df) <- c("123", "231", "312")

i <- 1

for (j1 in linspace(lower, upper, number)) {
  for (j2 in linspace(lower, upper, number)) {
    for (j3 in linspace(lower, upper, number)) {
      if (j1 != 0 | j2 != 0 | j3 != 0) {
        df[i, ] <- c(j1, j2, j3)
        i <- i+1
      }
    }
  }
}

write.table(df, file="./pars.txt")
}