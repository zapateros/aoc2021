input <- readLines("input/input_day_03.txt")
n_col <- nchar(input[1])
mat   <- data.frame(matrix(as.numeric(unlist(strsplit(input, ""))), byrow = TRUE, ncol = n_col))
bits  <- 2^((n_col - 1):0)

# Star 1
mostCommon <- round(colMeans(mat))
gamma      <- sum(bits * mostCommon)
epsilon    <- sum(bits) - gamma
output     <- gamma * epsilon
cat("Day 3 Star 1:" ,output)

# Star 2
oxygen <- co2 <- 1:length(input)
for(i in 1:n_col){
  oxygen <- oxygen[mat[oxygen, i] == floor(0.5 + mean(mat[oxygen, i]))]
  if(length(co2) != 1){
    co2    <- co2[mat[co2, i] == (1 - floor(0.5 + mean(mat[co2, i])))]
  }
}
output <- sum(mat[oxygen,] * bits) * sum(mat[co2,] * bits) 
cat("Day 3 Star 2:" ,output)
