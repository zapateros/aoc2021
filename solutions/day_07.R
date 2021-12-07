input  <- as.numeric(unlist(strsplit(readLines("input/input_day_07.txt"),",")))

# Star 1
steps <- sapply(c(min(input):max(input)), function(x){sum(abs(input - x))})
output <- min(steps)
cat("Day 7 Star 1:", output)

# Star 2
steps <- sapply(c(min(input):max(input)), function(x){
  sum(sapply(input, function(y){sum(1:abs(y - x))}))
  })
output <- min(steps)
cat("Day 7 Star 2:", output)
