# Day 1 ----
input <- as.numeric(readLines("input/input_day_01.txt"))
output <- sum(diff(input) > 0)
cat("Day 1 Star 1: ", output)

# Star 2
# (x2 + x3 + x4) - (x1 + x2 + x3) = x4 - x1
output <- sum((input[-c(1:3)] > head(input, -3)))
cat("Day 1 Star 2: ", output)
